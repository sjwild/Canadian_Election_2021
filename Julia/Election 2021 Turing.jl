using Plots, StatsPlots
using RCall
using DataFrames, CSV
using Turing, ReverseDiff, Memoization
using StatsFuns
using Dates
using JLD
using LinearAlgebra
using Measures
using Random 
using PlotlyBase



# Set some global variables for Plots
updated_date = "Sept. 19, 2021"
day_title = "September 20, 2021"
update_date = Date(2021, 09, 20)
value_date = Date(2021, 09, 20)
dateformat = DateFormat("y-m-d")



# custom functions
function clean_mode(x::Vector{String})

    xmode = Vector{String}(undef, length(x))

    for i in 1:length(x)
  
        if contains(x[i], "online/telephone")
            xmode[i] = "Online - telephone"
        elseif contains(x[i], "telephone/IVR")
            xmode[i] = "telephone - IVR"
        elseif contains(x[i], "IVR") == true  & 
            contains(x[i], "telephone - IVR") == false
            xmode[i] = "IVR"
        elseif contains(x[i], "online") == true &
            contains(x[i], "Online - telephone") == false
            xmode[i] = "online"
        elseif contains(x[i], "telephone") == true &
            contains(x[i], "telephone - IVR") == false &
            contains(x[i], "Online - telephone") == false
            xmode[i] = "telephone"
            
        end

    end
    
  return xmode
  
end


function clean_samplesize(x::Vector)

    x = replace.(x, " (1/3)" => "")
    x = replace.(x, " (2/3)" => "")
    x = replace.(x, " (3/3)" => "")
    x = replace.(x, " (1/2)" => "")
    x = replace.(x, " (2/2)" => "")
    x = replace.(x, " (1/4)" => "")
    x = replace.(x, "," => "")
    x = parse.(Int, x)

    return x

end




function convert_numeric(x::DataFrame, var::Vector)

    for i in 1:length(var)
        x[:, var[i]] = parse.(Float64, x[:, var[i]])
    end

end



function calc_moe(x, ss)
    return  sqrt(x * (1-x) / ss)

end


function extract_params(chn::Chains, param::String)

    tmp = chn |> DataFrame
    tmp = tmp[:, startswith.(names(tmp), param)]
    ll = [quantile(tmp[:,i], 0.025) for i in 1:size(tmp, 2)]
    m = [quantile(tmp[:,i], 0.5) for i in 1:size(tmp, 2)]
    uu = [quantile(tmp[:,i], 0.975) for i in 1:size(tmp, 2)]

    return ll, m, uu
    
end



function get_value(x)

    out = Matrix{Float64}(undef, 6, 3)

    out[:,1] = ??_m[xi_days .== x,:]
    out[:,2] = ??_ll[xi_days .== x,:]
    out[:,3] = ??_uu[xi_days .== x,:]

    return out
 
end



function get_seats()

    out = Matrix{Float64}(undef, 6, 3)

    out[:,1] = [quantile(num_seats[:, i], .5) for i in 1:6]
    out[:,2] = [quantile(num_seats[:, i], 0.025) for i in 1:6]
    out[:,3] = [quantile(num_seats[:, i], 0.975) for i in 1:6]

    return out
 
end


function clean_results(X::Vector, outcome_vars::Vector, p::Vector, election::Vector)
    outcomes = CSV.read(X[1], DataFrame; normalizenames = true)
    outcomes.RidingNumber = outcomes.Electoral_District_Number_Num??ro_de_circonscription
    outcomes.Elected = outcomes.Elected_Candidate_Indicator_Indicateur_du_candidat_??lu
    outcomes.Party = outcomes.Political_Affiliation_Name_English_Appartenance_politique_Anglais
    outcomes.Incumbent = outcomes.Incumbent_Indicator_Indicateur_Candidat_sortant
    outcomes.VoteCount = outcomes.Candidate_Poll_Votes_Count_Votes_du_candidat_pour_le_bureau
    outcomes = outcomes[:, outcomes_vars]

    for i ??? 2:length(X)
        tmp = CSV.read(X[i], DataFrame; normalizenames = true)
        tmp.RidingNumber = tmp.Electoral_District_Number_Num??ro_de_circonscription
        tmp.Elected = tmp.Elected_Candidate_Indicator_Indicateur_du_candidat_??lu
        tmp.Party = tmp.Political_Affiliation_Name_English_Appartenance_politique_Anglais
        tmp.Incumbent = tmp.Incumbent_Indicator_Indicateur_Candidat_sortant
        tmp.VoteCount = tmp.Candidate_Poll_Votes_Count_Votes_du_candidat_pour_le_bureau
        tmp = tmp[:, outcomes_vars]

        append!(outcomes, tmp)
    end


    outcomes.Incumbent = ifelse.(outcomes.Incumbent .== "Y", 1, 0)
    outcomes.Incumbent = ifelse.(outcomes.Elected .== "Y", 1, 0)
    outcomes.Party[outcomes.Party .== "Liberal"] .= "LPC"
    outcomes.Party[outcomes.Party .== "Conservative"] .= "CPC"
    outcomes.Party[outcomes.Party .== "NDP-New Democratic Party"] .= "NDP"
    outcomes.Party[outcomes.Party .== "Bloc Qu??b??cois"] .= "BQ"
    outcomes.Party[outcomes.Party .== "Green Party"] .= "GPC"

    oth = in(p).(outcomes.Party)
    outcomes.Party[oth .== 0] .= "Other"


    outcomes = groupby(outcomes, [:RidingNumber, :Party])
    results = combine(outcomes, [:Incumbent => maximum => :Incumbent, 
                                        :Elected => maximum => :Elected, 
                                        :VoteCount => sum => :VoteCount])
    riding = groupby(results, :RidingNumber)
    riding = combine(riding, :VoteCount => sum => :TotalVotes)
    results = leftjoin(results, riding, on = :RidingNumber)
    results.VotePercent = results.VoteCount ./ results.TotalVotes

    # unstack
    results = unstack(results[:, [:RidingNumber, :Party, :VotePercent]], :Party, :VotePercent)


    results.Other = coalesce.(results.Other, 0.0)
    results.BQ = coalesce.(results.BQ, 0.0)
    results.Election_LPC = [election[1] for i in 1:size(results, 1)]
    results.Election_CPC = [election[2] for i in 1:size(results, 1)]
    results.Election_NDP = [election[3] for i in 1:size(results, 1)]
    results.Election_BQ = [election[4] for i in 1:size(results, 1)]
    results.Election_GPC = [election[5] for i in 1:size(results, 1)]
    results.Election_Other = [election[6] for i in 1:size(results, 1)]

    return results

end








# scrape data from web. Use R, as rvest makes it easy

R"""
library(rvest)

wiki <- read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_2019_Canadian_federal_election")
wiki_tables <- html_table(wiki, 
                          fill = TRUE, 
                          header = TRUE)


# Campaign period polls
campaign_polls <- wiki_tables[[2]]

# Pre-campaign polls
pre_polls <- wiki_tables[[3]]

#Pre-2021 polls
wiki_2021 <- read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_44th_Canadian_federal_election")
wiki_tables_2021 <- html_table(wiki_2021, 
                               fill = TRUE, 
                               header = TRUE)


# Pre campaign period polls for 2021 election 
pre_2021_polls <- wiki_tables_2021[[3]]

# Campaing polls for pre-2021 election
campaign_2021_polls <- wiki_tables_2021[[2]]

"""



# bring into julia
campaign_polls = @rget campaign_polls
pre_polls = @rget pre_polls
pre_2021_polls = @rget pre_2021_polls
campaign_2021_polls = @rget campaign_2021_polls



# Polling firms for subsetting
# Note: Need to keep an eye on 2021 campaign and add in any missing pollsters
# Should new pollsters show up in Wikipedia tables
polling_firms =  ["Abacus Data", "Leger", "Mainstreet Research", "Nanos Research",
                  "Campaign Research", "Innovative Research", "EKOS", "Angus Reid",
                  "Ipsos", "DART/Maru", "Forum Research", "Research Co.",
                  "Pollara", "Insights West", "Stratcom", "Counsel", "Delphi Polling",
                  "Earnscliffe/Leger"] 
parties = [:LPC, :CPC, :NDP, :BQ, :GPC]



# clean 2019 election campaign polls
dropmissing!(campaign_polls, :LPC)
campaign_polls.Polling_firm = campaign_polls[:, "Polling firm"]
campaign_polls = campaign_polls[in(polling_firms).(campaign_polls.Polling_firm), :]
campaign_polls.PollDate = campaign_polls[:, "Last dateof polling[1]"]
campaign_polls.SampleSize = campaign_polls[:, "Samplesize[3]"]
campaign_polls.SampleSize = clean_samplesize(campaign_polls.SampleSize)
campaign_polls.mode = clean_mode(campaign_polls[:, "Polling method[4]"])



# clean 2019 pre-campaign polls
dropmissing!(pre_polls, :LPC)
pre_polls.Polling_firm = pre_polls[:, "Polling firm"]
pre_polls = pre_polls[in(polling_firms).(pre_polls.Polling_firm), :]
pre_polls.PollDate = pre_polls[:, "Last dateof polling[1]"]
pre_polls.SampleSize = pre_polls[:, "Samplesize[3]"]
pre_polls.SampleSize = clean_samplesize(pre_polls.SampleSize)
pre_polls.mode = clean_mode(pre_polls[:, "Polling method[4]"])
pre_polls.LPC = parse.(Float64, pre_polls.LPC)
pre_polls.CPC = parse.(Float64, pre_polls.CPC)
pre_polls.NDP = parse.(Float64, pre_polls.NDP)
pre_polls.BQ = parse.(Float64, pre_polls.BQ)
pre_polls.GPC = ifelse.(pre_polls.GPC .== "", missing, pre_polls.GPC)
pre_polls.GPC = passmissing(parse).(Float64, pre_polls.GPC)



# Clean 2021 pre-campaign polls
dropmissing!(pre_2021_polls, :LPC)
pre_2021_polls.Polling_firm = pre_2021_polls[:, "Polling firm"]
pre_2021_polls = pre_2021_polls[in(polling_firms).(pre_2021_polls.Polling_firm), :]
pre_2021_polls.PollDate = pre_2021_polls[:, "Last dateof polling[a]"]
pre_2021_polls.SampleSize = pre_2021_polls[:, "Samplesize[c]"]
pre_2021_polls.SampleSize = clean_samplesize(pre_2021_polls.SampleSize)
pre_2021_polls.mode = clean_mode(pre_2021_polls[:, "Polling method[d]"])
pre_2021_polls.LPC = parse.(Float64, pre_2021_polls.LPC)
pre_2021_polls.CPC = parse.(Float64, pre_2021_polls.CPC)
pre_2021_polls.NDP = parse.(Float64, pre_2021_polls.NDP)
pre_2021_polls.BQ = ifelse.(pre_2021_polls.BQ .== "N/A", missing, pre_2021_polls.BQ)
pre_2021_polls.BQ = passmissing(parse).(Float64, pre_2021_polls.BQ)
pre_2021_polls.GPC = ifelse.(pre_2021_polls.GPC .== "N/A", missing, pre_2021_polls.GPC)
pre_2021_polls.GPC = passmissing(parse).(Float64, pre_2021_polls.GPC)



# Clean 2021 pre-campaign polls
dropmissing!(campaign_2021_polls, :LPC)
campaign_2021_polls.Polling_firm = campaign_2021_polls[:, "Polling firm"]
campaign_2021_polls = campaign_2021_polls[in(polling_firms).(campaign_2021_polls.Polling_firm), :]
campaign_2021_polls.PollDate = campaign_2021_polls[:, "Last dateof polling[a]"]
campaign_2021_polls.SampleSize = campaign_2021_polls[:, "Samplesize[c]"]
campaign_2021_polls.SampleSize = clean_samplesize(campaign_2021_polls.SampleSize)
campaign_2021_polls.mode = clean_mode(campaign_2021_polls[:, "Polling method[d]"])
campaign_2021_polls.LPC = parse.(Float64, campaign_2021_polls.LPC)
campaign_2021_polls.CPC = parse.(Float64, campaign_2021_polls.CPC)
campaign_2021_polls.NDP = parse.(Float64, campaign_2021_polls.NDP)
campaign_2021_polls.BQ = parse.(Float64, campaign_2021_polls.BQ)
campaign_2021_polls.GPC = parse.(Float64, campaign_2021_polls.GPC)


# Combine polls into one DataFrame
subset_vars = ["Polling_firm", "PollDate", "LPC", "CPC", "NDP", "BQ", "GPC", 
               "SampleSize", "mode"]
can_polls = vcat(campaign_polls[:, subset_vars], pre_polls[:, subset_vars], 
                  pre_2021_polls[:, subset_vars], campaign_2021_polls[:, subset_vars])


# drop missing polls because it makes my life easier
dropmissing!(can_polls)


# Dates
election_day_2015 = Date(2015, 10, 19)
election_day_2019 = Date(2019, 10, 21)
election_day_2021 = Date(2021, 09, 20)
can_polls.PollDate = replace.(can_polls.PollDate, " " => "-")
can_polls.PollDate = replace.(can_polls.PollDate, "," => "")
can_polls.PollDate = Date.(can_polls.PollDate, "U-d-y")
can_polls.NumDays =  Dates.value.(can_polls.PollDate .- election_day_2015 .+ Dates.Day(1))


# Convert poll percentages to proportions
parties_subtract = [:LPC, :CPC, :NDP, :BQ, :GPC]
can_polls[:, parties_subtract] = can_polls[:, parties_subtract] ./ 100
can_polls[:, :Other] = [1 - sum(can_polls[i,parties_subtract]) for i in 1:size(can_polls, 1)]



# Subset polls to remove :Others with a value of 0
# If included, these polls will result in overestimation of support for other parties
# Reason: A poll with 1000 respondents should reasonably have about 20 people voting "Other"
can_polls = can_polls[can_polls.Other .> 0.0000, :]


# Pollster id for indexing in Turing model
pollster_dict = Dict(key => idx for (idx, key) in enumerate(unique(can_polls.Polling_firm)))
can_polls.pollster_id = [pollster_dict[i] for i in can_polls.Polling_firm]
reverse_pollster = Dict(value => key for (key, value) in pollster_dict)


# mode id for indexing in Turing model
mode_dict = Dict(key => idx for (idx, key) in enumerate(unique(can_polls.mode)))
can_polls.mode_id = [mode_dict[i] for i in can_polls.mode]
reverse_mode= Dict(value => key for (key, value) in mode_dict)


# write CSV for polls
CSV.write("Data/can_polls2.csv", can_polls)



# Prep data for model
#parties = ["LPC", "CPC", "NDP", "BQ", "GPC"]
parties = ["LPC", "CPC", "NDP", "BQ", "GPC", "Other"]
election_2019 = Dates.value(election_day_2019 - election_day_2015) + 1
N_days = Dates.value(election_day_2021 - election_day_2015) + 1
N_polls = size(can_polls, 1)
N_pollsters = length(unique(can_polls.pollster_id))
N_parties = length(parties)
N_modes = length(unique(can_polls.mode_id))
y_mat = Matrix(can_polls[:, parties])
y_mat_moe = Matrix(calc_moe.(y_mat, can_polls.SampleSize))
#start_election = Vector([.395, .319, .197, 0.047, 0.034])
#end_election = Vector([.331, .343, 0.16, 0.076, 0.065]) 
start_election = Vector([.395, .319, .197, 0.047, 0.034, .008])
end_election = Vector([.331, .343, 0.16, 0.076, 0.065, 0.025]) 
poll_date = convert.(Int64, can_polls.NumDays)
poll_id = [1:size(can_polls, 1);]
pollster_id = can_polls.pollster_id
mode_id = Vector(can_polls.mode_id)



# define model
@model function state_space_elections(
    y::Matrix, 
    y_moe::Matrix, 
    start_election::Vector, 
    end_election::Vector, 
    poll_date::Vector{Int}, 
    poll_id::Vector{Int}, 
    N_days::Int, 
    N_polls::Int,
    N_modes::Int,
    election_2019::Int,
    N_pollsters::Int, 
    N_parties::Int,
    pollster_id::Vector{Int}, 
    mode_id::Vector{Int},
    ::Type{T} = Float64) where {T}


    # empty containers
    ?? = Matrix{T}(undef, (N_days, N_parties))
    ?? = Matrix{T}(undef, (N_polls, N_parties))
    ?? = Matrix{T}(undef, (N_polls, N_parties))
    

    # Omega and Rho for non-centered parameterization
    ?? ~ filldist(truncated(Normal(0, 0.005), 0, Inf), N_parties)
    ?? ~ LKJ(N_parties, 2.0)

    # House effects
    ?? ~ filldist(Normal(0, 0.05), N_pollsters, N_parties)
    

    # sigmas for party and pollster-by-party effects    
    ??_party ~ filldist(Exponential(1/20), N_parties)
    ??_pollster ~ filldist(Exponential(1/20), N_pollsters, N_parties)
    ??_mode ~ filldist(Exponential(1/20), N_modes, N_parties)


    # Transform parameters
    ?? ~ filldist(MvNormal(zeros(N_parties), ??), N_days-2)

    ?? = diagm(??) * ??


    ??[1, :] = start_election
    ??[election_2019, :] = end_election    
    
    # for loops to fill in random walk priors
    for t in 2:(election_2019 - 1)
        for j in 1:N_parties
            ??[t,j] = ??[t-1, j] + ??[j, t-1]
        end
    end

    for tt in (election_2019 + 1):(N_days)
        for j in 1:N_parties
            ??[tt,j] = ??[tt - 1, j] + ??[j, tt - 2]
        end
    end   

    # for loops to run model
    for i in 1:N_polls
        for j in 1:N_parties
            ??[i, j] = sqrt(??_party[j]^2 + ??_pollster[pollster_id[i], j]^2 + ??_mode[mode_id[i], j]^2 + y_moe[i, j]^2)
            ??[i, j] = ??[poll_date[i], j] + ??[pollster_id[i], j]
            y[i, j] ~ Normal(??[i, j], ??[i, j])
        end
    end 

    for j in 1:N_parties
        end_election[j] ~ Normal(??[election_2019 - 1, j], 0.001)
    end

    return ??
    #return ??, ??, ??

end

mod_election = state_space_elections(y_mat,
                                     y_mat_moe,
                                     start_election, 
                                     end_election,
                                     poll_date,
                                     poll_id,
                                     N_days,
                                     N_polls,
                                     N_modes,
                                     election_2019,
                                     N_pollsters,
                                     N_parties,
                                     pollster_id,
                                     mode_id)



# Set iters
n_adapt = 1000
n_iter = 1000
n_chains = 4

# Define and run model
Random.seed!(4329)
#Random.seed!(16102)
Turing.setadbackend(:reversediff)
Turing.setrdcache(true)
chns_election = sample(mod_election, NUTS(n_adapt, 0.8; max_depth = 12), MCMCThreads(), n_iter, n_chains)


# Save chains
save("Data/turing_model_can_election.jld", "chns_election", chns_election)


# Generate ??
??_gq = generated_quantities(mod_election, chns_election)

rs = n_iter * n_chains
?? = Array{Float64}(undef, (rs, N_days, N_parties))

for i in 1:rs
    tmp = collect(??_gq[i])
    for j in 1:N_days
        for p in 1:N_parties
        ??[i, j, p] = tmp[j, p]
        end
        #??[i, j, N_parties + 1] = 1 - sum(??[i, j, 1:N_parties])
    end
end

??_ll = Matrix{Float64}(undef, (N_days, N_parties))
??_m = Matrix{Float64}(undef, (N_days, N_parties))
??_uu = Matrix{Float64}(undef, (N_days, N_parties))

for j in 1:N_days
    for p in 1:(N_parties)
        ??_ll[j,p] = quantile(??[: ,j, p], 0.025)
        ??_m[j,p] = quantile(??[: ,j, p], 0.50)
        ??_uu[j,p] = quantile(??[: ,j, p], 0.975)
    end
end


# Plot ?? and polls over time
xi_days = election_day_2015 .+ Dates.Day.(1:N_days) .- Dates.Day(1)
colours = [:red, :blue, :orange, :cyan, :green, :purple]
parties_other = ["LPC", "CPC", "NDP", "BQ", "GPC", "Other"]


# should probably build a function to do this
plt = plot(size = (750, 500), legend = :topright, fontfamily = :Verdana, left_margin = 10mm, bottom_margin = 15mm, ylabel = "Vote intention (%)")
ylims!(plt, (0.0, 0.6))
for i in 1:length(colours)
    scatter!(plt, can_polls.PollDate, can_polls[:, parties_other[i]], label = parties_other[i], mc = colours[i])
    plot!(plt, xi_days, ??_m[:,i], ribbon = (??_m[:,i] - ??_ll[:,i], ??_uu[:,i] - ??_m[:,i]), 
          label = nothing, fc = colours[i], lc = colours[i], lw = 2)
end

title!(plt, "Estimated vote intention of Canadian voters:\n2015 to 2021", title_align= :left, titlefontsize = 12)
annotate!(plt, xi_days[end], -0.08, StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", :lower, :right, 8, :grey))
yticks!(plt, [0.0, 0.1, 0.2, 0.3, 0.4, 0.5], 
             ["0", "10", "20", "30", "40", "50"])

savefig(plt, "Images/can_vote_intention_2015_2021.png")





# Plot house effects
parties_list = repeat(parties, inner = N_pollsters)
??_ll, ??_m, ??_uu = extract_params(chns_election, "??")
pollsters = [reverse_pollster[i] for i in 1:maximum(can_polls.pollster_id)]


plt_house = []
for i in 1:length(parties)
    plt_tmp = plot(legend = false, title = parties[i], title_align = :left, xlims = (-0.1, .1),
                   fontfamily = :Verdana, 
                   bottom_margin = 15mm,
                   left_margin = 4mm)
    Plots.scatter!(plt_tmp, (??_m[parties_list .== parties[i]], pollsters), xerror = (??_m[parties_list .== parties[i]] - ??_ll[parties_list .== parties[i]], ??_uu[parties_list .== parties[i]] - ??_m[parties_list .== parties[i]]),
                   mc = :black, msc = :black)
    vline!(plt_tmp, [0.0], linestyle = :dot, lc = :orange)
    xticks!(plt_tmp, ([-.1, -0.05, 0, 0.05, .1], ["-10", "-5", "0", "5", "10"]))
    if i == 1
        yticks!(plt_tmp, 0.5:1:(length(pollsters) + 0.5), pollsters)
    else
        yaxis!(plt_tmp, y_ticks = nothing)
    end

    if i == 3
        xaxis!(x_guide = "Percent")
    end

    push!(plt_house, plt_tmp)
end

annotate!(plt_house[5], .1, -2.0, 
          StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", 
          :lower, :right, 8, :grey))

title = plot(title = "House effects: 2015 to 2021", titlefontsize = 16,
             titlefontfamily = :Verdana,
             grid = false, xaxis = nothing, yaxis = nothing, 
             showaxis = false, bottom_margin = 1mm)

plt_house_effects = plot(title,
                         plt_house[1],
                         plt_house[2],
                         plt_house[3],
                         plt_house[4],
                         plt_house[5],
                         layout = @layout([A{0.01h}; [B C D E F]]),
                         size = (1100, 750))


savefig(plt_house_effects, "Images/house_effects_pollsters.png")



# Plt 2019 to election day
plt_2019 = plot(size = (750, 500), legend = :topright, fontfamily = :Verdana, left_margin = 10mm, bottom_margin = 15mm, ylabel = "Vote intention (%)")
ylims!(plt_2019, (0.0, 0.6))
for i in 1:length(colours)
    scatter!(plt_2019, can_polls.PollDate[can_polls.PollDate .??? Date(2019, 10, 21)], 
             can_polls[can_polls.PollDate .??? Date(2019, 10, 21), parties_other[i]], 
             label = parties_other[i], mc = colours[i])
    plot!(plt_2019, xi_days[xi_days .??? Date(2019, 10, 21)], 
          ??_m[xi_days .??? Date(2019, 10, 21), i], 
          ribbon = (??_m[xi_days .??? Date(2019, 10, 21), i] - ??_ll[xi_days .??? Date(2019, 10, 21), i], 
                    ??_uu[xi_days .??? Date(2019, 10, 21), i] - ??_m[xi_days .??? Date(2019, 10, 21), i]), 
                    label = nothing, fc = colours[i], lc = colours[i], lw = 2)
end

title!(plt_2019, "Estimated vote intention of Canadian voters:\n2019 to 2021", title_align= :left, titlefontsize = 12)
annotate!(plt_2019, xi_days[end], -0.08, StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", :lower, :right, 8, :grey))
yticks!(plt_2019, [0.0, 0.1, 0.2, 0.3, 0.4, 0.5], 
             ["0", "10", "20", "30", "40", "50"])

savefig(plt_2019, "Images/can_vote_intention_2019_2021.png")



# Plot densities for vote share
plt_dens = plot(size = (750, 500), 
                title = "Estimated vote intention: $day_title",
                title_align= :left, bottom_margin = 12mm, showaxis = :x,
                y_ticks = nothing, fontfamily = :Verdana)
for i in 1:(N_parties)
    StatsPlots.density!(plt_dens, ??[:, xi_days .== update_date, i], 
                        label = parties_other[i], fill = (0, .2, colours[i]),
                        lc = colours[i], lw = 2)
end

annotate!(plt_dens, .37, -18, StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", :lower, :right, 8, :grey))
xticks!(plt_dens, [0.1, 0.2, 0.3, 0.4, 0.5], 
             ["10", "20", "30", "40", "50"])
xlabel!(plt_dens, "Percent")


plt_dens

savefig(plt_dens, "Images/can_vote_intention_on_election_date.png")


# Plt campaign period
plt_campaign = plot(size = (750, 500), legend = :topright, fontfamily = :Verdana, left_margin = 10mm, bottom_margin = 15mm, ylabel = "Vote intention (%)")
ylims!(plt_campaign, (0.0, 0.6))
for i in 1:length(colours)
    scatter!(plt_campaign, can_polls.PollDate[can_polls.PollDate .??? Date(2021, 08, 15)], 
             can_polls[can_polls.PollDate .??? Date(2021, 08, 15), parties_other[i]], 
             label = parties_other[i], mc = colours[i])
    plot!(plt_campaign, xi_days[xi_days .??? Date(2021, 08, 15)], 
          ??_m[xi_days .??? Date(2021, 08, 15), i], 
          ribbon = (??_m[xi_days .??? Date(2021, 08, 15), i] - ??_ll[xi_days .??? Date(2021, 08, 15), i], 
                    ??_uu[xi_days .??? Date(2021, 08, 15), i] - ??_m[xi_days .??? Date(2021, 08, 15), i]), 
                    label = nothing, fc = colours[i], lc = colours[i], lw = 2)
end

title!(plt_campaign, "Estimated vote intention of Canadian voters:\nElection campaign 2021", title_align= :left, titlefontsize = 12)
annotate!(plt_campaign, xi_days[end], -0.08, StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", :lower, :right, 8, :grey))
yticks!(plt_campaign, [0.0, 0.1, 0.2, 0.3, 0.4, 0.5], 
             ["0", "10", "20", "30", "40", "50"])

savefig(plt_campaign, "Images/can_vote_intention_campaign_period.png")












#### BYM2 model to estimate votes by riding ####

# Helper functions
function convolve_re(??::Vector, ??::Vector, id_num::Vector, ??, sf)

    re = sqrt(1 - ??) .* ??[id_num] .+ sqrt(?? / sf) .* ??[id_num]

    return re

end

function icar_adjustment(??::Vector, node1::Vector, node2::Vector)

    x = ??[node1] - ??[node2]

    return -0.5 * dot(x, x)

end


# Load data
nodes = CSV.read("nodes.csv", DataFrame)
n_edges = 806
n_nodes = 338
scaling_factor = 6.967394

# Load outcomes 2015 and 2019
files_2015 = readdir("Vote 2015", join = true)
files_2019 = readdir("Vote 2019", join = true)



# 2015 results
outcomes_vars = ["RidingNumber", "Elected", "Party", "Incumbent", "VoteCount"]
results_2015 = clean_results(files_2015, outcomes_vars, parties[1:5], start_election)

# 2019 results
results_2019 = clean_results(files_2019, outcomes_vars, parties[1:5], end_election)

# combine
results = [results_2015; results_2019]

riding_dict = Dict(key => idx for (idx, key) in enumerate(unique(results.RidingNumber)))
results.RidingNumber_id = [riding_dict[i] for i in results.RidingNumber]
reverse_riding = Dict(value => key for (key, value) in riding_dict)


results_2021 = DataFrame(:Election_LPC => ??_m[xi_days .== update_date, 1],
                         :Election_CPC => ??_m[xi_days .== update_date, 2],
                         :Election_NDP => ??_m[xi_days .== update_date, 3],
                         :Election_BQ => ??_m[xi_days .== update_date, 4],
                         :Election_GPC => ??_m[xi_days .== update_date, 5],
                         :Election_Other => ??_m[xi_days .== update_date, 6])
std_2021 = DataFrame(:std_LPC => std(??[:, xi_days .== update_date, 1]),
                     :std_CPC => std(??[:, xi_days .== update_date, 2]),
                     :std_NDP => std(??[:, xi_days .== update_date, 3]),
                     :std_BQ => std(??[:, xi_days .== update_date, 4]),
                     :std_GPC => std(??[:, xi_days .== update_date, 5]),
                     :std_Other => std(??[:, xi_days .== update_date, 6]))


@model function bym2(N, N_edges, node1, node2, y, x, x_obs, x_std, 
                     scaling_factor, id_num, id_num_new)

    # priors
    #?? ~ Normal(0, 1)
    #?? ~ Normal(0, 1)

    ?? ~ truncated(Normal(0, 1), 0, Inf)
    ?? ~ truncated(Normal(0, 1), 0, Inf)
    ?? ~ filldist(Uniform(-1, 1), N)
    z_?? ~ Normal(0, 1)
    sum_?? ~ Normal(0, 0.0001 * N)
  
    ?? ~ filldist(Normal(0, 1), N)

    ?? ~ Beta(0.5, 0.5)

    x_est ~ Uniform(0, 1)
    
    # model
    sum_?? = sum(??)
    convolved_re = sqrt(1 - ??) .* ??[id_num] .+ sqrt(?? / scaling_factor) .* ??[id_num]


    Turing.@addlogprob! -0.5 * dot(??[node1] - ??[node2], ??[node1] - ??[node2])
    ?? =  x .+ convolved_re[id_num] .* ??
    y ~ MvNormal(??, ??)
    x_obs ~ Normal(x_est, x_std)

    # gq 
    y_new = x_est .+ convolved_re[id_num_new] .* ?? .+ ?? .* z_?? 
 
    return y_new

end


n_iter_bym = 2000
n_adapt_bym = 1000

model_bym2 = []
chns_bym2 = []
for i in 1:length(parties)
    party = parties[i]
    x_obs = results_2021[1, "Election_$party"]
    x_std = std_2021[1, "std_$party"]
    push!(model_bym2, bym2(n_nodes,
                           n_edges,
                           nodes.node1,
                           nodes.node2,
                           Float64.(results[:, party]),
                           results[:, "Election_$party"],
                           x_obs,
                           x_std,
                           scaling_factor,
                           results.RidingNumber_id,
                           [1:338;])) 
    push!(chns_bym2, sample(model_bym2[i], NUTS(n_adapt_bym, 0.99), 
                            MCMCThreads(), n_iter_bym, 5))
    #push!(chns_bym2, sample(model_bym2[i], NUTS(n_adapt_bym, 0.8), 
    #                        n_iter_bym))

    Turing.emptyrdcache()
end


seats_2021_gq = []
for i in 1:6
    push!(seats_2021_gq, generated_quantities(model_bym2[i], chns_bym2[i]))
end


rs_bym2 = n_iter_bym * 5
votes_2021 = Array{Float64}(undef, (rs_bym2, 338, N_parties))

for p in 1:N_parties
    for i in 1:rs_bym2
        tmp = collect(seats_2021_gq[p][i])
        for j in 1:338
            votes_2021[i, j, p] = tmp[j]
        end
    end
end


pred_winner = Matrix{String}(undef, (rs_bym2, 338))
for i in 1:rs_bym2
    for j in 1:338
    winner = parties[argmax([votes_2021[i, j, 1],
                             votes_2021[i, j, 2],
                             votes_2021[i, j, 3],
                             votes_2021[i, j, 4],
                             votes_2021[i, j, 5],
                             votes_2021[i, j, 6]])]
    pred_winner[i, j] = winner
    end
end

num_seats = Matrix{Int64}(undef, (rs_bym2, 6))
for i in 1:rs_bym2
    for j in 1:length(parties)
        num_seats[i, j] = sum(pred_winner[i, :] .== parties[j])
    end
end




# Plot densities for vote share
plt_seats = plot(size = (750, 500), 
                 title = "Estimated seat count: $day_title",
                 title_align= :left, bottom_margin = 12mm, showaxis = :x,
                 y_ticks = nothing, fontfamily = :Verdana)
for i in 1:(N_parties)
    StatsPlots.histogram!(plt_seats, num_seats[:, i], 
                          label = parties_other[i], fill = (0, .2, colours[i]),
                          lc = colours[i], lw = 2)
end

annotate!(plt_seats, maximum(num_seats) + 10, -1750, StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", :lower, :right, 8, :grey))
xlabel!(plt_seats, "Seats")


plt_seats

savefig(plt_seats, "can_seat_count_on_election_date.png")



# Get values
get_value(value_date)
get_seats()
sum(num_seats[:,1] .> num_seats[:,2]) / 10000

seat_colours = Vector{String}(undef, size(num_seats, 1))
for i in 1:size(num_seats, 1)
    if num_seats[i, 1] > 170
        seat_colours[i] = "red"
    elseif num_seats[i, 2] > 170
        seat_colours[i] = "blue"
    else 
        seat_colours[i] = "grey"
    end
end

plt_majority = StatsPlots.scatter(num_seats[:,1], num_seats[:,2], alpha = 0.3, 
                                  legend = nothing, msc = seat_colours, 
                                  mc = seat_colours, size = (750, 500), left_margin = 10mm,
                                  bottom_margin = 12mm)
hline!(plt_majority, [170], ls = :dash, lc = :blue, legend = nothing)
vline!(plt_majority, [170], ls = :dash, lc = :red, legend = nothing)
title!(plt_majority, "Estimated seat counts and chance of majority", title_align= :left, titlefontsize = 12)
ylabel!(plt_majority, "Seats CPC")
xlabel!(plt_majority, "Seats LPC")
annotate!(plt_majority, maximum(num_seats[:,1]), minimum(num_seats[:, 2]) - 27, StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", :lower, :right, 8, :grey))


savefig(plt_majority, "can_seat_count_LPC_CPC_majority.png")

get_seats()
get_value(update_date)
sum(num_seats[:, 1] .> num_seats[:, 2]) / size(num_seats, 1)
