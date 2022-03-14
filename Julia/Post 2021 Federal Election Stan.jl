
# activate FederalElection
ENV["CMDSTAN"] = expanduser("~/cmdstan/")

using Plots, StatsPlots
using RCall
using DataFrames, CSV
using StanSample
using StatsFuns
using Dates
using Measures
using Random 
using PlotlyBase
using MCMCChains
using StatsBase



# Set some global variables for Plots
updated_date = "March 3, 2022"
day_title = "March 3, 2022"
update_date = Date(2022, 03, 03)
value_date = Date(2022, 03, 03)
dateformat = DateFormat("y-m-d")



include("01_custom_functions.jl")


# scrape data from web. Use R, as rvest makes it easy

R"""
library(rvest)

wiki <- read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_45th_Canadian_federal_election")
wiki_tables <- html_table(wiki, 
                          fill = TRUE, 
                          header = TRUE)


# Pre-campaign polls
pre_polls <- wiki_tables[[2]]

#Pre-2021 polls
wiki_2021 <- read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_44th_Canadian_federal_election")
wiki_tables_2021 <- html_table(wiki_2021, 
                               fill = TRUE, 
                               header = TRUE)


# Pre campaign period polls for 2021 election 
pre_2021_polls <- wiki_tables_2021[[4]]

# Campaing polls for pre-2021 election
campaign_2021_polls <- wiki_tables_2021[[3]]

"""



# bring into julia
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
parties = [:LPC, :CPC, :NDP, :BQ, :GPC, :PPC]
colours = [:red, :blue, :orange, :cyan, :green, :purple, :yellow]
parties_names = ["LPC", "CPC", "NDP", "BQ", "GPC", "PPC", "Other"]


# clean 2021+ pre-campaign polls
dropmissing!(pre_polls, :LPC)
pre_polls.Polling_firm = pre_polls[:, "Polling firm"]
pre_polls = pre_polls[in(polling_firms).(pre_polls.Polling_firm), :]
pre_polls.PollDate = pre_polls[:, "Last dateof polling[a]"]
pre_polls.Others = pre_polls[:, "Others[b]"]
pre_polls.SampleSize = pre_polls[:, "Samplesize[d]"]
pre_polls.SampleSize = clean_samplesize(pre_polls.SampleSize)
pre_polls.mode = clean_mode(pre_polls[:, "Polling method[e]"])
pre_polls.LPC = parse.(Float64, pre_polls.LPC)
pre_polls.CPC = parse.(Float64, pre_polls.CPC)
pre_polls.NDP = parse.(Float64, pre_polls.NDP)
pre_polls.BQ = ifelse.(pre_polls.BQ .== "N/A", missing, pre_polls.BQ)
pre_polls.BQ = passmissing(parse).(Float64, pre_polls.BQ)
pre_polls.PPC = ifelse.(pre_polls.PPC .== "N/A", missing, pre_polls.PPC)
pre_polls.PPC = passmissing(parse).(Float64, pre_polls.PPC)
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
pre_2021_polls.PPC = ifelse.(pre_2021_polls.PPC .== "N/A", missing, pre_2021_polls.PPC)
pre_2021_polls.PPC = passmissing(parse).(Float64, pre_2021_polls.PPC)



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
campaign_2021_polls.PPC = parse.(Float64, campaign_2021_polls.PPC)


# Combine polls into one DataFrame
subset_vars = ["Polling_firm", "PollDate", "LPC", "CPC", "NDP", "BQ", "GPC", "PPC",
               "SampleSize", "mode"]
can_polls = vcat(pre_polls[:, subset_vars], 
                  pre_2021_polls[:, subset_vars], campaign_2021_polls[:, subset_vars])


# drop missing polls because it makes my life easier
dropmissing!(can_polls)


# Dates
election_day_2019 = Date(2019, 10, 21)
election_day_2021 = Date(2021, 09, 20)
can_polls.PollDate = replace.(can_polls.PollDate, " " => "-")
can_polls.PollDate = replace.(can_polls.PollDate, "," => "")
can_polls.PollDate = Date.(can_polls.PollDate, "U-d-y")
can_polls.NumDays =  Dates.value.(can_polls.PollDate .- election_day_2019 .+ Dates.Day(1))


# Convert poll percentages to proportions
parties_subtract = [:LPC, :CPC, :NDP, :BQ, :GPC, :PPC]
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
CSV.write("Data/can_polls_after_2021.csv", can_polls)



# Prep data for model
#parties = ["LPC", "CPC", "NDP", "BQ", "GPC"]
parties = ["LPC", "CPC", "NDP", "BQ", "GPC", "PPC", "Other"]
election_2021 = Dates.value(election_day_2021 - election_day_2019) + 1
N_days = Dates.value(update_date - election_day_2019) + 1
N_polls = size(can_polls, 1)
N_pollsters = length(unique(can_polls.pollster_id))
N_parties = length(parties)
N_modes = length(unique(can_polls.mode_id))
y_mat = Matrix(can_polls[:, parties])
y_mat_moe = Matrix(calc_moe.(y_mat, can_polls.SampleSize))
#start_election = Vector([.395, .319, .197, 0.047, 0.034])
#end_election = Vector([.331, .343, 0.16, 0.076, 0.065]) 
start_election = Vector([.331, .343, 0.16, 0.076, 0.065, 0.016, 0.009])
mid_election = Vector([.326, .337, 0.178, 0.076, 0.023, 0.049, 0.011]) 
poll_date = convert.(Int64, can_polls.NumDays)
poll_id = [1:size(can_polls, 1);]
pollster_id = can_polls.pollster_id
mode_id = Vector(can_polls.mode_id)


state_space_ncp = read("Stan/state_space_pooling_polls_ncp.stan", String)

data = Dict(
  
    "N_days" => N_days,
    "N_parties" => N_parties,
    "N_polls" => N_polls,
    "N_pollsters" => N_pollsters,
    "N_modes" => N_modes,
    
    "xi_start" => start_election,
    "xi_mid" => mid_election,
    "election_day_mid" => election_2021,
    
    "y" => y_mat',
    "y_moe" => y_mat_moe',
    
    "poll_date" => poll_date,
    "pollster_id" => pollster_id,
    
    "mode_id" => mode_id

  )


tmpdir = joinpath(@__DIR__, "tmp")

sm = SampleModel("ss_ncp", state_space_ncp, tmpdir)
rc = stan_sample(sm; data, num_chains = 4, seed = 943129384)


if success(rc)
    st = read_samples(sm)
    params = DataFrame(st)
end


# extract parameters
ξ_raw = params[:, contains.(names(params), "xi")]
δ_raw = params[:, contains.(names(params), "delta")]

# Build containers
ξ = Array{Float64}(undef, size(ξ_raw, 1), N_days, N_parties)
ξ_summary = Array{Float64}(undef, 5, N_days, N_parties)
δ = Array{Float64}(undef, size(δ_raw, 1), N_pollsters, N_parties)
δ_summary = Array{Float64}(undef, 5, N_pollsters, N_parties)


# enter starting values for for-loop
p_start = 1
p_end = N_days
pollster_start = 1
pollster_end = N_pollsters
xi_days = election_day_2019 .+ Dates.Day.(1:N_days) .- Dates.Day(1)
day_180_before = update_date - Dates.Day(180)

for p in 1:N_parties
    ξ[:, :, p] .= ξ_raw[:, p_start:p_end]
    δ[:, :, p] .= δ_raw[:, pollster_start:pollster_end]
    p_start = p_end + 1
    p_end = (p + 1) * N_days
    pollster_start = pollster_end + 1
    pollster_end = (p + 1) * N_pollsters

    for j in 1:N_days

        # xi: latent vote intention
        ξ_summary[1, j, p] = mean(ξ[:, j, p])
        ξ_summary[2, j, p] = quantile(ξ[:, j, p], 0.025)
        ξ_summary[3, j, p] = quantile(ξ[:, j, p], 0.975)
        ξ_summary[4, j, p] = ξ_summary[1, j, p] - ξ_summary[2, j, p]
        ξ_summary[5, j, p] = ξ_summary[3, j, p] - ξ_summary[1, j, p]
        
    end

    # house effects
    for pollster in 1:N_pollsters
        δ_summary[1, pollster, p] = mean(δ[:, pollster, p])
        δ_summary[2, pollster, p] = quantile(δ[:, pollster, p], 0.025)
        δ_summary[3, pollster, p] = quantile(δ[:, pollster, p], 0.975)
        δ_summary[4, pollster, p] = δ_summary[1, pollster, p] - δ_summary[2, pollster, p]
        δ_summary[5, pollster, p] = δ_summary[3, pollster, p] - δ_summary[1, pollster, p]
    end


end
  
plt_trend = plot(size = (750, 500), 
                 legend = :topright, fontfamily = :Verdana, 
                 left_margin = 10mm, bottom_margin = 15mm, 
                 ylabel = "Vote intention (%)")
ylims!(plt_trend, (0.0, 0.55))
for i in 1:length(colours)
    scatter!(plt_trend, can_polls.PollDate, 
             can_polls[:, parties[i]], 
             label = parties_names[i], 
             mc = colours[i])
    plot!(plt_trend, xi_days, 
          ξ_summary[1, :, i], 
          ribbon = (ξ_summary[4,:,i], 
                    ξ_summary[5,:,i]), 
          label = nothing, fc = colours[i], 
           lc = colours[i], lw = 2)
end

title!(plt_trend, "Estimated vote intention of Canadian voters:\n2019 to 2022", 
       title_align= :left, titlefontsize = 12)
annotate!(plt_trend, xi_days[end], -0.08, 
          StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", 
                          :lower, :right, 8, :grey))
yticks!(plt_trend, [0.0, 0.1, 0.2, 0.3, 0.4, 0.5], 
             ["0", "10", "20", "30", "40", "50"])

savefig(plt_trend, "Images/Federal/can_vote_intention_2019_post_2021.png")


# Plot house effects
pollster_list = [reverse_pollster[i] for i in 1:N_pollsters]


plt_house = []
for i in 1:length(parties)
    plt_tmp = plot(legend = false, title = parties[i], 
                   title_align = :left, xlims = (-0.1, .1),
                   fontfamily = :Verdana, 
                   bottom_margin = 15mm,
                   left_margin = 4mm,
                   grid = :x)
    Plots.scatter!(plt_tmp, (δ_summary[1, :, i], pollster_list), 
                   xerror = (δ_summary[4, :, i], 
                             δ_summary[5, :, i]),
                   mc = :black, msc = :black)
    vline!(plt_tmp, [0.0], linestyle = :dot, lc = :orange)
    xticks!(plt_tmp, ([-.1, -0.05, 0, 0.05, .1], ["-10", "-5", "0", "5", "10"]))
    if i == 1
        yticks!(plt_tmp, 0.5:1:(N_pollsters - 0.5), pollster_list)
    else
        yaxis!(plt_tmp, y_ticks = nothing)
    end

    if i == 4
        xaxis!(x_guide = "Percent")
    end

    push!(plt_house, plt_tmp)
end

annotate!(plt_house[7], .1, -1.25, 
          StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", 
          :lower, :right, 8, :grey))

title = plot(title = "House effects at the federal level: 2019 to 2022", 
             titlefontsize = 20,
             titlefontfamily = :Verdana,
             titleposition = :left,
             grid = false, xaxis = nothing, yaxis = nothing, 
             showaxis = false)

plt_house_effects = plot(title,
                         plt_house[1],
                         plt_house[2],
                         plt_house[3],
                         plt_house[4],
                         plt_house[5],
                         plt_house[6],
                         plt_house[7],
                         layout = @layout([A{0.01h}; [B C D E F G H]]),
                         size = (1100, 750))


savefig(plt_house_effects, "Images/Federal/can_house_effects_pollsters_2019_2022.png")


plt_dens = plot(size = (750, 500), 
                title = "Estimated vote intention at the federal level: $day_title",
                title_align= :left, 
                bottom_margin = 12mm, showaxis = :x,
                right_margin = 2mm,
                y_ticks = nothing, fontfamily = :Verdana,
                titlefontsize = 18)
for i in 1:(N_parties)
    StatsPlots.density!(plt_dens, ξ[:, end, i], 
                        label = parties_names[i], fill = (0, .2, colours[i]),
                        lc = colours[i], lw = 2)
end

ann_loc = maximum(ξ) - 0.03
annotate!(plt_dens, ann_loc, -30, 
          StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", 
                          :lower, :right, 8, :grey))
xticks!(plt_dens, [0.0, 0.1, 0.2, 0.3, 0.4], 
        ["0%", "10%", "20%", "30%", "40%"],
        xtickfontsize = 14)

savefig(plt_dens, "Images/Federal/can_vote_intention_2022.png")



plt_trend_180 = plot(size = (750, 500), 
                    legend = :topright, fontfamily = :Verdana, 
                    left_margin = 10mm, bottom_margin = 15mm, 
                    top_margin = 5mm,
                    ylabel = "Vote intention (%)",
                    tickfontsize = 12,
                    guidefontsize = 14,
                    xrotation = 45)
ylims!(plt_trend_180, (0.0, 0.60))
for i in 1:length(colours)
    scatter!(plt_trend_180, can_polls.PollDate[can_polls.PollDate .≥ day_180_before], 
             can_polls[can_polls.PollDate .≥ day_180_before, parties[i]], 
             label = parties_names[i], 
             mc = colours[i],
             markersize = 6)
    plot!(plt_trend_180, xi_days[xi_days .≥ day_180_before], 
          ξ_summary[1, xi_days .≥ day_180_before, i], 
          ribbon = (ξ_summary[4, xi_days .≥ day_180_before, i], 
                    ξ_summary[5, xi_days .≥ day_180_before, i]), 
          label = nothing, fc = colours[i], 
           lc = colours[i], lw = 2)
end

title!(plt_trend_180, "Estimated vote intention at the federal level:\nLast 180 days", 
       title_align= :left, titlefontsize = 18)
annotate!(plt_trend_180, xi_days[end], -0.22, 
          StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", 
                          :lower, :right, 8, :grey))
yticks!(plt_trend_180, [0.0, 0.1, 0.2, 0.3, 0.4, 0.5], 
             ["0", "10", "20", "30", "40", "50"])

savefig(plt_trend_180, "Images/Federal/can_vote_intention_last_180_days.png")

[ξ_summary[:, end, i] for i in 1:size(ξ, 3)]
