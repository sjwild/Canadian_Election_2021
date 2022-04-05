
abstract type ElectionResults end

mutable struct ElectionSummary <: ElectionResults
    ξ::Array{Float64}
    ξ_last::Matrix{Float64}
    parties::Vector{String}
    pollsters::Vector{String}
    house_effects::Array{Float64}
    dates::Vector{Date}
end



# These are a collection of custom functions to clean the poll data
# custom functions
function clean_mode(x::Vector{String})

    xmode = Vector{Union{Missing, String}}(undef, length(x))

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

function clean_mode(x::Vector{Union{Missing, String}})

    xmode = Vector{Union{Missing, String}}(undef, length(x))

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


function clean_mean(x::Vector{String})

    tmp = Vector{Union{Missing, Float64}}(undef, length(x))

    idx = in(["N/A", "–", "n/a", "—"]).(x)

    tmp[Not(idx)] = parse.(Float64, x[Not(idx)])
    
    return tmp
end

function clean_mean(x::Vector{Union{Missing, String}})

    tmp = Vector{Union{Missing, Float64}}(undef, length(x))

    idx = in(["N/A", "–", "n/a", "—"]).(x)

    tmp[Not(idx)] = parse.(Float64, x[Not(idx)])
    
    return tmp
end

function clean_mean(x::Vector{Float64})
    return x
end

function clean_mean(x::Vector{Union{Missing, Float64}})
    return x
end


function clean_samplesize(x::Vector)

    x = replace.(x, " (1/3)" => "")
    x = replace.(x, " (2/3)" => "")
    x = replace.(x, " (3/3)" => "")
    x = replace.(x, " (1/2)" => "")
    x = replace.(x, " (2/2)" => "")
    x = replace.(x, " (1/4)" => "")
    x = replace.(x, "+" => "")
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


"""
function calc_date takes two date value and returns the difference between them + 1

"""
function calc_date(firstdate::Date,
    seconddate::Date)
return Dates.value(firstdate - seconddate) + 1
end

calc_date(firstdate::String, seconddate::String) = cacl_date(Date(firstdate), Date(seconddate))

function parties_other(parties::Vector{Symbol})
    if sum(contains.(String.(parties), "Other")) != 0
        parties_tmp = String.(parties)
    else
        parties_tmp = vcat(String.(parties), "Other")
    end

    return parties_tmp
end



function prep_stan_data(parties::Vector{Symbol},
                        first_election_date::Date,
                        second_election_date::Date,
                        prediction_date::Date,
                        first_election_results::Vector{Float64},
                        second_election_results::Vector{Float64},
                        polls::DataFrame)

# Ensure correct number of parties
    parties_tmp = parties_other(parties)

    # build matrices needed below
    y_mat = Matrix{Float64}(polls[:, parties_tmp])
    y_mat_moe = Matrix{Float64}(calc_moe.(y_mat, polls.SampleSize))

    data = Dict(

        "N_days" => calc_date(prediction_date, first_election_date),
        "N_parties" => length(parties_tmp),
        "N_polls" => size(polls, 1),
        "N_pollsters" => length(unique(polls.pollster_id)),
        "N_modes" => length(unique(polls.mode_id)),

        "xi_start" => first_election_results,
        "xi_mid" => second_election_results,
        "election_day_mid" => calc_date(second_election_date, first_election_date),

        "y" => y_mat',
        "y_moe" => y_mat_moe',

        "poll_date" => polls.NumDays,
        "pollster_id" => polls.pollster_id,

        "mode_id" => polls.mode_id

    ) 

    return data

end


function extract_params(rc::Base.ProcessChain,
                        model::SampleModel)
    
    if success(rc)
        st = read_samples(model)
        params = DataFrame(st)
    end

    return params
end


function get_xi(params::DataFrame,
                N_parties::Int64,
                N_days::Int64)

    ξ_raw = params[:, contains.(names(params), "xi")]
    ξ = Array{Float64}(undef, size(ξ_raw, 1), N_days, N_parties)
    ξ_summary = Array{Float64}(undef, 5, N_days, N_parties)
    ξ_last = Matrix{Float64}(undef, size(params, 1), N_parties)

    p_start = 1
    p_end = N_days

    for p in 1:N_parties
        ξ[:, :, p] .= ξ_raw[:, p_start:p_end]
        p_start = p_end + 1
        p_end = (p + 1) * N_days

        for j in 1:N_days
    
            # xi: latent vote intention
            ξ_summary[1, j, p] = mean(ξ[:, j, p])
            ξ_summary[2, j, p] = quantile(ξ[:, j, p], 0.025)
            ξ_summary[3, j, p] = quantile(ξ[:, j, p], 0.975)
            ξ_summary[4, j, p] = ξ_summary[1, j, p] - ξ_summary[2, j, p]
            ξ_summary[5, j, p] = ξ_summary[3, j, p] - ξ_summary[1, j, p]
            
        end

        ξ_last[:, p] = ξ[:, N_days, p]

    end


    return ξ_summary, ξ_last
end

function get_delta(params::DataFrame,
                   N_parties::Int64,
                   N_pollsters::Int64)

    δ_raw = params[:, contains.(names(params), "delta")]
    δ = Array{Float64}(undef, size(δ_raw, 1), N_pollsters, N_parties)
    δ_summary = Array{Float64}(undef, 5, N_pollsters, N_parties)
    
    pollster_start = 1
    pollster_end = N_pollsters

    for p in 1:N_parties
        δ[:, :, p] .= δ_raw[:, pollster_start:pollster_end]
        pollster_start = pollster_end + 1
        pollster_end = (p + 1) * N_pollsters
    
        # house effects
        for pollster in 1:N_pollsters
            δ_summary[1, pollster, p] = mean(δ[:, pollster, p])
            δ_summary[2, pollster, p] = quantile(δ[:, pollster, p], 0.025)
            δ_summary[3, pollster, p] = quantile(δ[:, pollster, p], 0.975)
            δ_summary[4, pollster, p] = δ_summary[1, pollster, p] - δ_summary[2, pollster, p]
            δ_summary[5, pollster, p] = δ_summary[3, pollster, p] - δ_summary[1, pollster, p]
        end
    
    
    end

    return δ_summary

end



function summarize_data(rc::Base.ProcessChain,
                        model::SampleModel,
                        parties::Vector{Symbol},
                        N_pollsters::Int64,
                        reverse_pollster::Dict,
                        first_election_date::Date,
                        prediction_date::Date)
    parties_tmp = parties_other(parties)

    N_days = calc_date(prediction_date, first_election_date)
    pollster_list = [reverse_pollster[i] for i in 1:N_pollsters]

    params = extract_params(rc, model)

    xi_summary, xi_last = get_xi(params, length(parties_tmp), N_days)

    delta_summary = get_delta(params, length(parties_tmp), N_pollsters)

    xi_days = first_election_date .+ Dates.Day.(1:N_days) .- Dates.Day(1)

    return ElectionSummary(xi_summary,
                           xi_last,
                           parties_tmp,
                           pollster_list,
                           delta_summary,
                           xi_days)

end




function plot_trend(results::ElectionResults,
    polls::DataFrame,
    party_colours::Dict,
    title_text::String;
    yticks::Tuple = ([0.0, 0.1, 0.2, 0.3, 0.4, .5], 
                      ["0", "10", "20", "30", "40", "50"]),
    annotationx::Date = results.dates[end], 
    annotationy::Float64 = -0.08,
    kwargs...)

    plt = plot(size = (750, 500), 
               legend = :topright, fontfamily = :Verdana, 
               left_margin = 10mm, bottom_margin = 10mm, 
               top_margin = 6mm,
               ylabel = "Vote intention (%)",
               title = title_text, 
               title_align= :left, 
               titlefontsize = 20,
               guidefontsize = 14,
               tickfontsize = 12,
               legendfontsize = 10,
               yticks = yticks,
               annotation = (annotationx, annotationy, #annotation_location
                             Plots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", 
                                        :lower, :right, 8, :grey));
               kwargs...)

    for i in 1:length(results.parties)
        scatter!(plt, polls.PollDate, 
                 polls[:, results.parties[i]], 
                 label = results.parties[i], 
                 mc = party_colours[results.parties[i]])
        plot!(plt, results.dates,
              results.ξ[1, :, i], 
              ribbon = (results.ξ[4, :, i], 
                        results.ξ[5, :, i]), 
              label = nothing, 
              fc = party_colours[results.parties[i]], 
              lc = party_colours[results.parties[i]], lw = 2)
    end

    return plt

end


function plot_trend(results::ElectionResults,
    polls::DataFrame,
    party_colours::Dict,
    title_text::String,
    last180::Symbol;
    yticks::Tuple = ([0.0, 0.1, 0.2, 0.3, 0.4, .5], 
                      ["0", "10", "20", "30", "40", "50"]),
    annotationx::Date = results.dates[end], 
    annotationy::Float64 = -0.08,
    kwargs...)

    if last180 == :last180

        tmp_dates = results.dates[(end-180):end]
        tmp_results = results.ξ[:, (end-180):end, :]
        tmp_polls = polls[polls.PollDate .> minimum(tmp_dates), :]


        plt = plot(size = (750, 500), 
                legend = :topright, fontfamily = :Verdana, 
                left_margin = 10mm, bottom_margin = 10mm, 
                top_margin = 6mm,
                ylabel = "Vote intention (%)",
                title = title_text, 
                title_align= :left, 
                titlefontsize = 20,
                guidefontsize = 14,
                tickfontsize = 12,
                legendfontsize = 10,
                yticks = yticks,
                annotation = (annotationx, annotationy, #annotation_location
                                Plots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", 
                                            :lower, :right, 8, :grey));
                kwargs...)

        for i in 1:length(results.parties)
            scatter!(plt, tmp_polls.PollDate, 
                     tmp_polls[:, results.parties[i]], 
                     label = results.parties[i], 
                     mc = party_colours[results.parties[i]])
            plot!(plt, tmp_dates,
                  tmp_results[1, :, i], 
                  ribbon = (tmp_results[4, :, i], 
                            tmp_results[5, :, i]), 
                  label = nothing, 
                  fc = party_colours[results.parties[i]], 
                  lc = party_colours[results.parties[i]], lw = 2)
        end
    
    end

    return plt

end


    


function plot_house(results::ElectionResults,
    title_text::String;
    xticks::Tuple = ([-.1, -0.05, 0, 0.05, .1], 
                     ["-10", "-5", "0", "5", "10"]),
    kwargs...)

    plt_house = []
    
    for i in 1:length(results.parties)
    plt_tmp = plot(legend = false, title = results.parties[i], 
                   title_align = :left, 
                   xlims = (-0.1, 0.1),
                   fontfamily = :Verdana, 
                   left_margin = 4mm,
                   grid = :x,
                   xticks = xticks,
                   kwargs...)
    Plots.scatter!(plt_tmp, (results.house_effects[1, :, i], results.pollsters), 
                   xerror = (results.house_effects[4, :, i], 
                             results.house_effects[5, :, i]),
                   mc = :black, msc = :black)
    vline!(plt_tmp, [0.0], linestyle = :dot, lc = :orange)

    if i == 1
        yticks!(plt_tmp, 0.5:1:(length(results.pollsters) - 0.5), results.pollsters)
    else
        yaxis!(plt_tmp, y_ticks = nothing)
    end


    push!(plt_house, plt_tmp)
    end


    title = plot(title = title_text, 
                 titlefontsize = 20,
                 titlefontfamily = :Verdana,
                 titleposition = :left,
                 grid = false, xaxis = nothing, yaxis = nothing, 
                 showaxis = false)

    ann = plot(title = "Percent", 
               titlefontsize = 12,
               titlefontfamily = :Verdana,
               titleposition = :center,
               grid = false, xaxis = nothing, yaxis = nothing, 
               showaxis = false)
    annotate!(ann, 1, 0.5, 
              StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", 
              :lower, :right, 8, :grey))

    return plt_house, title, ann

end


function plot_density(results::ElectionResults,
    party_colours::Dict,
    title_text::String;
    xticks::Tuple = ([0.0, 0.1, 0.2, 0.3, 0.4], 
                     ["0", "10", "20", "30", "40"]),
    annotationx::Float64 = 0.4, 
    annotationy::Float64 = -30.0,
    kwargs...)
  
    plt_dens = plot(size = (750, 500), 
                    title = title_text,
                    title_align= :left, 
                    bottom_margin = 12mm, 
                    showaxis = :x,
                    right_margin = 2mm,
                    y_ticks = nothing, 
                    xticks = xticks,
                    xtickfontsize = 12,
                    xlabel = "Percent",
                    xguidefontsize = 14,
                    fontfamily = :Verdana,
                    titlefontsize = 20;
                    kwargs...)
    
    for i in 1:length(results.parties)
        StatsPlots.density!(plt_dens, results.ξ_last[:, i], 
                            label = results.parties[i], 
                            fill = (0, .2, party_colours[results.parties[i]]),
                            lc = party_colours[results.parties[i]], 
                            lw = 2)
    end

    annotate!(plt_dens, annotationx, annotationy, 
              StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", 
              :lower, :right, 8, :grey))

end




function show_results(results::ElectionSummary)

    for i in 1:length(results.parties)
        println(results.parties[i], ": ", results.ξ[1:3, end, i])
    end
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

    out[:,1] = ξ_m[xi_days .== x,:]
    out[:,2] = ξ_ll[xi_days .== x,:]
    out[:,3] = ξ_uu[xi_days .== x,:]

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
    outcomes.RidingNumber = outcomes.Electoral_District_Number_Numéro_de_circonscription
    outcomes.Elected = outcomes.Elected_Candidate_Indicator_Indicateur_du_candidat_élu
    outcomes.Party = outcomes.Political_Affiliation_Name_English_Appartenance_politique_Anglais
    outcomes.Incumbent = outcomes.Incumbent_Indicator_Indicateur_Candidat_sortant
    outcomes.VoteCount = outcomes.Candidate_Poll_Votes_Count_Votes_du_candidat_pour_le_bureau
    outcomes = outcomes[:, outcomes_vars]

    for i ∈ 2:length(X)
        tmp = CSV.read(X[i], DataFrame; normalizenames = true)
        tmp.RidingNumber = tmp.Electoral_District_Number_Numéro_de_circonscription
        tmp.Elected = tmp.Elected_Candidate_Indicator_Indicateur_du_candidat_élu
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
    outcomes.Party[outcomes.Party .== "Bloc Québécois"] .= "BQ"
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