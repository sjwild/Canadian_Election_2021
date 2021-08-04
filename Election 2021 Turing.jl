using Plots, StatsPlots
using DataFrames, CSV
using Turing, ReverseDiff, Memoization
using StatsFuns
using Dates
using JLD
using LinearAlgebra
using Measures
using Random 

# Helper functions
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



# Dates
dateformat = DateFormat("y-m-d")
election_day_2015 = Date(2015, 10, 19)
election_day_2019 = Date(2019, 10, 21)
election_day_2021 = Date(2021, 09, 30)

# Load pre-cleaned polls
can_polls = CSV.read("can_polls2.csv", DataFrame; missingstring ="NA")
can_polls = dropmissing(can_polls, [:BQ, :GPC])
can_polls[:, :Other] = [1 - sum(can_polls[i,[:LPC, :CPC, :NDP, :BQ, :GPC]]) for i in 1:size(can_polls, 1)]
can_polls = can_polls[can_polls.Other .> 0.0000, :]

# Pollster id
pollster_dict = Dict(key => idx for (idx, key) in enumerate(unique(can_polls.Polling_firm)))
can_polls.pollster_id = [pollster_dict[i] for i in can_polls.Polling_firm]
reverse_pollster = Dict(value => key for (key, value) in pollster_dict)


# Dates for plotting
can_polls.poll_date = election_day_2015 .+ Dates.Day.(can_polls.NumDays) .- Dates.Day(1)


# Prep data for model
parties = ["LPC", "CPC", "NDP", "BQ", "GPC"]
#parties = ["LPC", "CPC", "NDP", "BQ", "GPC", "Other"]
election_2019 = Dates.value(election_day_2019 - election_day_2015) + 1
N_days = Dates.value(election_day_2021 - election_day_2015) + 1
N_polls = size(can_polls, 1)
N_pollsters = length(unique(can_polls.pollster_id))
N_parties = length(parties)
N_modes = length(unique(can_polls.mode_id))
y_mat = Matrix(can_polls[:, parties])
y_mat_moe = Matrix(calc_moe.(y_mat, can_polls.SampleSize))
start_election = Vector([.395, .319, .197, 0.047, 0.034])
end_election = Vector([.331, .343, 0.16, 0.076, 0.065]) 
#start_election = Vector([.395, .319, .197, 0.047, 0.034, .008])
#end_election = Vector([.331, .343, 0.16, 0.076, 0.065, 0.25]) 
poll_date = convert.(Int64, can_polls.NumDays)
poll_id = [1:size(can_polls, 1);]
pollster_id = can_polls.pollster_id
mode_id = Vector(can_polls.mode_id)


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
    ξ = Matrix{T}(undef, (N_days, N_parties))
    σ = Matrix{T}(undef, (N_polls, N_parties))
    μ = Matrix{T}(undef, (N_polls, N_parties))
    

    # Omega and Rho for non-centered parameterization
    z_ω ~ filldist(Normal(0, 1), N_parties, (N_days - 2))
    ω ~ filldist(truncated(Normal(0, 0.005), 0, Inf), N_parties)
    ρ ~ LKJ(N_parties, 2.0)


    # House effects
    δ ~ filldist(Normal(0, 0.05), N_pollsters, N_parties)
    

    # sigmas for party and pollster-by-party effects    
    σ_party ~ filldist(Exponential(1/20), N_parties)
    σ_pollster ~ filldist(Exponential(1/20), N_pollsters, N_parties)
    σ_mode ~ filldist(Exponential(1/20), N_modes, N_parties)


    # Transform parameters
    Ω = LinearAlgebra.diagm(ω) * ρ * LinearAlgebra.diagm(ω) * z_ω

    ξ[1, :] = start_election
    ξ[election_2019, :] = end_election    
    
    # for loop to fill in random walk priors
    for t in 2:(election_2019 - 1)
        for j in 1:N_parties
            ξ[t,j] = ξ[t-1, j] + Ω[j, t-1]
        end
    end

    for tt in (election_2019 + 1):(N_days)
        for j in 1:N_parties
            ξ[tt,j] = ξ[tt - 1, j] + Ω[j, tt - 2]
        end
    end   

    # for loops to run model
    for i in 1:N_polls
        for j in 1:N_parties
            σ[i, j] = sqrt(σ_party[j]^2 + σ_pollster[pollster_id[i], j]^2 + σ_mode[mode_id[i], j]^2 + y_moe[i, j]^2)
            #σ[i, j] = σ_party[j] + σ_pollster[pollster_id[i], j] + σ_mode[mode_id[i], j] + y_moe[i, j] # error also occurs if I use this line
            μ[i, j] = ξ[poll_date[i], j] + δ[pollster_id[i], j]
            y[i, j] ~ Normal(μ[i, j], σ[i, j])
        end
    end 

    for j in 1:N_parties
        end_election[j] ~ Normal(ξ[election_2019 - 1, j], 0.001)
    end

    return ξ
    #return ξ, σ, μ

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



# Set iters and ids
n_adapt = 750
n_iter = 750
n_chains = 4


#Random.seed!(4329)
Random.seed!(53103)
Turing.setadbackend(:reversediff)
Turing.setrdcache(true)
chns_election = sample(mod_election, NUTS(n_adapt, 0.8; max_depth = 12), MCMCThreads(), n_iter, n_chains)


# Save chains
save("turing_model_can_election.jld", "chns_election", chns_election)


# Generate ξ
ξ_gq = generated_quantities(mod_election, chns_election)

rs = n_iter * n_chains
ξ = Array{Float64}(undef, (rs, N_days, N_parties))

for i in 1:rs
    tmp = collect(ξ_gq[i])
    for j in 1:N_days
        for p in 1:N_parties
        ξ[i, j, p] = tmp[j, p]
        end
    end
end

ξ_ll = Matrix{Float64}(undef, (N_days, N_parties))
ξ_m = Matrix{Float64}(undef, (N_days, N_parties))
ξ_uu = Matrix{Float64}(undef, (N_days, N_parties))

for j in 1:N_days
    for p in 1:N_parties
        ξ_ll[j,p] = quantile(ξ[: ,j, p], 0.025)
        ξ_m[j,p] = quantile(ξ[: ,j, p], 0.50)
        ξ_uu[j,p] = quantile(ξ[: ,j, p], 0.975)
    end
end


# Plot ξ and polls over time
num_days = election_day_2015 .+ Dates.Day.(1:N_days) .- Dates.Day(1)
colours = [:red, :blue, :orange, :cyan, :green]
#colours = [:red, :blue, :orange, :cyan, :green, :purple]


plt = plot(size = (750, 500), legend = :topright, fontfamily = :Courier, left_margin = 10mm, bottom_margin = 15mm, ylabel = "Vote intention (%)")
ylims!(plt, (0.0, 0.6))
for i in 1:length(colours)
    scatter!(plt, can_polls.poll_date, can_polls[:, parties[i]], label = parties[i], mc = colours[i])
    plot!(plt, num_days, ξ_m[:,i], ribbon = (ξ_m[:,i] - ξ_ll[:,i], ξ_uu[:,i] - ξ_m[:,i]), 
          label = nothing, fc = colours[i], lc = colours[i], lw = 2)
end

title!(plt, "Estimated vote intention of Canadian voters:\n2015 to 2021", title_align= :left, titlefontsize = 12)
annotate!(plt, num_days[end], -0.08, StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated Aug. 2, 2021", :lower, :right, 8, :grey))
yticks!(plt, [0.0, 0.1, 0.2, 0.3, 0.4, 0.5], 
             ["0", "10", "20", "30", "40", "50"])

savefig(plt, "can_vote_intention_2015_2021.png")





# Plot house effects
parties_list = repeat(parties, inner = N_pollsters)
δ_ll, δ_m, δ_uu = extract_params(chns_election, "δ")
pollsters = [reverse_pollster[i] for i in 1:maximum(can_polls.pollster_id)]



plt_house = []
for i in 1:length(parties)
    plt_tmp = plot(legend = false, title = parties[i], title_align = :left, xlims = (-0.1, .1),
                   fontfamily = :Courier, 
                   bottom_margin = 15mm,
                   left_margin = 4mm)
    Plots.scatter!(plt_tmp, (δ_m[parties_list .== parties[i]], pollsters), xerror = (δ_m[parties_list .== parties[i]] - δ_ll[parties_list .== parties[i]], δ_uu[parties_list .== parties[i]] - δ_m[parties_list .== parties[i]]),
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
          StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated Aug. 2, 2021", 
          :lower, :right, 8, :grey))

title = plot(title = "House effects: 2015 to 2021", titlefontsize = 16,
             titlefontfamily = :Courier,
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


savefig(plt_house_effects, "house_effects_pollsters.png")



# Plt 2019 to election day
plt_2019 = plot(size = (750, 500), legend = :topright, fontfamily = :Courier, left_margin = 10mm, bottom_margin = 15mm, ylabel = "Vote intention (%)")
ylims!(plt_2019, (0.0, 0.6))
for i in 1:length(colours)
    scatter!(plt_2019, can_polls.poll_date[can_polls.poll_date .≥ Date(2019, 10, 21)], 
             can_polls[can_polls.poll_date .≥ Date(2019, 10, 21), parties[i]], 
             label = parties[i], mc = colours[i])
    plot!(plt_2019, num_days[num_days .≥ Date(2019, 10, 21)], 
          ξ_m[num_days .≥ Date(2019, 10, 21), i], 
          ribbon = (ξ_m[num_days .≥ Date(2019, 10, 21), i] - ξ_ll[num_days .≥ Date(2019, 10, 21), i], 
                    ξ_uu[num_days .≥ Date(2019, 10, 21), i] - ξ_m[num_days .≥ Date(2019, 10, 21), i]), 
                    label = nothing, fc = colours[i], lc = colours[i], lw = 2)
end

title!(plt_2019, "Estimated vote intention of Canadian voters:\n2019 to 2021", title_align= :left, titlefontsize = 12)
annotate!(plt_2019, num_days[end], -0.08, StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated Aug. 2, 2021", :lower, :right, 8, :grey))
yticks!(plt_2019, [0.0, 0.1, 0.2, 0.3, 0.4, 0.5], 
             ["0", "10", "20", "30", "40", "50"])

savefig(plt_2019, "can_vote_intention_2019_2021.png")