

# activate OntarioElection
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
updated_date = "February 19, 2022"
day_title = "February 19, 2022"
update_date = Date(2022, 02, 19)
value_date = Date(2022, 02, 19)
dateformat = DateFormat("y-m-d")


include("01_custom_functions.jl")


# scrape data from web. Use R, as rvest makes it easy

R"""
library(rvest)

#Opinion_polls

wiki <- read_html("https://en.wikipedia.org/wiki/2022_Ontario_general_election#Opinion_polls")
wiki_tables <- html_table(wiki, 
                          fill = TRUE, 
                          header = TRUE)
pre_polls <- wiki_tables[[8]]


wiki_2018 <- read_html("https://en.wikipedia.org/wiki/2018_Ontario_general_election#Opinion_polls")
wiki_tables_2018 <- html_table(wiki_2018, 
                          fill = TRUE, 
                          header = TRUE)
polls_2018 <- wiki_tables_2018[[72]]

"""



# bring into julia
pre_polls = @rget pre_polls
polls_2018 = @rget polls_2018



# Polling firms for subsetting
# Note: Need to keep an eye on 2021 campaign and add in any missing pollsters
# Should new pollsters show up in Wikipedia tables
polling_firms =  ["Abacus Data", "Leger", "Mainstreet Research", "Nanos Research",
                  "Campaign Research", "Innovative Research", "EKOS", "Angus Reid",
                  "Ipsos", "DART/Maru", "Forum Research", "Research Co.",
                  "Pollara", "Insights West", "Stratcom", "Counsel", "Delphi Polling",
                  "Earnscliffe/Leger"] 
parties = [:PC, :NDP, :Liberal, :Green, :Other]
colours = [:blue, :orange, :red, :green, :yellow]
parties_names = ["PC", "NDP", "Liberal", "Green", "Others"]

# clean pre 2022 election polls
pre_polls.Green = allowmissing(pre_polls.Green)
pre_polls.Green[pre_polls.Green .== "10[a]"] .= missing
dropmissing!(pre_polls, [:PC, :Green])

pre_polls.Polling_firm = pre_polls[:, "Polling firm"]
pre_polls = pre_polls[in(polling_firms).(pre_polls.Polling_firm), :]
pre_polls.PollDate = pre_polls[:, "Last dateof polling"]
pre_polls.SampleSize = pre_polls[:, "Sample size"]
pre_polls.SampleSize = clean_samplesize(pre_polls.SampleSize)
pre_polls.mode = clean_mode(pre_polls[:, "Polling type"])
pre_polls.PC = parse.(Float64, pre_polls.PC)
pre_polls.NDP = parse.(Float64, pre_polls.NDP)
pre_polls.Liberal = parse.(Float64, pre_polls.Liberal)
pre_polls.Green = passmissing(parse).(Float64, pre_polls.Green)
pre_polls.Other = ifelse.(pre_polls.Other .== "n/a", missing, pre_polls.Other)
pre_polls.Other = passmissing(parse).(Float64, pre_polls.Other)



# Clean 2018 polls
dropmissing!(polls_2018, :PC)
polls_2018.Polling_firm = polls_2018[:, "Polling organisation"]
polls_2018 = polls_2018[in(polling_firms).(polls_2018.Polling_firm), :]
polls_2018.PollDate = polls_2018[:, "Last date of polling"]
polls_2018.SampleSize = polls_2018[:, "Sample size"]
polls_2018.SampleSize = clean_samplesize(polls_2018.SampleSize)
polls_2018.mode = clean_mode(polls_2018[:, "Polling type"])
polls_2018.PC = parse.(Float64, polls_2018.PC)
polls_2018.NDP = parse.(Float64, polls_2018.NDP)
polls_2018.Liberal = parse.(Float64, polls_2018.Lib)
polls_2018.Gr[polls_2018.Gr .== ""] .= "–"
polls_2018.Green = ifelse.(polls_2018.Gr .== "–", missing, polls_2018.Gr)
polls_2018.Green = passmissing(parse).(Float64, polls_2018.Green)
polls_2018.Other = Vector{Union{Missing, Float64}}(missing, size(polls_2018, 1))

for i in 1:size(polls_2018, 1)
    m = sum(ismissing.(Vector(polls_2018[i, [:PC, :Liberal, :NDP, :Green]]))) == 0
    if m
        Other = sum(polls_2018[i, [:PC, :Liberal, :NDP, :Green]])
        polls_2018.Other[i] = coalesce(polls_2018.Other[i], (100 - Other))
    end
end


dropmissing!(polls_2018, :Other)
polls_2018 = polls_2018[polls_2018.Other .≥ 0, :]

# Combine polls into one DataFrame
subset_vars = ["Polling_firm", "PollDate", "PC", "NDP", "Liberal", "Green", "Other",
               "SampleSize", "mode"]
on_polls = vcat(pre_polls[:, subset_vars], polls_2018[:, subset_vars])


# drop missing polls because it makes my life easier
dropmissing!(on_polls)


# Dates
election_day_2014 = Date(2014, 06, 12)
election_day_2018 = Date(2018, 06, 17)
on_polls.PollDate = replace.(on_polls.PollDate, " " => "-")
on_polls.PollDate = replace.(on_polls.PollDate, "," => "")
on_polls.PollDate = Date.(on_polls.PollDate, "U-d-y")
on_polls.NumDays =  Dates.value.(on_polls.PollDate .- election_day_2014 .+ Dates.Day(1))


# Pollster id for indexing in Stan model
pollster_dict = Dict(key => idx for (idx, key) in enumerate(unique(on_polls.Polling_firm)))
on_polls.pollster_id = [pollster_dict[i] for i in on_polls.Polling_firm]
reverse_pollster = Dict(value => key for (key, value) in pollster_dict)


# mode id for indexing in Turing model
mode_dict = Dict(key => idx for (idx, key) in enumerate(unique(on_polls.mode)))
on_polls.mode_id = [mode_dict[i] for i in on_polls.mode]
reverse_mode= Dict(value => key for (key, value) in mode_dict)


# write CSV for polls
CSV.write("Data/ON_polls_2014_2022.csv", on_polls)



# Prep data for model
days_to_election_2018 = Dates.value(election_day_2018 - election_day_2014) + 1
N_days = Dates.value(update_date - election_day_2014) + 1
N_polls = size(on_polls, 1)
N_pollsters = length(unique(on_polls.pollster_id))
N_parties = length(parties)
N_modes = length(unique(on_polls.mode_id))
y_mat = Matrix(on_polls[:, parties]) ./ 100
y_mat_moe = Matrix(calc_moe.(y_mat, on_polls.SampleSize)) 
election_results_2014 = Vector([0.3125, 0.32375, 0.3865, 0.0484, 0.0151])
election_results_2018 = Vector([0.4050, 0.3356, 0.1959, 0.046, 0.0175]) 
poll_date = convert.(Int64, on_polls.NumDays)
poll_id = [1:size(on_polls, 1);]
pollster_id = on_polls.pollster_id
mode_id = Vector(on_polls.mode_id)


state_space_ON_ncp = "
data {

    // Ns
    int<lower=1> N_days;                     // number of days
    int<lower=1> N_parties;                  // number of parties
    int<lower=1> N_polls;                    // number of polls
    int<lower=1> N_modes;                    // number of survey modes
    int<lower=1> N_pollsters;                // number of pollsters
    
    // election results
    real xi_start[N_parties];                // value at starting election
    real xi_2018[N_parties];                 // value at final election
    int election_day_2018;                   // date of 2019 election
    
    // poll results
    matrix[N_polls, N_parties] y;            // actual values in polls for each party
    matrix[N_polls, N_parties] y_moe;        // margins of errors for each party based on srs
    
    // poll date + polster id
    int<lower=1> poll_date[N_polls];         // the number of days since starting election each poll was taken
    int pollster_id[N_polls];                // id for each pollster for each party
    
    // mode id
    int<lower=1> mode_id[N_polls];           // mode id for variance
    
  }
  
  
  parameters {
    
    matrix[N_days-2, N_parties] z_omega;                          // Matrix to hold std_normal errors for non-centered parameterization 
    vector<lower=0>[N_parties] omega;                             // innovation sd for each party
    cholesky_factor_corr[N_parties] L_Rho;                        // L_corr to account for correlated errors
    
    vector<lower=0>[N_parties] sigma_party;                       // party specific error
    matrix<lower=0>[N_pollsters, N_parties] sigma_pollster;       // to account for error per pollster other than sampling
    matrix<lower=0>[N_modes, N_parties] sigma_mode;               // to account for mode
    
    matrix[N_pollsters, N_parties] delta;                         // house effect for each pollster for each party
    
    //matrix[N_modes, N_parties] gamma;                           // uncomment for mode effects
    
  }
  
  transformed parameters {
    matrix[N_days-2, N_parties] Omega;                            // matrix to hold innovation sd per party
    matrix[N_days, N_parties] xi;                                 // underlying state of vote intention
    matrix<lower=0>[N_polls, N_parties] sigma;                    // total sd
    matrix[N_polls, N_parties] mu;                                // xi + delta
  
  
    Omega = z_omega * diag_pre_multiply(omega, L_Rho);            // Done as pre-multiplication because indexing was easier for me
    
  
    // set up non-centered paramterization for innovation sd
    for(j in 1:N_parties) {
      
      xi[1, j] = xi_start[j];
      xi[election_day_2018,j] = xi_2018[j];
        
      // from 2015 until 2019 election
      for (t in 2:(election_day_2018 - 1)) {
        
        xi[t, j] = xi[(t - 1), j] + Omega[(t - 1), j];
        
        }
        
      // post 2019 election
      for(tt in (election_day_2018 + 1):N_days) {
        
        xi[tt, j] = xi[(tt - 1), j] + Omega[(tt - 2), j];
        
      }
        
    }
      
    // build mu and sigma for likelihood  
    for(i in 1:N_polls){
      for(j in 1:N_parties){
        
        sigma[i, j] = sqrt(square(sigma_party[j]) + square(sigma_pollster[pollster_id[i], j]) + 
                           square(sigma_mode[mode_id[i], j]) + square(y_moe[i, j]));
        //mu[i, j] = xi[poll_date[i], j] + delta[pollster_id[i], j] + gamma[mode_id[i], j];    // uncomment for mode effects
        mu[i, j] = xi[poll_date[i], j] + delta[pollster_id[i], j];
  
        }
        
      }
      
      
  }
  
  
  model {
    
    // priors for innovation sd (omega and corr matrix)
    to_vector(z_omega) ~ normal(0, 1);
    omega ~ exponential(5);
    L_Rho ~ lkj_corr_cholesky(2);
  
    // priors for various sigmas
    sigma_party ~ exponential(5);
    to_vector(sigma_pollster) ~ exponential(5);
    to_vector(sigma_mode) ~ exponential(5);
   
    // prior for house effects and survey mode effects (if applicable) 
    to_vector(delta) ~ normal(0, 0.05);
    //to_vector(gamma) ~ normal(0, 0.05);                         // uncomment for mode effects
  
  
    // force small sd of xi on day before election day, to force 
    for(j in 1:N_parties){
  
      xi_2018[j] ~ normal(xi[election_day_2018-1,j] , 0.001); 
      
    }
  
    
    
     // Likelihood
    for(j in 1:N_parties){
      y[, j] ~ normal(mu[, j], sigma[, j]);
    }
    
  }
  "

data = Dict(
  
    "N_days" => N_days,
    "N_parties" => N_parties,
    "N_polls" => N_polls,
    "N_pollsters" => N_pollsters,
    "N_modes" => N_modes,
    
    "xi_start" => election_results_2014,
    "xi_2018" => election_results_2018,
    "election_day_2018" => days_to_election_2018,
    
    "y" => y_mat',
    "y_moe" => y_mat_moe',
    
    "poll_date" => poll_date,
    "pollster_id" => pollster_id,
    
    "mode_id" => mode_id

  )
 

tmpdir = joinpath(@__DIR__, "tmp")

sm = SampleModel("ss_ncp", state_space_ON_ncp, tmpdir)
rc = stan_sample(sm; data, num_chains = 4, seed = 1453124)


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
xi_days = election_day_2014 .+ Dates.Day.(1:N_days) .- Dates.Day(1)
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
ylims!(plt_trend, (0.0, 0.65))
for i in 1:length(colours)
    scatter!(plt_trend, on_polls.PollDate, 
             on_polls[:, parties[i]] / 100, 
             label = parties_names[i], 
             mc = colours[i])
    plot!(plt_trend, xi_days, 
          ξ_summary[1, :, i], 
          ribbon = (ξ_summary[4,:,i], 
                    ξ_summary[5,:,i]), 
          label = nothing, fc = colours[i], 
           lc = colours[i], lw = 2)
end

title!(plt_trend, "Estimated vote intention of Ontario voters:\n2014 to 2022", 
       title_align= :left, titlefontsize = 12)
annotate!(plt_trend, xi_days[end], -0.08, 
          StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", 
                          :lower, :right, 8, :grey))
yticks!(plt_trend, [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6], 
             ["0", "10", "20", "30", "40", "50", "60"])

savefig(plt_trend, "Images/Ontario/ON_vote_intention_2014_2022.png")


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

    if i == 3
        xaxis!(x_guide = "Percent")
    end

    push!(plt_house, plt_tmp)
end

annotate!(plt_house[5], .1, -1.25, 
          StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", 
          :lower, :right, 8, :grey))

title = plot(title = "House effects in Ontario: 2014 to 2022", 
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
                         layout = @layout([A{0.01h}; [B C D E F]]),
                         size = (1100, 750))


savefig(plt_house_effects, "Images/Ontario/ON_house_effects_pollsters_2014_2022.png")


plt_dens = plot(size = (750, 500), 
                title = "Estimated vote intention for Ontario: $day_title",
                title_align= :left, bottom_margin = 12mm, showaxis = :x,
                y_ticks = nothing, fontfamily = :Verdana)
for i in 1:(N_parties)
    StatsPlots.density!(plt_dens, ξ[:, end, i], 
                        label = parties_names[i], fill = (0, .2, colours[i]),
                        lc = colours[i], lw = 2)
end

ann_loc = maximum(ξ) - 0.1
annotate!(plt_dens, ann_loc, -8, 
          StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", 
                          :lower, :right, 8, :grey))
xticks!(plt_dens, [0.1, 0.2, 0.3, 0.4, 0.5, 0.6], 
        ["10", "20", "30", "40", "50", "60"])
xlabel!(plt_dens, "Percent")

savefig(plt_dens, "Images/Ontario/ON_vote_intention_2022.png")



plt_trend_180 = plot(size = (750, 500), 
                    legend = :topright, fontfamily = :Verdana, 
                    left_margin = 10mm, bottom_margin = 15mm, 
                    ylabel = "Vote intention (%)")
ylims!(plt_trend_180, (0.0, 0.65))
for i in 1:length(colours)
    scatter!(plt_trend_180, on_polls.PollDate[on_polls.PollDate .≥ day_180_before], 
             on_polls[on_polls.PollDate .≥ day_180_before, parties[i]] / 100, 
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

title!(plt_trend_180, "Estimated vote intention of Ontario voters:\nLast 180 days", 
       title_align= :left, titlefontsize = 12)
annotate!(plt_trend_180, xi_days[end], -0.08, 
          StatsPlots.text("Source: Wikipedia. Analysis by sjwild.github.io\nUpdated $updated_date", 
                          :lower, :right, 8, :grey))
yticks!(plt_trend_180, [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6], 
             ["0", "10", "20", "30", "40", "50", "60"])

savefig(plt_trend_180, "Images/Ontario/ON_vote_intention_last_180_days.png")

[ξ_summary[:, end, i] for i in 1:size(ξ, 3)]



