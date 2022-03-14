

# Prep data for Stan model
data_fed = prep_stan_data(parties_fed,
                          election_day_2019_fed,
                          election_day_2021_fed,
                          update_date,
                          Vector{Float64}([.331, .343, 0.16, 0.076, 0.065, 0.016, 0.009]),
                          Vector{Float64}([.326, .337, 0.178, 0.076, 0.023, 0.049, 0.011]),
                          polls_fed) 

# Load model
state_space_ncp = read("Stan/state_space_pooling_polls_ncp.stan", String)

tmpdir = joinpath(@__DIR__, "tmp")

sm_fed = SampleModel("ss_ncp", state_space_ncp, tmpdir)
rc_fed = stan_sample(sm_fed; 
                     data = data_fed, 
                     num_samples = 750,
                     num_warmups = 750,
                     num_chains = 4, 
                     seed = 943129384)

fed_results = summarize_data(rc_fed,
                             sm_fed,
                             parties_fed,
                             maximum(polls_fed.pollster_id),
                             reverse_pollster_fed,
                             election_day_2019_fed,
                             update_date)
