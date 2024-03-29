


# Prep data for Stan model
data_ON = prep_stan_data(parties_ON,
                         election_day_2018_ON,
                         election_day_2022_ON,
                         update_date,
                         Vector([0.4050, 0.3356, 0.1959, 0.046, 0.0175]),
                         Vector([0.4082, 0.2374, 0.2382, 0.060, 0.0562]),
                        polls_ON) 


# Load model
state_space_ncp = read("Stan/state_space_pooling_polls_ncp.stan", String)

tmpdir = joinpath(@__DIR__, "tmp")

sm_ON = SampleModel("ss_ncp_on", state_space_ncp, tmpdir)
rc_ON = stan_sample(sm_ON; 
                    data = data_ON, 
                    num_samples = 1000,
                    num_warmups = 1000,
                    max_depth = 12,
                    num_chains = 4, 
                    seed = 943129384)

diagnose(sm_ON)

ON_results = summarize_data(rc_ON,
                            sm_ON,
                            parties_ON,
                            maximum(polls_ON.pollster_id),
                            reverse_pollster_ON,
                            election_day_2018_ON,
                            update_date)
