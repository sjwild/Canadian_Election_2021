
parties_fed = [:LPC, :CPC, :NDP, :BQ, :GPC, :PPC]
colours_fed = [:red, :blue, :orange, :cyan, :green, :purple, :yellow]
parties_names_fed = ["LPC", "CPC", "NDP", "BQ", "GPC", "PPC", "Other"]


# clean 2021+ pre-campaign polls
dropmissing!(pre_polls_fed, :LPC)
pre_polls_fed.Polling_firm = pre_polls_fed[:, "Polling firm"]
pre_polls_fed = pre_polls_fed[in(polling_firms).(pre_polls_fed.Polling_firm), :]
pre_polls_fed.PollDate = pre_polls_fed[:, "Last dateof polling[a]"]
pre_polls_fed.Others = pre_polls_fed[:, "Others[b]"]
pre_polls_fed.SampleSize = pre_polls_fed[:, "Samplesize[d]"]
pre_polls_fed.SampleSize = clean_samplesize(pre_polls_fed.SampleSize)
pre_polls_fed.mode = clean_mode(pre_polls_fed[:, "Polling method[e]"])
pre_polls_fed.LPC = clean_mean(pre_polls_fed.LPC)
pre_polls_fed.CPC = clean_mean(pre_polls_fed.CPC)
pre_polls_fed.NDP = clean_mean(pre_polls_fed.NDP)
pre_polls_fed.BQ = clean_mean(pre_polls_fed.BQ)
pre_polls_fed.PPC = clean_mean(pre_polls_fed.PPC)
pre_polls_fed.GPC = clean_mean(pre_polls_fed.GPC)



# Clean 2021 pre-campaign polls
dropmissing!(pre_2021_polls_fed, :LPC)
pre_2021_polls_fed.Polling_firm = pre_2021_polls_fed[:, "Polling firm"]
pre_2021_polls_fed = pre_2021_polls_fed[in(polling_firms).(pre_2021_polls_fed.Polling_firm), :]
pre_2021_polls_fed.PollDate = pre_2021_polls_fed[:, "Last dateof polling[a]"]
pre_2021_polls_fed.SampleSize = pre_2021_polls_fed[:, "Samplesize[c]"]
pre_2021_polls_fed.SampleSize = clean_samplesize(pre_2021_polls_fed.SampleSize)
pre_2021_polls_fed.mode = clean_mode(pre_2021_polls_fed[:, "Polling method[d]"])
pre_2021_polls_fed.LPC = clean_mean(pre_2021_polls_fed.LPC)
pre_2021_polls_fed.CPC = clean_mean(pre_2021_polls_fed.CPC)
pre_2021_polls_fed.NDP = clean_mean(pre_2021_polls_fed.NDP)
pre_2021_polls_fed.BQ = clean_mean(pre_2021_polls_fed.BQ)
pre_2021_polls_fed.GPC = clean_mean(pre_2021_polls_fed.GPC)
pre_2021_polls_fed.PPC = clean_mean(pre_2021_polls_fed.PPC)



# Clean 2021 pre-campaign polls
dropmissing!(campaign_2021_polls_fed, :LPC)
campaign_2021_polls_fed.Polling_firm = campaign_2021_polls_fed[:, "Polling firm"]
campaign_2021_polls_fed = campaign_2021_polls_fed[in(polling_firms).(campaign_2021_polls_fed.Polling_firm), :]
campaign_2021_polls_fed.PollDate = campaign_2021_polls_fed[:, "Last dateof polling[a]"]
campaign_2021_polls_fed.SampleSize = campaign_2021_polls_fed[:, "Samplesize[c]"]
campaign_2021_polls_fed.SampleSize = clean_samplesize(campaign_2021_polls_fed.SampleSize)
campaign_2021_polls_fed.mode = clean_mode(campaign_2021_polls_fed[:, "Polling method[d]"])
campaign_2021_polls_fed.LPC = clean_mean(campaign_2021_polls_fed.LPC)
campaign_2021_polls_fed.CPC = clean_mean(campaign_2021_polls_fed.CPC)
campaign_2021_polls_fed.NDP = clean_mean(campaign_2021_polls_fed.NDP)
campaign_2021_polls_fed.BQ = clean_mean(campaign_2021_polls_fed.BQ)
campaign_2021_polls_fed.GPC = clean_mean(campaign_2021_polls_fed.GPC)
campaign_2021_polls_fed.PPC = clean_mean(campaign_2021_polls_fed.PPC)


# Combine polls into one DataFrame
subset_vars_fed = ["Polling_firm", "PollDate", "LPC", "CPC", "NDP", "BQ", "GPC", "PPC",
                   "SampleSize", "mode"]
polls_fed = vcat(pre_polls_fed[:, subset_vars_fed], 
                 pre_2021_polls_fed[:, subset_vars_fed], 
                 campaign_2021_polls_fed[:, subset_vars_fed])


# drop missing polls because it makes my life easier
dropmissing!(polls_fed)


# Dates
election_day_2019_fed = Date(2019, 10, 21)
election_day_2021_fed = Date(2021, 09, 20)
polls_fed.PollDate = replace.(polls_fed.PollDate, " " => "-")
polls_fed.PollDate = replace.(polls_fed.PollDate, "," => "")
polls_fed.PollDate = Date.(polls_fed.PollDate, "U-d-y")
polls_fed.NumDays =  Dates.value.(polls_fed.PollDate .- election_day_2019_fed .+ Dates.Day(1))


# Convert poll percentages to proportions
polls_fed[:, parties_fed] = polls_fed[:, parties_fed] ./ 100
polls_fed[:, :Other] = [1 - sum(polls_fed[i, parties_fed]) for i in 1:size(polls_fed, 1)]



# Subset polls to remove :Others with a value of 0
# If included, these polls will result in overestimation of support for other parties
# Reason: A poll with 1000 respondents should reasonably have about 20 people voting "Other"
polls_fed = polls_fed[polls_fed.Other .> 0.0000, :]


# Pollster id for indexing in Turing model
pollster_dict_fed = Dict(key => idx for (idx, key) in enumerate(unique(polls_fed.Polling_firm)))
polls_fed.pollster_id = [pollster_dict_fed[i] for i in polls_fed.Polling_firm]
reverse_pollster_fed = Dict(value => key for (key, value) in pollster_dict_fed)


# mode id for indexing in Turing model
mode_dict_fed = Dict(key => idx for (idx, key) in enumerate(unique(polls_fed.mode)))
polls_fed.mode_id = [mode_dict_fed[i] for i in polls_fed.mode]
reverse_mode_fed = Dict(value => key for (key, value) in mode_dict_fed)


# write CSV for polls
CSV.write("Data/fed_polls_after_2021.csv", polls_fed)