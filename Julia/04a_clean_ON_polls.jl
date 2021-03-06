
parties_ON = [:PC, :NDP, :Liberal, :Green, :Other]
colours_ON = [:blue, :orange, :red, :green, :yellow]
parties_names_ON = ["PC", "NDP", "Liberal", "Green", "Others"]


# clean pre 2022 election polls
allowmissing!(campaign_polls_ON)
#campaign_polls_ON.Green[campaign_polls_ON.Green .== "10[a]"] .= missing
dropmissing!(campaign_polls_ON, [:PC, :Green])

campaign_polls_ON.Polling_firm = campaign_polls_ON[:, "Polling firm"]
campaign_polls_ON.Polling_firm[contains.(campaign_polls_ON.Polling_firm, "Mainstreet")] .= "Mainstreet Research"
campaign_polls_ON = campaign_polls_ON[in(polling_firms).(campaign_polls_ON.Polling_firm), :]
campaign_polls_ON.PollDate = campaign_polls_ON[:, "Last dateof polling"]
campaign_polls_ON.SampleSize = campaign_polls_ON[:, "Sample size"]
campaign_polls_ON.SampleSize = clean_samplesize(campaign_polls_ON.SampleSize)
campaign_polls_ON.mode = clean_mode(campaign_polls_ON[:, "Polling type"])
campaign_polls_ON.PC = clean_mean(campaign_polls_ON.PC)
campaign_polls_ON.NDP = clean_mean(campaign_polls_ON.NDP)
campaign_polls_ON.Liberal = clean_mean(campaign_polls_ON.Liberal)
campaign_polls_ON.Green = clean_mean(campaign_polls_ON.Green)
#campaign_polls_ON.Other = replace.(campaign_polls_ON.Other, "2[a]" => "2",
#                                                            "5[b]" => "5")
campaign_polls_ON.Other = clean_mean(campaign_polls_ON.Other)




# clean pre 2022 election polls
allowmissing!(pre_polls_ON)
#pre_polls_ON.Green[pre_polls_ON.Green .== "10[a]"] .= missing
dropmissing!(pre_polls_ON, [:PC, :Green])

pre_polls_ON.Polling_firm = pre_polls_ON[:, "Polling firm"]
pre_polls_ON = pre_polls_ON[in(polling_firms).(pre_polls_ON.Polling_firm), :]
pre_polls_ON.PollDate = pre_polls_ON[:, "Last dateof polling"]
pre_polls_ON.SampleSize = pre_polls_ON[:, "Sample size"]
pre_polls_ON.SampleSize = clean_samplesize(pre_polls_ON.SampleSize)
pre_polls_ON.mode = clean_mode(pre_polls_ON[:, "Polling type"])
pre_polls_ON.PC = clean_mean(pre_polls_ON.PC)
pre_polls_ON.NDP = clean_mean(pre_polls_ON.NDP)
pre_polls_ON.Liberal = clean_mean(pre_polls_ON.Liberal)
pre_polls_ON.Green = clean_mean(pre_polls_ON.Green)
pre_polls_ON.Other = replace.(pre_polls_ON.Other, "2[a]" => "2",
                                            "5[b]" => "5")
pre_polls_ON.Other = clean_mean(pre_polls_ON.Other)



# Clean 2018 polls
dropmissing!(polls_2018_ON, :PC)
polls_2018_ON.Polling_firm = polls_2018_ON[:, "Polling organisation"]
polls_2018_ON = polls_2018_ON[in(polling_firms).(polls_2018_ON.Polling_firm), :]
polls_2018_ON.PollDate = polls_2018_ON[:, "Last date of polling"]
polls_2018_ON.SampleSize = polls_2018_ON[:, "Sample size"]
polls_2018_ON.SampleSize = clean_samplesize(polls_2018_ON.SampleSize)
polls_2018_ON.mode = clean_mode(polls_2018_ON[:, "Polling type"])
polls_2018_ON.PC = clean_mean(polls_2018_ON.PC)
polls_2018_ON.NDP = clean_mean(polls_2018_ON.NDP)
polls_2018_ON.Liberal = clean_mean(polls_2018_ON.Lib)
polls_2018_ON.Gr[polls_2018_ON.Gr .== ""] .= "???"
polls_2018_ON.Green = clean_mean(polls_2018_ON.Gr)
polls_2018_ON.Other = Vector{Union{Missing, Float64}}(missing, size(polls_2018_ON, 1))

for i in 1:size(polls_2018_ON, 1)
    m = sum(ismissing.(Vector(polls_2018_ON[i, [:PC, :Liberal, :NDP, :Green]]))) == 0
    if m
        Other = sum(polls_2018_ON[i, [:PC, :Liberal, :NDP, :Green]])
        polls_2018_ON.Other[i] = coalesce(polls_2018_ON.Other[i], (100 - Other))
    end
end


dropmissing!(polls_2018_ON, :Other)
polls_2018_ON = polls_2018_ON[polls_2018_ON.Other .??? 0, :]

# Combine polls into one DataFrame
subset_vars_ON = ["Polling_firm", "PollDate", "PC", "NDP", "Liberal", "Green", "Other",
                  "SampleSize", "mode"]
polls_ON = vcat(campaign_polls_ON[:, subset_vars_ON],
                pre_polls_ON[:, subset_vars_ON], 
                polls_2018_ON[:, subset_vars_ON])


# drop missing polls because it makes my life easier
dropmissing!(polls_ON)


# Dates
election_day_2014_ON = Date(2014, 06, 12)
election_day_2018_ON = Date(2018, 06, 17)
polls_ON.PollDate = replace.(polls_ON.PollDate, " " => "-")
polls_ON.PollDate = replace.(polls_ON.PollDate, "," => "")
polls_ON.PollDate = Date.(polls_ON.PollDate, "U-d-y")
polls_ON.NumDays =  Dates.value.(polls_ON.PollDate .- election_day_2014_ON .+ Dates.Day(1))

# Convert polls percentages to proprotions
polls_ON[:, parties_ON] = polls_ON[:, parties_ON] ./ 100


# Pollster id for indexing in Stan model
pollster_dict_ON = Dict(key => idx for (idx, key) in enumerate(unique(polls_ON.Polling_firm)))
polls_ON.pollster_id = [pollster_dict_ON[i] for i in polls_ON.Polling_firm]
reverse_pollster_ON = Dict(value => key for (key, value) in pollster_dict_ON)


# mode id for indexing in Turing model
mode_dict_ON = Dict(key => idx for (idx, key) in enumerate(unique(polls_ON.mode)))
polls_ON.mode_id = [mode_dict_ON[i] for i in polls_ON.mode]
reverse_mode_ON = Dict(value => key for (key, value) in mode_dict_ON)


# write CSV for polls
CSV.write("Data/ON_polls_2014_2022.csv", polls_ON)