
parties_ON = [:PC, :NDP, :Liberal, :Green, :Other]
colours_ON = [:blue, :orange, :red, :green, :yellow]
parties_names_ON = ["PC", "NDP", "Liberal", "Green", "Others"]


# clean pre 2022 election polls
allowmissing!(campaign_polls_2022_ON)
#campaign_polls_2022_ON.Green[campaign_polls_2022_ON.Green .== "10[a]"] .= missing
dropmissing!(campaign_polls_2022_ON, [:PC, :Green])

campaign_polls_2022_ON.Polling_firm = campaign_polls_2022_ON[:, "Polling firm"]
campaign_polls_2022_ON.Polling_firm[contains.(campaign_polls_2022_ON.Polling_firm, "Mainstreet")] .= "Mainstreet Research"
campaign_polls_2022_ON = campaign_polls_2022_ON[in(polling_firms).(campaign_polls_2022_ON.Polling_firm), :]
campaign_polls_2022_ON.PollDate = campaign_polls_2022_ON[:, "Last dateof polling"]
campaign_polls_2022_ON.SampleSize = campaign_polls_2022_ON[:, "Sample size"]
campaign_polls_2022_ON.SampleSize = clean_samplesize(campaign_polls_2022_ON.SampleSize)
campaign_polls_2022_ON.mode = clean_mode(campaign_polls_2022_ON[:, "Polling type"])
campaign_polls_2022_ON.PC = clean_mean(campaign_polls_2022_ON.PC)
campaign_polls_2022_ON.NDP = clean_mean(campaign_polls_2022_ON.NDP)
campaign_polls_2022_ON.Liberal = clean_mean(campaign_polls_2022_ON.Liberal)
campaign_polls_2022_ON.Green = clean_mean(campaign_polls_2022_ON.Green)
campaign_polls_2022_ON.Other = replace.(campaign_polls_2022_ON.Other, "2[a]" => "2",
                                                            "5[b]" => "5")
campaign_polls_2022_ON.Other = clean_mean(campaign_polls_2022_ON.Other)




# clean pre 2022 election polls
allowmissing!(pre_polls_2022_ON)
#pre_polls_2022_ON.Green[pre_polls_2022_ON.Green .== "10[a]"] .= missing
dropmissing!(pre_polls_2022_ON, [:PC, :Green])

pre_polls_2022_ON.Polling_firm = pre_polls_2022_ON[:, "Polling firm"]
pre_polls_2022_ON = pre_polls_2022_ON[in(polling_firms).(pre_polls_2022_ON.Polling_firm), :]
pre_polls_2022_ON.PollDate = pre_polls_2022_ON[:, "Last dateof polling"]
pre_polls_2022_ON.SampleSize = pre_polls_2022_ON[:, "Sample size"]
pre_polls_2022_ON.SampleSize = clean_samplesize(pre_polls_2022_ON.SampleSize)
pre_polls_2022_ON.mode = clean_mode(pre_polls_2022_ON[:, "Polling type"])
pre_polls_2022_ON.PC = clean_mean(pre_polls_2022_ON.PC)
pre_polls_2022_ON.NDP = clean_mean(pre_polls_2022_ON.NDP)
pre_polls_2022_ON.Liberal = clean_mean(pre_polls_2022_ON.Liberal)
pre_polls_2022_ON.Green = clean_mean(pre_polls_2022_ON.Green)
pre_polls_2022_ON.Other = replace.(pre_polls_2022_ON.Other, "2[a]" => "2",
                                            "5[b]" => "5")
pre_polls_2022_ON.Other = clean_mean(pre_polls_2022_ON.Other)



# Clean 2018 polls
dropmissing!(polls_ON, :PC)
polls_ON.Polling_firm = polls_ON[:, "Polling firm"]
polls_ON = polls_ON[in(polling_firms).(polls_ON.Polling_firm), :]
polls_ON.PollDate = polls_ON[:, "Last dateof polling"]
polls_ON.SampleSize = polls_ON[:, "Sample size"]
polls_ON.SampleSize = clean_samplesize(polls_ON.SampleSize)
polls_ON.mode = clean_mode(polls_ON[:, "Polling type"])
polls_ON.PC = clean_mean(polls_ON.PC)
polls_ON.NDP = clean_mean(polls_ON.NDP)
polls_ON.Liberal = clean_mean(polls_ON.Liberal)
polls_ON.Green = clean_mean(polls_ON.Green)
polls_ON.Other = Vector{Union{Missing, Float64}}(missing, size(polls_ON, 1))

for i in 1:size(polls_ON, 1)
    m = sum(ismissing.(Vector(polls_ON[i, [:PC, :Liberal, :NDP, :Green]]))) == 0
    if m
        Other = sum(polls_ON[i, [:PC, :Liberal, :NDP, :Green]])
        polls_ON.Other[i] = coalesce(polls_ON.Other[i], (100 - Other))
    end
end


dropmissing!(polls_ON, :Other)
polls_ON = polls_ON[polls_ON.Other .≥ 0, :]

# Combine polls into one DataFrame
subset_vars_ON = ["Polling_firm", "PollDate", "PC", "NDP", "Liberal", "Green", "Other",
                  "SampleSize", "mode"]
polls_ON = vcat(campaign_polls_2022_ON[:, subset_vars_ON],
                pre_polls_2022_ON[:, subset_vars_ON], 
                polls_ON[:, subset_vars_ON])

for i in 1:size(polls_ON, 1)
    m = sum(ismissing.(Vector(polls_ON[i, [:PC, :Liberal, :NDP, :Green]]))) == 0
    if m
        Other = sum(polls_ON[i, [:PC, :Liberal, :NDP, :Green]])
        polls_ON.Other[i] = coalesce(polls_ON.Other[i], (100 - Other))
    end
end
# drop missing polls because it makes my life easier
dropmissing!(polls_ON)


# Dates
election_day_2022_ON = Date(2022, 06, 02)
election_day_2018_ON = Date(2018, 06, 17)
polls_ON.PollDate = replace.(polls_ON.PollDate, " " => "-")
polls_ON.PollDate = replace.(polls_ON.PollDate, "," => "")
polls_ON.PollDate = Date.(polls_ON.PollDate, "U-d-y")
polls_ON.NumDays =  Dates.value.(polls_ON.PollDate .- election_day_2018_ON .+ Dates.Day(1))

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