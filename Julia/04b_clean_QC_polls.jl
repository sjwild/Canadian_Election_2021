

# Polling firms for subsetting
# Note: Need to keep an eye on 2021 campaign and add in any missing pollsters
# Should new pollsters show up in Wikipedia tables

parties_QC = [:CAQ, :QLP, :PQ, :QS, :Other]

# clean pre 2022 election polls
allowmissing!(pre_polls_QC)

pre_polls_QC.Polling_firm = pre_polls_QC[:, "Polling organisation"]
pre_polls_QC = pre_polls_QC[in(polling_firms).(pre_polls_QC.Polling_firm), :]
pre_polls_QC.PollDate = pre_polls_QC[:, "Last date of polling"]
pre_polls_QC.SampleSize = pre_polls_QC[:, "Sample size"]
pre_polls_QC.SampleSize[pre_polls_QC.SampleSize .== "1,407–71"] .= "1,407"
pre_polls_QC.SampleSize = clean_samplesize(pre_polls_QC.SampleSize)
pre_polls_QC.CAQ = clean_mean(pre_polls_QC.CAQ)
pre_polls_QC.QLP = clean_mean(pre_polls_QC.Liberal)
pre_polls_QC.PQ = clean_mean(pre_polls_QC.PQ)
pre_polls_QC.QS = clean_mean(pre_polls_QC.QS)
pre_polls_QC.Other = 100 .- [sum(pre_polls_QC[i, [:CAQ, :QLP, :PQ, :QS]]) for i in 1:size(pre_polls_QC, 1)]



# Clean 2018 polls
allowmissing!(polls_2018_QC)
dropmissing!(polls_2018_QC, :CAQ)
polls_2018_QC.Polling_firm = polls_2018_QC[:, "Polling firm"]
polls_2018_QC = polls_2018_QC[in(polling_firms).(polls_2018_QC.Polling_firm), :]
polls_2018_QC.PollDate = polls_2018_QC[:, "Last date of polling"]
polls_2018_QC.SampleSize = polls_2018_QC[:, "Sample size"]
polls_2018_ON.SampleSize = clean_samplesize(polls_2018_ON.SampleSize)
polls_2018_ON.mode = clean_mode(polls_2018_ON[:, "Polling type"])
polls_2018_ON.PC = clean_mean(polls_2018_ON.PC)
polls_2018_ON.NDP = clean_mean(polls_2018_ON.NDP)
polls_2018_ON.Liberal = clean_mean(polls_2018_ON.Lib)
polls_2018_ON.Gr[polls_2018_ON.Gr .== ""] .= "–"
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
polls_2018_ON = polls_2018_ON[polls_2018_ON.Other .≥ 0, :]

# Combine polls into one DataFrame
subset_vars_ON = ["Polling_firm", "PollDate", "PC", "NDP", "Liberal", "Green", "Other",
                  "SampleSize", "mode"]
polls_ON = vcat(pre_polls_ON[:, subset_vars_ON], 
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