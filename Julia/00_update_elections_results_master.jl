
ENV["CMDSTAN"] = expanduser("~/cmdstan/")

# activate ElectionModelling
using Pkg
Pkg.activate("ElectionModelling")

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
updated_date = "June 22, 2024"
day_title = "June 22, 2024"
update_date = Date(2024, 06, 22)
value_date = Date(2024, 06, 22)
dateformat = DateFormat("y-m-d")

# Load custom functions for cleaning and post-processing
include("01_custom_functions.jl")


# Web scraping using RCall.jl and rvest
include("02_web_scraping.jl")


# clean federal election polls
include("03_clean_federal_polls.jl")


# Clean provincial polls. Set up this way so I can gradually add in provinces 
# as I feel like it. Will probably do QC and AB next
include("04a_clean_ON_polls.jl")


# Run models for each and extract samples
include("05a_run_model_federal_polls.jl")
include("05b_run_model_ON_polls.jl")

# Plot data
include("06a_plot_results_federal_polls.jl")
include("06b_plot_results_ON_polls.jl")




