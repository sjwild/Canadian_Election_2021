
R"""
library(rvest)

# Opinion_polls Ontario

wiki_ON <- read_html("https://en.wikipedia.org/wiki/2022_Ontario_general_election#Opinion_polls")
wiki_tables_ON <- html_table(wiki_ON, 
                             fill = TRUE, 
                             header = TRUE)
pre_polls_ON <- wiki_tables_ON[[14]]
campaign_polls_ON <- wiki_tables_ON[[13]]



wiki_2018_ON <- read_html("https://en.wikipedia.org/wiki/2018_Ontario_general_election#Opinion_polls")
wiki_tables_2018_ON <- html_table(wiki_2018_ON, 
                                  fill = TRUE, 
                                  header = TRUE)
polls_2018_ON <- wiki_tables_2018_ON[[72]]


# Federal polls
wiki_fed <- read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_45th_Canadian_federal_election")
wiki_tables_fed <- html_table(wiki_fed, 
                              fill = TRUE, 
                              header = TRUE)


# Pre-campaign polls
pre_polls_fed <- wiki_tables_fed[[2]]

#Pre-2021 polls
wiki_2021_fed <- read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_44th_Canadian_federal_election")
wiki_tables_2021_fed <- html_table(wiki_2021_fed, 
                                   fill = TRUE, 
                                   header = TRUE)


# Pre campaign period polls for 2021 election 
pre_2021_polls_fed <- wiki_tables_2021_fed[[4]]

# Campaing polls for pre-2021 election
campaign_2021_polls_fed <- wiki_tables_2021_fed[[3]]


"""

"""
# Opinion_polls QC
wiki_QC <- read_html("https://en.wikipedia.org/wiki/2022_Quebec_general_election#Opinion_polls")
wiki_tables_QC <- html_table(wiki_QC, 
                             fill = TRUE, 
                             header = TRUE)
pre_polls_QC <- wiki_tables_QC[[8]]


wiki_2018_QC <- read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_2018_Quebec_general_election")
wiki_tables_2018_QC <- html_table(wiki_2018_QC, 
                                  fill = TRUE, 
                                  header = TRUE)
polls_2018_QC <- wiki_tables_2018_QC[[4]][, 1:9]

"""


# Extract federal polls
pre_polls = @rget pre_polls_fed
pre_2021_polls = @rget pre_2021_polls_fed
campaign_2021_polls = @rget campaign_2021_polls_fed



# extract Ontario polls
campaign_polls_ON = @rget campaign_polls_ON
pre_polls_ON = @rget pre_polls_ON
polls_2018_ON = @rget polls_2018_ON


# Extract Quebec polls
#pre_polls_QC = @rget pre_polls_QC
#polls_2018_QC = @rget polls_2018_QC




# List of polling firms for cleaning
polling_firms =  ["Abacus Data", "Leger", "Léger", 
                  "Mainstreet Research", "Mainstreet", 
                  "Innovative Research", "Innovative Research Group",
                  "Angus Reid", "Angus Reid Institute",
                  "Campaign Research", "EKOS", "CROP",
                  "Forum Research", "Forum",
                  "Environics Research", "Environics",
                  "Ipsos", "DART/Maru", "Research Co.", "Nanos Research",
                  "Pollara", "Insights West", "Stratcom", "Counsel", "Delphi Polling",
                  "Earnscliffe/Leger", "Synopsis Recherche",
                  "Discover by Navigator", "Janet Brown Opinion Research",
                  "Commonground", "Northwest Research Poll",
                  "Stratcom", "Lethbridge College"] 