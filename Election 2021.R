library(tidyverse)
library(magrittr)
library(data.table)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(lubridate)
library(ggdark)
library(rvest)
library(sf)
library(spdep)
library(INLA)

setwd("~/Desktop/Election modelling")


#### Helper functions + plot theme ####

calc_moe <- function(x, ss) sqrt(x * (1-x) / ss)

clean_mode <- function(x){
  
  mode <- ifelse(str_detect(x, "online/telephone"),
                           "Online - telephone", x)
  mode <- ifelse(str_detect(mode, "telephone/IVR"),
                           "telephone - IVR", mode)
  mode <- ifelse(str_detect(mode, "IVR") &
                             str_detect(mode, "telephone - IVR") == FALSE,
                           "IVR", mode)
  mode <- ifelse(str_detect(mode, "telephone") &
                             str_detect(mode, "telephone - IVR") == FALSE &
                             str_detect(mode, "Online - telephone") == FALSE,
                           "telephone", mode)
  mode <- ifelse(str_detect(mode, "online") &
                             str_detect(mode, "Online - telephone") == FALSE,
                           "online", mode)
  
  return(mode)
  
}


clean_previous_years <- function(df, vote){
  
  # Summarize results by riding
  df$RidingNumber <- df$Electoral.District.Number.Numéro.de.circonscription
  df$Elected <- ifelse(df$Elected.Candidate.Indicator.Indicateur.du.candidat.élu == "Y",
                                      1, 0)
  df$Party <- df$Political.Affiliation.Name_English.Appartenance.politique_Anglais
  df$Incumbent <- ifelse(df$Incumbent.Indicator.Indicateur_Candidat.sortant == "Y", 1, 0)
  df$VoteCount <- df$Candidate.Poll.Votes.Count.Votes.du.candidat.pour.le.bureau
  df <- df[, c("RidingNumber", "Party", "Elected", "Incumbent", "VoteCount")] 
  
  # Rename parties
  df$Party[df$Party == "Liberal"] <- "LPC"
  df$Party[df$Party == "Conservative"] <- "CPC"
  df$Party[df$Party == "NDP-New Democratic Party"] <- "NDP"
  df$Party[df$Party == "Bloc Québécois"] <- "BQ"
  df$Party[df$Party == "Green Party"] <- "GPC"
  df$Party <- ifelse(df$Party %in% parties, df$Party, "Other")
  
  #succumb to tidyverse style
  results <- df %>% 
    group_by(RidingNumber, Party) %>%
    summarize(Elected = max(Elected),
              Incumbent = max(Incumbent),
              VoteCount = sum(VoteCount, na.rm = TRUE))
  results <- results %>%
    group_by(RidingNumber) %>%
    mutate(VotePercent = VoteCount/sum(VoteCount))
  
  
  results <- left_join(results, data.frame(Party = c("LPC", "CPC", "NDP", "BQ", "GPC", "Other"),
                                                     Vote = vote),
                            by = "Party")
  
  results <- pivot_wider(results[, c("RidingNumber", "VotePercent", "Vote", "Incumbent", "Party")],
                              names_from = "Party", 
                              values_from = c("VotePercent", "Vote", "Incumbent"))
  
  return(results)
  
}


# To use Stan
# Taken from https://github.com/stan-dev/example-models/blob/master/knitr/car-iar-poisson/nbdata4stan.R
nb2graph = function(x) {
  N = length(x);
  n_links = 0;
  for (i in 1:N) {
    if (x[[i]][1] != 0) {
      n_links = n_links + length(x[[i]]);
    }
  }
  N_edges = n_links / 2;
  node1 = vector(mode="numeric", length=N_edges);
  node2 = vector(mode="numeric", length=N_edges);
  idx = 0;
  for (i in 1:N) {
    if (x[[i]][1] > 0) {
      for (j in 1:length(x[[i]])) {
        n2 = unlist(x[[i]][j]);
        if (i < n2) {
          idx = idx + 1;
          node1[idx] = i;
          node2[idx] = n2;
        }
      }
    }
  }
  return (list("N"=N,"N_edges"=N_edges,"node1"=node1,"node2"=node2));
}

get_scaling_factor = function(nbs) {
  #Build the adjacency matrix
  adj.matrix = sparseMatrix(i=nbs$node1,j=nbs$node2,x=1,symmetric=TRUE)
  #The ICAR precision matrix (note! This is singular)
  Q=  Diagonal(nbs$N, rowSums(adj.matrix)) - adj.matrix
  
  #Add a small jitter to the diagonal for numerical stability (optional but recommended)
  Q_pert = Q + Diagonal(nbs$N) * max(diag(Q)) * sqrt(.Machine$double.eps)
  
  # Compute the diagonal elements of the covariance matrix subject to the
  # constraint that the entries of the ICAR sum to zero.
  #See the function help for further details.
  Q_inv = inla.qinv(Q_pert, constr=list(A = matrix(1,1,nbs$N),e=0))
  
  #Compute the geometric mean of the variances, which are on the diagonal of Q.inv
  return((mean(log(diag(Q_inv)))))
}



# theme for images
theme_blog <- theme_minimal() +
  theme(plot.caption = element_text(colour = "grey50"),
        text = element_text(family = "Courier"),
        strip.text = element_text(size = rel(.9), face = "bold"),
        axis.text = element_text(size = rel(1.0)),
        plot.title  = element_text(face = "bold"),
        plot.subtitle  = element_text(face = "bold"),
        axis.line.x.bottom = element_line(colour = "grey50"),
        axis.line.y.left = element_line(colour = "grey50"))





#### fit polls Canada 2015 to 2019 ####
parties <- c("LPC", "CPC", "NDP", "BQ", "GPC")
wiki <- read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_2019_Canadian_federal_election")
wiki_tables <- html_table(wiki, 
                          fill = TRUE, 
                          header = TRUE)


# Campaign period polls
campaign_polls <- wiki_tables[[2]]
campaign_polls$Polling_firm <- campaign_polls$`Polling firm`
campaign_polls$PollDate <- campaign_polls$`Last dateof polling[1]`
campaign_polls$SampleSize <- str_remove_all(campaign_polls$`Samplesize[3]`, "\\([0-9]\\/[0-9]\\)")
campaign_polls$SampleSize <- str_remove_all(campaign_polls$SampleSize, ",")
campaign_polls$mode <- clean_mode(campaign_polls$`Polling method[4]`)
campaign_polls <- campaign_polls[, c("Polling_firm", "PollDate", parties, "SampleSize", "mode")]


# Pre-campaign polls
pre_polls <- wiki_tables[[3]]
pre_polls$Polling_firm <- pre_polls$`Polling firm`
pre_polls$PollDate <- pre_polls$`Last dateof polling[1]`
pre_polls$SampleSize <- str_remove_all(pre_polls$`Samplesize[3]`, "\\([0-9]\\/[0-9]\\)")
pre_polls$SampleSize <- str_remove_all(pre_polls$SampleSize, ",")
pre_polls$mode <- clean_mode(pre_polls$`Polling method[4]`)
pre_polls <- pre_polls[, c("Polling_firm", "PollDate", parties, "SampleSize", "mode")]



# Combine polls and subset
can_polls <- rbind(pre_polls, campaign_polls)
can_polls <- subset(can_polls, Polling_firm != "")


# Election results
election_results <- subset(can_polls, str_detect(can_polls$Polling_firm, "election"))
can_polls <- subset(can_polls, str_detect(can_polls$Polling_firm, "election") == FALSE)
can_polls <- subset(can_polls, str_detect(can_polls$Polling_firm, "Polling firm") == FALSE)

election_results[,parties] <- sapply(election_results[,parties], as.numeric)
election_results[,parties] <- sapply(election_results[,parties], function(x) x / 100)



# Sample size
can_polls$SampleSize <- str_remove_all(can_polls$SampleSize, "\\([0-9]\\/[0-9]\\)")
can_polls$SampleSize <- str_remove_all(can_polls$SampleSize, ",")


#set.seed(10438174)
#can_polls$SampleSize[can_polls$SampleSize == ""] <- sample(can_polls$SampleSize, 1)

# Convert to numeric
can_polls[,c(parties, "SampleSize")] <- sapply(can_polls[,c(parties, "SampleSize")], as.numeric)
can_polls[,c(parties)] <- sapply(can_polls[,parties], function(x) x / 100)



# Calculate srs moe
parties_moe <- paste0(parties, "_moe")
for(i in 1:length(parties)){
  x <- paste0(parties[i])
  x_moe <- paste0(parties_moe[i])
  
  can_polls[,x_moe] <- calc_moe(can_polls[,x], can_polls$SampleSize)
  
}

parties_all <- c(parties, "Other")
parties_moe_all <- c(parties_moe, "Other_moe")

can_polls$Other <- 1 - (can_polls$LPC + can_polls$CPC + 
                          can_polls$NDP + can_polls$BQ + can_polls$GPC)
can_polls$Other_moe <- calc_moe(can_polls$Other, can_polls$SampleSize)

election_results$Other <- 1 - (election_results$LPC + election_results$CPC + 
                                 election_results$NDP + election_results$BQ + election_results$GPC)

# Dates
can_polls$PollDate <- mdy(can_polls$PollDate)
election_results$PollDate <- mdy(election_results$PollDate)
N_days <- as.numeric(election_results$PollDate[2] - election_results$PollDate[1]) + 1
can_polls$NumDays <- as.numeric(can_polls$PollDate - election_results$PollDate[1]) + 1

# Remove announcements
can_polls <- subset(can_polls, !is.na(LPC))


#Pre-2023 polls
wiki_2023 <- read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_44th_Canadian_federal_election")
wiki_tables_2023 <- html_table(wiki_2023, 
                               fill = TRUE, 
                               header = TRUE)


# Campaign period polls
pre_2023_polls <- wiki_tables_2023[[3]]
pre_2023_polls$Polling_firm <- pre_2023_polls$`Polling firm`
pre_2023_polls$PollDate <- pre_2023_polls$`Last dateof polling[1]`
pre_2023_polls$SampleSize <- str_remove_all(pre_2023_polls$`Samplesize[4]`, "\\([0-9]\\/[0-9]\\)")
pre_2023_polls$SampleSize <- str_remove_all(pre_2023_polls$SampleSize, ",")
pre_2023_polls$mode <- clean_mode(pre_2023_polls$`Polling method[5]`)
pre_2023_polls <- pre_2023_polls[, c("Polling_firm", "PollDate", parties, "SampleSize", "mode")]



# Combine polls and subset
pre_2023_polls <- subset(pre_2023_polls, Polling_firm != "")
pre_2023_polls <- subset(pre_2023_polls, str_detect(pre_2023_polls$Polling_firm, "election") == FALSE)
pre_2023_polls <- pre_2023_polls[2:nrow(pre_2023_polls),]

# Sample size
pre_2023_polls$SampleSize <- str_remove_all(pre_2023_polls$SampleSize, "\\([0-9]\\/[0-9]\\)")
pre_2023_polls$SampleSize <- str_remove_all(pre_2023_polls$SampleSize, ",")


#set.seed(10438174)
#pre_2023_polls$SampleSize[pre_2023_polls$SampleSize == ""] <- sample(pre_2023_polls$SampleSize, 1)

# Convert to numeric
pre_2023_polls[,c(parties, "SampleSize")] <- sapply(pre_2023_polls[,c(parties, "SampleSize")], as.numeric)
pre_2023_polls[,c(parties)] <- sapply(pre_2023_polls[,parties], function(x) x / 100)


# Calculate moe
parties_moe <- paste0(parties, "_moe")
for(i in 1:length(parties)){
  x <- paste0(parties[i])
  x_moe <- paste0(parties_moe[i])
  
  pre_2023_polls[,x_moe] <- calc_moe(pre_2023_polls[,x], pre_2023_polls$SampleSize)
  
}


pre_2023_polls$Other <- 1 - (pre_2023_polls$LPC + pre_2023_polls$CPC + 
                               pre_2023_polls$NDP + pre_2023_polls$BQ + pre_2023_polls$GPC)
pre_2023_polls$Other_moe <- calc_moe(pre_2023_polls$Other, pre_2023_polls$SampleSize)


# Dates
pre_2023_polls$PollDate <- mdy(pre_2023_polls$PollDate)
pre_2023_polls$NumDays <- as.numeric(pre_2023_polls$PollDate - election_results$PollDate[1]) + 1

# Remove announcements
pre_2023_polls <- subset(pre_2023_polls, !is.na(BQ))




# Campaign period polls
campaign_2023_polls <- wiki_tables_2023[[2]]
campaign_2023_polls$Polling_firm <- campaign_2023_polls$`Polling firm`
campaign_2023_polls$PollDate <- campaign_2023_polls$`Last dateof polling[1]`
campaign_2023_polls$SampleSize <- str_remove_all(campaign_2023_polls$`Samplesize[4]`, "\\([0-9]\\/[0-9]\\)")
campaign_2023_polls$SampleSize <- str_remove_all(campaign_2023_polls$SampleSize, ",")
campaign_2023_polls$mode <- clean_mode(campaign_2023_polls$`Polling method[5]`)
campaign_2023_polls <- campaign_2023_polls[, c("Polling_firm", "PollDate", parties, "SampleSize", "mode")]



# Combine polls and subset
campaign_2023_polls <- subset(campaign_2023_polls, Polling_firm != "")
campaign_2023_polls <- subset(campaign_2023_polls, str_detect(campaign_2023_polls$Polling_firm, "election") == FALSE)
campaign_2023_polls <- campaign_2023_polls[2:nrow(campaign_2023_polls),]

# Sample size
campaign_2023_polls$SampleSize <- str_remove_all(campaign_2023_polls$SampleSize, "\\([0-9]\\/[0-9]\\)")
campaign_2023_polls$SampleSize <- str_remove_all(campaign_2023_polls$SampleSize, ",")


#set.seed(10438174)
#campaign_2023_polls$SampleSize[campaign_2023_polls$SampleSize == ""] <- sample(campaign_2023_polls$SampleSize, 1)

# Convert to numeric
campaign_2023_polls[,c(parties, "SampleSize")] <- sapply(campaign_2023_polls[,c(parties, "SampleSize")], as.numeric)
campaign_2023_polls[,c(parties)] <- sapply(campaign_2023_polls[,parties], function(x) x / 100)


# Calculate moe
parties_moe <- paste0(parties, "_moe")
for(i in 1:length(parties)){
  x <- paste0(parties[i])
  x_moe <- paste0(parties_moe[i])
  
  campaign_2023_polls[,x_moe] <- calc_moe(campaign_2023_polls[,x], campaign_2023_polls$SampleSize)
  
}


campaign_2023_polls$Other <- 1 - (campaign_2023_polls$LPC + campaign_2023_polls$CPC + 
                               campaign_2023_polls$NDP + campaign_2023_polls$BQ + campaign_2023_polls$GPC)
campaign_2023_polls$Other_moe <- calc_moe(campaign_2023_polls$Other, campaign_2023_polls$SampleSize)


# Dates
campaign_2023_polls$PollDate <- mdy(campaign_2023_polls$PollDate)
campaign_2023_polls$NumDays <- as.numeric(campaign_2023_polls$PollDate - election_results$PollDate[1]) + 1

# Remove announcements
campaign_2023_polls <- subset(campaign_2023_polls, !is.na(BQ))




# New N_days, using Sept 30 as final date
N_days_2021 <- as.numeric(ymd("2021-09-20") - election_results$PollDate[1]) + 1


# Combine datasets
can_polls2 <- rbind(can_polls, pre_2023_polls, campaign_2023_polls)
can_polls2 <- can_polls2[complete.cases(can_polls2),]
can_polls2 <- subset(can_polls2, Other > 0)

# indexes for stan
can_polls2$pollster_id <- as.numeric(as.factor(can_polls2$Polling_firm))
can_polls2$mode_id <- as.numeric(as.factor(can_polls2$mode))

N_pollsters <- length(unique(can_polls2$pollster_id))
N_modes <- length(unique(can_polls2$mode))

# write CSV
write.csv(can_polls2, file = "can_polls2.csv", 
          row.names = FALSE)
#write.csv(can_polls2, file = "/Users/stephenwild/Desktop/Canandian_Election_2021/can_polls2.csv",
#          row.names = FALSE)

# fit state space model for pooling the polls
state_space_all <- cmdstan_model("state_space_all_parties_non_centered_prediction.stan")


all_data <- list(
  
  N_days = N_days_2021,
  N_parties = length(parties_all),
  N_polls = nrow(can_polls2),
  N_pollsters = N_pollsters,
  N_modes = N_modes,
  
  xi_start = as.vector(as.matrix(election_results[1, parties_all])),
  xi_2019 = as.vector(as.matrix(election_results[2, parties_all])),
  election_day_2019 = N_days,
  
  y = as.matrix(can_polls2[,parties_all]),
  y_moe = as.matrix(can_polls2[, parties_moe_all]),
  
  poll_date = can_polls2$NumDays,
  pollster_id = can_polls2$pollster_id,
  
  mode_id = can_polls2$mode_id
  
  
)

fit_all <- state_space_all$sample(
  data = all_data,
  seed = 6319483,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 750,
  iter_sampling = 750,
  refresh = 100,
  max_treedepth = 15,
  adapt_delta = 0.9
  
)

# diagnoses
fit_all$cmdstan_diagnose()
fit_all$save_object(file = "fit_all_July23.RDS")


# extract info for plots
all_draws <- as_draws_df(fit_all$draws("xi"))
house_draws <- as_draws_df(fit_all$draws("delta"))
#mode_draws <- as_draws_df(fit_all$draws("gamma"))

all_trend <- data.frame(mu = apply(all_draws[,1:(ncol(all_draws)-3)], 2, mean),
                        ll = apply(all_draws[,1:(ncol(all_draws)-3)], 2, function(x) quantile(x, 0.025)),
                        uu = apply(all_draws[,1:(ncol(all_draws)-3)], 2, function(x) quantile(x, 0.975)),
                        sigma_obs = apply(all_draws[,1:(ncol(all_draws)-3)], 2, sd),
                        party = rep(parties_all, each = N_days_2021),
                        NumDays = rep(1:N_days_2021, length(parties_all)))
house_effects <- data.frame(mu = apply(house_draws[,1:(ncol(house_draws) - 3)], 2, mean),
                            ll = apply(house_draws[,1:(ncol(house_draws) - 3)], 2, function(x) quantile(x, 0.025)),
                            uu = apply(house_draws[,1:(ncol(house_draws) - 3)], 2, function(x) quantile(x, 0.975)),
                            Polling_firm = rep(levels(as.factor(can_polls2$Polling_firm)), length(parties_all)),
                            party = rep(parties_all, each = N_pollsters))
#mode_effects <- data.frame(mu = apply(mode_draws[,1:(ncol(mode_draws) - 3)], 2, mean),
#                            ll = apply(mode_draws[,1:(ncol(mode_draws) - 3)], 2, function(x) quantile(x, 0.025)),
#                            uu = apply(mode_draws[,1:(ncol(mode_draws) - 3)], 2, function(x) quantile(x, 0.975)),
#                            mode = rep(levels(as.factor(can_polls2$mode)), length(parties_all)),
#                            party = rep(parties_all, each = length(unique(can_polls2$mode_id))))
house_effects$party = factor(house_effects$party, levels = parties_all)



# Join dates for x_axis
dates_join <- data_frame(NumDays = 1:N_days_2021)
dates_join$PollDate <- ymd(dates_join$NumDays + election_results$PollDate[1] - 1)
all_trend <- left_join(all_trend, dates_join, by = "NumDays")

can_polls_long <- pivot_longer(can_polls2[c(parties_all, "Polling_firm", "NumDays")],
                               cols = parties_all)
can_polls_long$party <- can_polls_long$name
can_polls_long <- left_join(can_polls_long, dates_join, by = "NumDays")



# Plot vote intention and house effects
vote_intention <- ggplot() +
  geom_point(data = can_polls_long,
             mapping = aes(x = PollDate,
                           y = value,
                           group = party,
                           colour = party)) +
  geom_line(data = all_trend,
            mapping = aes(x = PollDate,
                          y = mu,
                          group = party,
                          colour = party),
            size = 1.0) +
  geom_ribbon(data = all_trend,
              mapping = aes(x = PollDate,
                            ymin = ll,
                            ymax = uu,
                            group = party,
                            fill = party),
              alpha = 0.5) +
  labs(title = "Vote intention of Canadian voters",
       subtitle = "2015 to 2021",
       x = NULL,
       y = NULL,
       caption = "Source: Data from Wikipedia. Analysis by sjwild.github.io\nUpdated 25-July-2021") +
  scale_colour_manual(breaks = parties_all,
                      values = c("red", "blue", "orange", "lightblue", "green", "purple"),
                      name = "Party") +
  scale_fill_manual(breaks = parties_all,
                    values = c("red", "blue", "orange", "skyblue", "green", "purple"),
                    name = "Party") +
  scale_y_continuous(breaks = c(0, .2, .4, .6),
                     labels = c("0%", "20%", "40%", "60%")) +
  theme_blog
ggsave(plot = vote_intention, filename = "vote_intention_2015_to_2021.png",
       height = 3, width = 5, units = "in")
vote_intention

house_effect_plot <- ggplot(house_effects) +
  geom_pointrange(mapping = aes(x = Polling_firm,
                                y = mu,
                                ymin = ll,
                                ymax = uu)) +
  geom_hline(yintercept = 0,
             colour = "orange",
             linetype = "dashed") +
  labs(x = NULL, 
       y = "Percent",
       title = "House effects: all parties",
       subtitle = "2015 to 2021",
       caption = "Source: Data from Wikipedia. Analysis by sjwild.github.io\nUpdated 25-July-2021") +
  scale_y_continuous(breaks = c(-0.05, 0.00, 0.05, 0.1),
                     labels = c("-5", "0", "5", "10")) +
  coord_flip() +
  facet_wrap(~party) +
  theme_blog
ggsave(plot = house_effect_plot, filename = "house_effects_2015_to_2021.png",
       height = 1500, width = 2000, units = "px")
house_effect_plot

all_trend[all_trend$PollDate == ymd("2021-08-07"),]
#mode_effect_plot <- ggplot(mode_effects) +
#  geom_pointrange(mapping = aes(x = mode,
#                                y = mu,
#                                ymin = ll,
#                                ymax = uu),
#                  colour = "white") +
#  geom_hline(yintercept = 0,
#             colour = "orange",
#             linetype = "dashed") +
#  labs(x = NULL, 
#       y = "Percent",
#       title = "Survey mode effects: all parties",
#       subtitle = "2015 to 2021") +
#  scale_y_continuous(breaks = c(-0.05, 0.00, 0.05, 0.1),
#                     labels = c("-5", "0", "5", "10")) +
#  coord_flip() +
#  facet_wrap(~party) +
#  dark_theme_bw() 
#ggsave(plot = mode_effect_plot, filename = "mode_effects_2015_to_2021.png",
#       height = 5.25, width = 9.2, units = "in")
#mode_effect_plot




# Check that sums of means are close to 1
sums <- rep(0, N_days_2021)
for(i in 1:N_days_2021) sums[i] <- sum(all_trend$mu[all_trend$NumDays == i])


ggplot(data.frame(X = sums)) + 
  geom_density(mapping = aes(x = X))  +
  theme_blog

all_trend[all_trend$PollDate == ymd("2021-08-17"),]

# Results for all elections:
raw_results <- data.frame(LPC = c(.331, .395, .189, .262, .302, .367, .408, .385, .413),
                          CPC = c(.343, .319, .396, .376, .363, .296, .255 + .122, .194 + .188, .187 + .16),
                          NDP = c(.16, .197, .306, .182, .175, .157, .085, .11, .069),
                          BQ = c(.076, .047, .06, .1, .105, .124, .107, .107, .135),
                          GPC = c(.065, .034, .039, .068, .045, .043, NA, NA, NA),
                          time = c(46, 113, 63, 32, 145, 128, 85, 43, 110),
                          LPC_pop = c(.33, .31, .29, .32, .34, .35, .56, .52, .36),
                          CPC_pop = c(.36, .31, .34, .33, .27, .26, .31, NA, NA),
                          NDP_pop = c(.11, .27, .16, .13, .18, .13, NA, NA, NA),
                          BQ_pop = c(.04, .05, .1, .11, .11, .16, NA, NA, NA),
                          GPC_pop = c(.12, .04, .05, .10, .05, .05, NA, NA, NA),
                          UR = c(5.8, 6.9, 7.7, 6.1, 6.7, 7.4, 6.8, 9.6, 11.7),
                          CPC_incumbent = c(0, 1, 1, 1, 0, 0, 0, 0, 1),
                          PPC = c(1, 0, 0, 0, 0, 0, 0, 0, 0),
                          LPC_incumbent = c(1, 0, 0, 0, 1, 1, 1, 1, 0),
                          Election_year = c(2019, 2015, 2011, 2008, 2006, 2004, 2000, 1997, 1993),
                          year_num = c(6, 5, 4, 3, 2, 1, NA, NA, NA),
                          LPC_seats = c(157, 184, 34, 77, 103, 135, NA, NA, NA),
                          CPC_seats = c(121, 99, 166, 143, 124, 99, NA, NA, NA),
                          NDP_seats = c(24, 44, 103, 37, 29, 19, NA, NA, NA),
                          BQ_seats = c(32, 10, 4, 49, 51, 54, NA, NA, NA),
                          GPC_seats = c(3, 1, 1, 0, 0, 0, NA, NA, NA),
                          Total_seats = c(338, 338, 308, 308, 308, 308, NA, NA, NA))
raw_results$logtime <- log(raw_results$time)
raw_results$Other <- (1 - raw_results$LPC -
                        raw_results$CPC -
                        raw_results$NDP -
                        raw_results$BQ -
                        raw_results$GPC)
raw_results$Other_pop <- (1 - raw_results$LPC_pop -
                        raw_results$CPC_pop -
                        raw_results$NDP_pop -
                        raw_results$BQ_pop -
                        raw_results$GPC_pop)
raw_results$Other_seats <- (raw_results$Total_seats - raw_results$LPC_seats -
                              raw_results$CPC_seats -
                              raw_results$NDP_seats -
                              raw_results$BQ_seats -
                              raw_results$GPC_seats)

newdata = data.frame(logtime = log(24 + 46),
                     LPC_pop = .365,
                     CPC_pop = .285,
                     NDP_pop =.182,
                     BQ_pop = .070,
                     GPC_pop = .054,
                     Other_pop = .0536,
                     UR = 7.5, 
                     CPC_incumbent = 0,
                     PPC = 1,
                     year_num = 7)






#### Load seat data to estimate vote by district ####
vote_2015 <- as.vector(unlist(election_results[1, parties_all]))
vote_2019 <- as.vector(unlist(election_results[2, parties_all]))
riding_votes <- c("VotePercent_LPC", "VotePercent_CPC",
                  "VotePercent_NDP", "VotePercent_BQ",
                  "VotePercent_GPC", "VotePercent_Other")
national_votes <- c("Vote_LPC", "Vote_CPC", "Vote_NDP",
                    "Vote_BQ", "Vote_GPC", "Vote_Other")


# create file list
file_list_2015 <- list.files(path = "Vote 2015")
file_list_2019 <- list.files(path = "Vote 2019")


# Load 2019
results_2019_list <- list()
for(i in 1:length(file_list_2019)){
  results_2019_list[[i]] <- read.csv(file = paste0("Vote 2019/", file_list_2019[i]))
}

results_2019_full <- rbindlist(results_2019_list)
rm(results_2019_list)

results_2019 <- clean_previous_years(results_2019_full, 
                                     vote_2019)

results_2019$VotePercent_Other[is.na(results_2019$VotePercent_Other)] <- 0.0
results_2019$VotePercent_BQ[is.na(results_2019$VotePercent_BQ)] <- 0.0
results_2019$Vote_BQ[is.na(results_2019$Vote_BQ)] <- 0.076
results_2019$Vote_Other[is.na(results_2019$Vote_Other)] <- 0.025
results_2019$Incumbent_Other[is.na(results_2019$Incumbent_Other)] <- 1
results_2019$Incumbent_BQ[is.na(results_2019$Incumbent_BQ)] <- 0


# Load 2015
results_2015_list <- list()
for(i in 1:length(file_list_2015)){
  results_2015_list[[i]] <- read.csv(file = paste0("Vote 2015/", file_list_2015[i]))
}

results_2015_full <- rbindlist(results_2015_list)
rm(results_2015_list)

results_2015 <- clean_previous_years(results_2015_full, 
                                     vote_2015)

results_2015$VotePercent_Other[is.na(results_2015$VotePercent_Other)] <- 0.0
results_2015$VotePercent_BQ[is.na(results_2015$VotePercent_BQ)] <- 0.0
results_2015$Vote_BQ[is.na(results_2015$Vote_BQ)] <- 0.047
results_2015$Vote_Other[is.na(results_2015$Vote_Other)] <- 0.008
results_2015$Incumbent_Other[is.na(results_2015$Incumbent_Other)] <- 1
results_2015$Incumbent_BQ[is.na(results_2015$Incumbent_BQ)] <- 0


# create results 2021
election_date <- ymd("2021-08-17")
results_2021 <- data.frame(ID = 1:338,
                           Vote_LPC = all_trend$mu[all_trend$PollDate == election_date &
                                                     all_trend$party == "LPC"],
                           Vote_CPC = all_trend$mu[all_trend$PollDate == election_date &
                                                     all_trend$party == "CPC"],
                           Vote_NDP = all_trend$mu[all_trend$PollDate == election_date &
                                                     all_trend$party == "NDP"],
                           Vote_BQ = all_trend$mu[all_trend$PollDate == election_date &
                                                    all_trend$party == "BQ"],
                           Vote_GPC = all_trend$mu[all_trend$PollDate == election_date &
                                                     all_trend$party == "GPC"],
                           Vote_Other = all_trend$mu[all_trend$PollDate == election_date &
                                                      all_trend$party == "Other"])



results_2015_2019 <- rbind(results_2015, results_2019)


# get boundaries
riding_boundaries <- st_read("Shapefiles/lfed000b16a_e.shp")
riding_boundaries$ID <- 1:nrow(riding_boundaries)
riding_boundaries$FEDUID <- as.numeric(riding_boundaries$FEDUID)
results_2015_2019 <- left_join(results_2015_2019, riding_boundaries[,c("FEDUID", "ID")], 
                          by = c("RidingNumber" = "FEDUID"))
results_2015_2019$geometry <- NULL


nb <- poly2nb(riding_boundaries, 
              queen = TRUE,
              row.names = riding_boundaries$FEDUID)
save(nb, file = "Shapefiles/nb.rds")


# Prep and run Stan model
nodes <- nb2graph(nb)
scaling_factor <- get_scaling_factor(nodes)
#write.csv(nodes, file = "nodes.csv", row.names = FALSE)





single_riding <- cmdstan_model("single riding.stan")

fit_ridings <- list()
riding_draws <- list()

for(i in 1:length(parties)){
  
  single_data <- list(
    
    N_ridings = 338,
    
    riding_id = results_2015_2019$ID,
    
    N_edges = nodes$N_edges,
    node1 = nodes$node1,
    node2 = nodes$node2,
    
    X = as.vector(unlist(results_2015_2019[, paste(national_votes[i])])),
    
    Y = as.vector(unlist(results_2015_2019[, paste(riding_votes[i])])),
    
    X_obs = all_trend$mu[all_trend$PollDate == ymd("2021-08-13") &
                                         all_trend$party == paste(parties[i])],
    sigma_obs = all_trend$sigma_obs[all_trend$PollDate == ymd("2021-08-13") &
                                         all_trend$party == paste(parties[i])],
    
    sf = scaling_factor
    
  )
  
  fit_ridings[[i]] <- single_riding$sample(
    data = single_data,
    seed = 43150228,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 5000,
    iter_sampling = 2500,
    refresh = 500
    
  )
  
  riding_draws[[i]] <- data.frame(as_draws_df(fit_ridings[[i]]$draws("y_imp")))
  
}


riding_draws[[6]] <- 1 - riding_draws[[1]] -
  riding_draws[[2]] -
  riding_draws[[3]] -
  riding_draws[[4]] -
  riding_draws[[5]] 

pred_winner <- matrix(nrow = nrow(riding_draws[[1]]),
                      ncol = 338)
for(i in 1:nrow(riding_draws[[1]])){
  for(j in 1:338){
    tmp_votes <- data.frame(LPC = riding_draws[[1]][i,j],
                            CPC = riding_draws[[2]][i,j],
                            NDP = riding_draws[[3]][i,j],
                            BQ = riding_draws[[4]][i,j],
                            GPC = riding_draws[[5]][i,j],
                            Other = riding_draws[[6]][i,j])
    pred_winner[i, j] = colnames(tmp_votes)[apply(tmp_votes,1,which.max)]

  }
  
}

pred_seats <- matrix(nrow = 10000, ncol = 6)
for(i in 1:10000){
  for(j in 1:6){
    pred_seats[i,j] <- length(pred_winner[i, pred_winner[i,] == parties_all[j]])
  }
}

pred_seats <- data.frame(pred_seats)
colnames(pred_seats) <- parties_all

pred_probs <- matrix(nrow = 338, ncol = 6)
for(i in 1:338){
  for(j in 1:6){
    pred_probs[i,j] <- length(pred_winner[pred_winner[,i] == parties_all[j], i]) / 10000
  }
}

pred_probs <- data.frame(pred_probs)
colnames(pred_probs) <- parties_all

apply(pred_seats, 2, function(x) quantile(x, probs = c(0, 0.02, 0.5, 0.975, 1)))



sum(pred_seats$CPC > pred_seats$LPC) / nrow(pred_seats)














mean_votes <- data.frame(LPC = LPC_preds$mean,
                         CPC = CPC_preds$mean,
                         NDP = NDP_preds$mean,
                         BQ = BQ_preds$mean,
                         GPC = GPC_preds$mean )
mean_votes$Other <- 1 - apply(mean_votes[,c("LPC", "CPC", "NDP", "BQ", "GPC")], 1, sum)

mean_votes$predwinner <- colnames(mean_votes)[apply(mean_votes,1,which.max)]
mean_votes$winner <- colnames(results_2019[, riding_votes])[apply(results_2019[, riding_votes],1,which.max)]


# For inla
W <- nb2mat(nb, style = "B")
nb2INLA("Shapefiles/g.adj", nb)

mod_inla_LPC <- inla(VotePercent_LPC ~ 1 + Vote2019_LPC + Incumbent_LPC +
                   f(ID, model = "bym2", graph = W),
                 control.predictor = list(compute = TRUE),
                 control.compute = list(waic = TRUE),
                 data = results_2019)
mod_inla_CPC <- inla(VotePercent_CPC ~ 1 + Vote2019_CPC + Incumbent_CPC +
                   f(ID, model = "bym2", graph = W),
                 control.predictor = list(compute = TRUE),
                 control.compute = list(waic = TRUE),
                 data = results_2019)
mod_inla_NDP <- inla(VotePercent_NDP ~ 1 + Vote2019_NDP + Incumbent_NDP +
                       f(ID, model = "bym2", graph = W),
                     control.predictor = list(compute = TRUE),
                     control.compute = list(waic = TRUE),
                     data = results_2019)
mod_inla_BQ <- inla(VotePercent_BQ ~ 1 + Vote2019_BQ + Incumbent_BQ +
                       f(ID, model = "bym2", graph = W),
                     control.predictor = list(compute = TRUE),
                     control.compute = list(waic = TRUE),
                     data = results_2019)
mod_inla_GPC <- inla(VotePercent_GPC ~ 1 + Vote2019_GPC + Incumbent_GPC +
                       f(ID, model = "bym2", graph = W),
                     control.predictor = list(compute = TRUE),
                     control.compute = list(waic = TRUE),
                     data = results_2019)

LPC_preds <- mod_inla_LPC$summary.linear.predictor
CPC_preds <- mod_inla_CPC$summary.linear.predictor
NDP_preds <- mod_inla_NDP$summary.linear.predictor
BQ_preds <- mod_inla_BQ$summary.linear.predictor
GPC_preds <- mod_inla_GPC$summary.linear.predictor

mean_votes <- data.frame(LPC = LPC_preds$mean,
                         CPC = CPC_preds$mean,
                         NDP = NDP_preds$mean,
                         BQ = BQ_preds$mean,
                         GPC = GPC_preds$mean )
mean_votes$Other <- 1 - apply(mean_votes[,c("LPC", "CPC", "NDP", "BQ", "GPC")], 1, sum)

mean_votes$predwinner <- colnames(mean_votes)[apply(mean_votes,1,which.max)]
mean_votes$winner <- colnames(results_2019[, riding_votes])[apply(results_2019[, riding_votes],1,which.max)]









vars_to_subset <- c(8:33, 107:107, 109, 691, 695:707, 1289:1290, 1617:1618,
                    1683, 1685, 1693, 1886, 1897)
demo_data <- read.csv("Riding census data.csv")
demo_data$RidingNumber <- demo_data$GEO_CODE..POR.
demo_data$Index <- as.numeric(demo_data$Member.ID..Profile.of.Federal.Electoral.Districts..2013.Representation.Order...2247.)
demo_data$forcol <- demo_data$DIM..Profile.of.Federal.Electoral.Districts..2013.Representation.Order...2247.
demo_data$Total <- as.numeric(demo_data$Dim..Sex..3...Member.ID...1...Total...Sex)
demo_data$Male <- as.numeric(demo_data$Dim..Sex..3...Member.ID...2...Male)
demo_data$Female <- as.numeric(demo_data$Dim..Sex..3...Member.ID...3...Female)

#demo_data <- demo_data[, c("RidingNumber","forcol",  "Index", "Total", "Male", "Female")]
demo_data <- demo_data[, c("RidingNumber","forcol", "Index", "Total")]
demo_data <- subset(demo_data, Index %in% vars_to_subset &
                      RidingNumber > 10000)
demo_data$Index <- NULL
demo_data <- demo_data %>% pivot_wider(names_from = forcol, values_from = Total)








nodes <- nb2graph(nb)
scaling_factor <- get_scaling_factor(nodes)

# To use Stan
# Taken from https://github.com/stan-dev/example-models/blob/master/knitr/car-iar-poisson/nbdata4stan.R
nb2graph = function(x) {
  N = length(x);
  n_links = 0;
  for (i in 1:N) {
    if (x[[i]][1] != 0) {
      n_links = n_links + length(x[[i]]);
    }
  }
  N_edges = n_links / 2;
  node1 = vector(mode="numeric", length=N_edges);
  node2 = vector(mode="numeric", length=N_edges);
  idx = 0;
  for (i in 1:N) {
    if (x[[i]][1] > 0) {
      for (j in 1:length(x[[i]])) {
        n2 = unlist(x[[i]][j]);
        if (i < n2) {
          idx = idx + 1;
          node1[idx] = i;
          node2[idx] = n2;
        }
      }
    }
  }
  return (list("N"=N,"N_edges"=N_edges,"node1"=node1,"node2"=node2));
}

get_scaling_factor = function(nbs) {
  #Build the adjacency matrix
  adj.matrix = sparseMatrix(i=nbs$node1,j=nbs$node2,x=1,symmetric=TRUE)
  #The ICAR precision matrix (note! This is singular)
  Q=  Diagonal(nbs$N, rowSums(adj.matrix)) - adj.matrix
  
  #Add a small jitter to the diagonal for numerical stability (optional but recommended)
  Q_pert = Q + Diagonal(nbs$N) * max(diag(Q)) * sqrt(.Machine$double.eps)
  
  # Compute the diagonal elements of the covariance matrix subject to the
  # constraint that the entries of the ICAR sum to zero.
  #See the function help for further details.
  Q_inv = inla.qinv(Q_pert, constr=list(A = matrix(1,1,nbs$N),e=0))
  
  #Compute the geometric mean of the variances, which are on the diagonal of Q.inv
  return((mean(log(diag(Q_inv)))))
}

