library(tidyverse)
library(pscl)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(lubridate)
library(ggdark)
library(rvest)


setwd("~/Desktop/Election modelling")

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
  
  return(x)
  
}


#fit polls Canada 2011 to 2015
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
campaign_polls$mode <- ifelse(str_detect(campaign_polls$`Polling method[4]`, "online/telephone"),
                              "Online - telephone", campaign_polls$`Polling method[4]`)
campaign_polls$mode <- ifelse(str_detect(campaign_polls$mode, "telephone/IVR"),
                              "Telephone - IVR", campaign_polls$mode)
campaign_polls$mode <- ifelse(str_detect(campaign_polls$mode, "IVR") &
                                str_detect(campaign_polls$mode, "telephone - IVR") == FALSE,
                              "IVR", campaign_polls$mode)
campaign_polls$mode <- ifelse(str_detect(campaign_polls$mode, "telephone") &
                                str_detect(campaign_polls$mode, "telephone - IVR") == FALSE &
                                str_detect(campaign_polls$mode, "Online - telephone") == FALSE,
                              "Telephone", campaign_polls$mode)
campaign_polls$mode <- ifelse(str_detect(campaign_polls$mode, "online") &
                                str_detect(campaign_polls$mode, "Online - telephone") == FALSE,
                              "Online", campaign_polls$mode)


campaign_polls <- campaign_polls[, c("Polling_firm", "PollDate", parties, "SampleSize", "mode")]


# Pre-campaign polls
pre_polls <- wiki_tables[[3]]
pre_polls$Polling_firm <- pre_polls$`Polling firm`
pre_polls$PollDate <- pre_polls$`Last dateof polling[1]`
pre_polls$SampleSize <- str_remove_all(pre_polls$`Samplesize[3]`, "\\([0-9]\\/[0-9]\\)")
pre_polls$SampleSize <- str_remove_all(pre_polls$SampleSize, ",")
pre_polls$mode <- ifelse(str_detect(pre_polls$`Polling method[4]`, "online/telephone"),
                              "Online - telephone", pre_polls$`Polling method[4]`)
pre_polls$mode <- ifelse(str_detect(pre_polls$mode, "telephone/IVR"),
                              "Telephone - IVR", pre_polls$mode)
pre_polls$mode <- ifelse(str_detect(pre_polls$mode, "IVR") &
                                str_detect(pre_polls$mode, "telephone - IVR") == FALSE,
                              "IVR", pre_polls$mode)
pre_polls$mode <- ifelse(str_detect(pre_polls$mode, "telephone") &
                                str_detect(pre_polls$mode, "telephone - IVR") == FALSE &
                                str_detect(pre_polls$mode, "Online - telephone") == FALSE,
                              "Telephone", pre_polls$mode)
pre_polls$mode <- ifelse(str_detect(pre_polls$mode, "online") &
                                str_detect(pre_polls$mode, "Online - telephone") == FALSE,
                              "Online", pre_polls$mode)
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
pre_2023_polls <- wiki_tables_2023[[2]]
pre_2023_polls$Polling_firm <- pre_2023_polls$`Polling firm`
pre_2023_polls$PollDate <- pre_2023_polls$`Last dateof polling[1]`
pre_2023_polls$SampleSize <- str_remove_all(pre_2023_polls$`Samplesize[4]`, "\\([0-9]\\/[0-9]\\)")
pre_2023_polls$SampleSize <- str_remove_all(pre_2023_polls$SampleSize, ",")
pre_2023_polls$mode <- ifelse(str_detect(pre_2023_polls$`Polling method[5]`, "online/telephone"),
                         "Online - telephone", pre_2023_polls$`Polling method[5]`)
pre_2023_polls$mode <- ifelse(str_detect(pre_2023_polls$mode, "telephone/IVR"),
                         "telephone - IVR", pre_2023_polls$mode)
pre_2023_polls$mode <- ifelse(str_detect(pre_2023_polls$mode, "IVR") &
                           str_detect(pre_2023_polls$mode, "telephone - IVR") == FALSE,
                         "IVR", pre_2023_polls$mode)
pre_2023_polls$mode <- ifelse(str_detect(pre_2023_polls$mode, "telephone") &
                           str_detect(pre_2023_polls$mode, "telephone - IVR") == FALSE &
                           str_detect(pre_2023_polls$mode, "Online - telephone") == FALSE,
                         "telephone", pre_2023_polls$mode)
pre_2023_polls$mode <- ifelse(str_detect(pre_2023_polls$mode, "online") &
                           str_detect(pre_2023_polls$mode, "Online - telephone") == FALSE,
                         "telephone", pre_2023_polls$mode)

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

# New N_days, using Sept 30 as final date
N_days_2021 <- as.numeric(ymd("2021-09-30") - election_results$PollDate[1]) + 1


# Combine datasets
can_polls2 <- rbind(can_polls, pre_2023_polls)
can_polls2 <- can_polls2[complete.cases(can_polls2),]
can_polls2 <- subset(can_polls2, Other > 0)

# indexes for stan
can_polls2$pollster_id <- as.numeric(as.factor(can_polls2$Polling_firm))
can_polls2$mode_id <- as.numeric(as.factor(can_polls2$mode))

N_pollsters <- length(unique(can_polls2$pollster_id))
N_modes <- length(unique(can_polls2$mode))



# fit state space model for pooling the polls
state_space_all <- cmdstan_model("state_space_all_parties_non_centered_prediction.stan")
#state_space_prior <- cmdstan_model("state_space_all_parties_non_centered_prediction.stan")


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
  iter_warmup = 1000,
  iter_sampling = 1500,
  refresh = 100,
  max_treedepth = 15,
  adapt_delta = 0.9
)

# diagnoses
fit_all$cmdstan_diagnose()
fit_all$save_object(file = "fit_all_July19.RDS")


# extract info for plots
all_draws <- as_draws_df(fit_all$draws("xi"))
house_draws <- as_draws_df(fit_all$draws("delta"))
#mode_draws <- as_draws_df(fit_all$draws("gamma"))

all_trend <- data.frame(mu = apply(all_draws[,1:(ncol(all_draws)-3)], 2, mean),
                        ll = apply(all_draws[,1:(ncol(all_draws)-3)], 2, function(x) quantile(x, 0.025)),
                        uu = apply(all_draws[,1:(ncol(all_draws)-3)], 2, function(x) quantile(x, 0.975)),
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
       y = NULL) +
  scale_colour_manual(breaks = parties_all,
                      values = c("red", "blue", "orange", "lightblue", "green", "purple"),
                      name = "Party") +
  scale_fill_manual(breaks = parties_all,
                    values = c("red", "blue", "orange", "lightblue", "green", "purple"),
                    name = "Party") +
  scale_y_continuous(breaks = c(0, .2, .4, .6),
                     labels = c("0%", "20%", "40%", "60%")) +
  dark_theme_minimal()
ggsave(plot = vote_intention, filename = "vote_intention_2015_to_2021.png",
       height = 5.25, width = 9.2, units = "in")
vote_intention

house_effect_plot <- ggplot(house_effects) +
  geom_pointrange(mapping = aes(x = Polling_firm,
                                y = mu,
                                ymin = ll,
                                ymax = uu),
                  colour = "white") +
  geom_hline(yintercept = 0,
             colour = "orange",
             linetype = "dashed") +
  labs(x = NULL, 
       y = "Percent",
       title = "House effects: all parties",
       subtitle = "2015 to 2021") +
  scale_y_continuous(breaks = c(-0.05, 0.00, 0.05, 0.1),
                     labels = c("-5", "0", "5", "10")) +
  coord_flip() +
  facet_wrap(~party) +
  dark_theme_bw() 
ggsave(plot = house_effect_plot, filename = "house_effects_2015_to_2021.png",
       height = 5.25, width = 9.2, units = "in")
house_effect_plot

mode_effect_plot <- ggplot(mode_effects) +
  geom_pointrange(mapping = aes(x = mode,
                                y = mu,
                                ymin = ll,
                                ymax = uu),
                  colour = "white") +
  geom_hline(yintercept = 0,
             colour = "orange",
             linetype = "dashed") +
  labs(x = NULL, 
       y = "Percent",
       title = "Survey mode effects: all parties",
       subtitle = "2015 to 2021") +
  scale_y_continuous(breaks = c(-0.05, 0.00, 0.05, 0.1),
                     labels = c("-5", "0", "5", "10")) +
  coord_flip() +
  facet_wrap(~party) +
  dark_theme_bw() 
ggsave(plot = mode_effect_plot, filename = "mode_effects_2015_to_2021.png",
       height = 5.25, width = 9.2, units = "in")
mode_effect_plot




# Check that sums of means are close to 1
sums <- rep(0, N_days_2021)
for(i in 1:N_days_2021) sums[i] <- sum(all_trend$mu[all_trend$NumDays == i])


ggplot(data.frame(X = sums)) + 
  geom_density(mapping = aes(x = X))  +
  dark_theme_bw()



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
                          year_num = c(6, 5, 4, 3, 2, 1, NA, NA, NA))
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


mod_list <- list()
mod_list[[1]] <- lm(LPC ~ 1 + LPC_pop + logtime, data = raw_results[1:6,])
mod_list[[2]] <- lm(CPC ~ 1 + CPC_pop + logtime, data = raw_results[1:6,])
mod_list[[3]] <- lm(NDP ~ 1 + NDP_pop + logtime, data = raw_results[1:6,])
mod_list[[4]] <- lm(BQ ~ 1 + BQ_pop + logtime, data = raw_results[1:6,])
mod_list[[5]] <- lm(GPC ~ 1 + GPC_pop + logtime, data = raw_results[1:6,])
mod_list[[6]] <- lm(Other ~ 1 + Other_pop + logtime, data = raw_results[1:6,])


prior_df <- data.frame(mu = rep(NA, length(parties_all)),
                       sigma = rep(NA, length(parties_all)))
for(i in 1:length(parties_all)){
  prior_df[i,1] <- predict(mod_list[[i]], newdata = newdata)
  prior_df[i,2] <- summary(mod_list[[i]])$sigma
}

sum_mu <- sum(prior_df$mu)
prior_df$mu <- prior_df$mu / sum_mu #force sum-to-one constraint
prior_df$sigma <- prior_df$sigma / sum_mu #hacky adjustment to sigma 









predict(mod_LPC, newdata = newdata)
predict(mod_CPC, newdata = newdata)
predict(mod_NDP, newdata = newdata)
predict(mod_BQ, newdata = newdata)
predict(mod_GPC, newdata = newdata)
predict(mod_Other, newdata = newdata)

predict(mod_CPC, newdata = data.frame(logtime = log(24 + 46),
                                  CPC_pop = .285,
                                  UR = 7.5, 
                                  CPC_incumbent = 0))

pred_data = data.frame(time = 24,
                       LPC_pop = 36.5,
                       UR = 7.5, 
                       CPC_incumbent = 0)





sur_data <- list(
  
  N_elections = 6,
  N_parties = length(parties_all),
  P = 7,
  
  
  Y = as.matrix(raw_results[1:6, parties_all]),
  X = as.matrix(raw_results[1:6, c("LPC_pop", "CPC_pop", "NDP_pop", "BQ_pop", "GPC_pop", 
                                   "Other_pop", "logtime")]),
  
  X_mis = as.vector(newdata[,c("LPC_pop", "CPC_pop", "NDP_pop", "BQ_pop", "GPC_pop", 
                                       "Other_pop", "logtime")])
  
  
)



sur_model <- cmdstan_model("SUR.stan")
fit_sur <- sur_model$sample(
  data = sur_data,
  seed = 6319483,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1500,
  refresh = 100,
  max_treedepth = 15,
  adapt_delta = 0.95
)
