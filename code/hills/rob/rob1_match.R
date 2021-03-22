library(Matching)
library(data.table)
library(scales)
library(kableExtra)
library(tidyverse)
require(snow)
require(parallel)


hills_pre_match <- readRDS("temp/hills_pre_match.rds")

############## 2014

hills14_t <- filter(hills_pre_match, first_tr_year == "2014-11-04") %>% 
  mutate(treated = 1) %>% 
  select(-pre_stops_c)

hills14_c <- filter(hills_pre_match, first_tr_year == "2016-11-08",
                    !(voter_id %in% hills14_t$voter_id)) %>% 
  mutate(treated = 0) %>% 
  select(-pre_stops) %>% 
  rename(pre_stops = pre_stops_c)


hills14 <- bind_rows(hills14_t, hills14_c) %>%  
  mutate(first_tr_year = 1) %>% 
  select(-v14, -v16, -pre) %>% 
  rename(v1 = v08,
         v2 = v10,
         v3 = v12)

############## 2016

hills16_t <- filter(hills_pre_match, first_tr_year == "2016-11-08") %>% 
  mutate(treated = 1) %>% 
  select(-pre_stops_c)

hills16_c <- filter(hills_pre_match, first_tr_year == "2018-11-06",
                    !(voter_id %in% hills16_t$voter_id)) %>% 
  mutate(treated = 0) %>% 
  select(-pre_stops) %>% 
  rename(pre_stops = pre_stops_c)


hills16 <- bind_rows(hills16_t, hills16_c) %>%  
  mutate(first_tr_year = 2) %>% 
  rename(v1 = v10,
         v2 = v12,
         v3 = v14)

############## 2018

hills18_t <- filter(hills_pre_match, first_tr_year == "2018-11-06") %>% 
  mutate(treated = 1) %>% 
  select(-pre_stops_c)

hills18_c <- filter(hills_pre_match, first_tr_year == "2020-11-03",
                    !(voter_id %in% hills18_t$voter_id)) %>% 
  mutate(treated = 0) %>% 
  select(-pre_stops) %>% 
  rename(pre_stops = pre_stops_c)


hills18 <- bind_rows(hills18_t, hills18_c) %>%  
  mutate(first_tr_year = 3) %>% 
  rename(v1 = v12,
         v2 = v14,
         v3 = v16)

###########################################
pre <- bind_rows(hills14, hills16, hills18)
ids <- pre %>%
  mutate(id = row_number()) %>%
  select(id, voter_id)

X <- pre %>%
  select(-voter_id, -treated, -GEOID, -amount_paid, -last_date,
         -name_first, -name_last, -birth_date, -v08, -v16, -pre, -v10) %>% 
  mutate_at(vars(white, black, latino, asian, male, dem, rep, v1, v2, v3), ~ ifelse(. == T, 1, 0)) %>% 
  mutate(reg_date = as.integer(reg_date)) %>% 
  select(first_tr_year, everything())

genout <- readRDS("temp/genout_hills_y.rds")


mout <- Matchby(Tr = pre$treated, X = X,
                by = c(X$first_tr_year,
                       X$white,
                       X$black,
                       X$latino,
                       X$asian,
                       X$male,
                       X$dem,
                       X$rep), estimand = "ATT", Weight.matrix = genout, M = 1, ties = T,
                exact = c(T, rep(F, 18)))


save(mout, file = "./temp/mout_hills_y.RData")

load("temp/mout_hills_y.RData")

matches <- data.table(voter = c(mout$index.control,
                                mout$index.treated),
                      group = rep(mout$index.treated, 2),
                      weight = rep(mout$weights, 2)) %>%
  group_by(voter, group) %>%
  summarize(weight = sum(weight)) %>%
  ungroup()


matches <- left_join(matches, ids, by = c("voter" = "id")) %>%
  select(-voter) %>%
  rename(voter = voter_id)

matches <- left_join(matches, ids, by = c("group" = "id")) %>%
  select(-group) %>%
  rename(group = voter_id)

saveRDS(matches, "temp/matches_hills_y.rds")

balance <- MatchBalance(treated ~ white + black + latino + asian + male +
                          dem + rep + age + reg_date + pre_stops + v1 + v2 + v3 +
                          median_income + some_college + unem,
                        data = pre, match.out = mout)

saveRDS(balance, "temp/balance_table.rds")
# 
# varnames <- c("white", "black", "latino", "asian", "male",
#               "dem", "rep", "age", "reg_date", "pre_12", "v1", "v2", "v3",
#               "median_income", "some_college", "unem")
# 
# balance <- readRDS("temp/balance_table.rds")
# 
# TrMean <- c()
# PreMean <- c()
# PreQQmed <- c()
# PreQQmean <- c()
# PreQQmax <- c()
# PostMean <- c()
# PostQQmed <- c()
# PostQQmean <- c()
# PostQQmax <- c()
# 
# for(i in c(1:length(balance$BeforeMatching))){
#   TrMean <- unlist(c(TrMean, balance$BeforeMatching[[i]][3][1]))
#   PreMean <- unlist(c(PreMean, balance$BeforeMatching[[i]][4][1]))
#   PreQQmed <- unlist(c(PreQQmed, balance$BeforeMatching[[i]]$qqsummary[2]))
#   PreQQmean <- unlist(c(PreQQmean, balance$BeforeMatching[[i]]$qqsummary[1]))
#   PreQQmax <- unlist(c(PreQQmax, balance$BeforeMatching[[i]]$qqsummary[3]))
#   
#   PostMean <- unlist(c(PostMean, balance$AfterMatching[[i]][4][1]))
#   PostQQmed <- unlist(c(PostQQmed, balance$AfterMatching[[i]]$qqsummary[2]))
#   PostQQmean <- unlist(c(PostQQmean, balance$AfterMatching[[i]]$qqsummary[1]))
#   PostQQmax <- unlist(c(PostQQmax, balance$AfterMatching[[i]]$qqsummary[3]))
# }
# 
# 
# 
# df <- data.frame("TrMean" = TrMean,
#                  "TrMean2" = TrMean,
#                  "PreMean" = PreMean,
#                  "PreQQmed" = PreQQmed,
#                  "PreQQmean" = PreQQmean,
#                  "PreQQmax" = PreQQmax,
#                  "PostMean" = PostMean,
#                  "PostQQmed" = PostQQmed,
#                  "PostQQmean" = PostQQmean,
#                  "PostQQmax" = PostQQmax,
#                  "names" = varnames) %>%
#   mutate(change_mean = 1 - (abs(TrMean - PostMean) / abs(TrMean - PreMean)),
#          change_eqqmed = 1 - abs(PostQQmed / PreQQmed),
#          change_eqqmean = 1 - abs(PostQQmean / PreQQmean),
#          change_eqqmax = 1 - abs(PostQQmax / PreQQmax)) %>%
#   mutate_at(vars(TrMean, PreMean, TrMean2, PostMean), ~ comma(round(., 3), accuracy = .001)) %>%
#   mutate_at(vars(change_mean, change_eqqmed, change_eqqmean, change_eqqmax),
#             ~ format(round(. * 100, 2), nsmall = 2)) %>%
#   filter(names != "voted_primary")
# 
# 
# ####
# 
# df <- full_join(df,
#                 fread("./raw_data/var_orders.csv"),
#                 by = c("names" = "variable")) %>%
#   arrange(order) %>% 
#   select(name, TrMean, PreMean, TrMean2, PostMean, change_mean, change_eqqmed, change_eqqmean, change_eqqmax) %>%
#   filter(!is.na(TrMean))
# 
# 
# df <- df %>% 
#   mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
#             ~ ifelse(name == "Registration Date",
#                      as.character(as.integer(gsub(",", "", .)) + as.Date("2000-01-01")),
#                      .)) %>% 
#   mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
#             ~ ifelse(name == "Age" | grepl("Pre", name),
#                      round(as.numeric(.), 1),
#                      .)) %>%
#   mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
#             ~ ifelse(name == "Median Income",
#                      dollar(as.numeric(gsub(",", "", .)), 1),
#                      .)) %>%
#   mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
#             ~ ifelse(name == "Income", dollar(round(as.numeric(gsub(",", "", .)))), .)) %>%
#   mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
#             ~ ifelse(substring(name, 1, 1) == "%"|grepl("Turnout|Unempl", name),
#                      percent(as.numeric(.), accuracy = .1), .)) %>%
#   filter(!is.na(name)) %>% 
#   mutate_all(~ gsub("[%]", paste0("\\\\", "%"), .)) %>% 
#   mutate_all(~ gsub("[$]", paste0("\\\\", "$"), .))
# 
# colnames(df) <- c("", "Treated", "Control", "Treated", "Control", "Mean Diff", "eQQ Med", "eQQ Mean", "eQQ Max")
# 
# saveRDS(df, "./temp/balance_table_y.rds")
