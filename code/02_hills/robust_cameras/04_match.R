
## needs to be run on nyu hpc or its way too slow
## set seed for reproducibility. this is a random 5-digit number: floor(runif(1, min = 10000, max = 99999))
set.seed(45251)

library(Matching)
library(data.table)
library(scales)
library(kableExtra)
library(tidyverse)
require(snow)
require(parallel)


hills_pre_match <- readRDS("temp/hills_pre_match_cam.rds")

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
  select(-v14, -v16) %>% 
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

saveRDS(pre, "temp/real_pre_match_hills_cam.rds")

ids <- pre %>%
  mutate(id = row_number()) %>%
  select(id, voter_id, first_tr_year)

X <- pre %>%
  select(-voter_id, -treated, -GEOID, -amount_paid, -last_date,
         -v08, -v16, -v10, -reg_date) %>% 
  mutate_at(vars(white, black, latino, asian, male, dem, rep, v1, v2, v3, paid), ~ ifelse(. == T, 1, 0)) %>% 
  select(first_tr_year, paid, civil, tampa_pd, v1, v2, v3, everything())

genout <- readRDS("temp/genout_hills_yem_cam.rds")


mout <- Matchby(Tr = pre$treated, X = X,
                by = c(X$first_tr_year,
                       X$white,
                       X$black,
                       X$latino,
                       X$asian,
                       X$male,
                       X$dem,
                       X$rep), estimand = "ATT", Weight.matrix = genout, M = 1,
                exact = c(rep(T, 7), rep(F, 14)), ties = T)



save(mout, file = "./temp/mout_hills_y_cam.RData")

load("temp/mout_hills_y_cam.RData")

matches <- data.table(voter = c(mout$index.control,
                                mout$index.treated),
                      group = rep(mout$index.treated, 2),
                      weight = rep(mout$weights, 2)) %>%
  group_by(voter, group) %>%
  summarize(weight = sum(weight)) %>%
  ungroup()


matches <- left_join(matches, ids, by = c("voter" = "id")) %>%
  select(-voter, -first_tr_year) %>%
  rename(voter = voter_id)

matches <- left_join(matches, ids, by = c("group" = "id")) %>%
  select(-group) %>%
  rename(group = voter_id)

saveRDS(matches, "temp/matches_hills_yem_cam.rds")
