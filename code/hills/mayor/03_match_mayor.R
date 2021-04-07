
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


hills_pre_match <- readRDS("temp/hills_pre_match_mayor.rds")

##############################################

hills15_t <- filter(hills_pre_match, first_tr_year == "2015-03-03") %>% 
  mutate(treated = last_date <= "2015-03-03",
         first_tr_year = 1) %>% 
  rename(v1 = v07,
         v2 = v11)

############## 2019

hills19_t <- filter(hills_pre_match, first_tr_year == "2019-03-05") %>% 
  mutate(treated = last_date <= "2019-03-05",
         first_tr_year = 2) %>% 
  rename(v1 = v11,
         v2 = v15)

###########################################
pre <- bind_rows(hills15_t, hills19_t)
saveRDS(pre, "temp/real_pre_match_hills_mayor.rds")

ids <- pre %>%
  mutate(id = row_number()) %>%
  select(id, voter_id, first_tr_year)

X <- pre %>%
  select(-voter_id, -treated, -GEOID, -amount_paid, -last_date,
         -pre, -v15, -v19, -v07) %>% 
  mutate_at(vars(white, black, latino, asian, male, dem, rep, v1, v2, paid), ~ ifelse(. == T, 1, 0)) %>% 
  mutate(reg_date = as.integer(reg_date)) %>% 
  select(first_tr_year, paid, civil, everything())


genout <- readRDS("temp/genout_hills_y_mayor.rds")

mout <- Matchby(Tr = pre$treated, X = X,
                by = c(X$first_tr_year,
                       X$white,
                       X$black,
                       X$latino,
                       X$asian,
                       X$male,
                       X$dem,
                       X$rep), estimand = "ATT", Weight.matrix = genout, M = 1, ties = T,
                exact = c(rep(T, 3), rep(F, 17)))


save(mout, file = "./temp/mout_hills_y_mayor.RData")

load("temp/mout_hills_y_mayor.RData")

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

saveRDS(matches, "temp/matches_hills_y_mayor.rds")