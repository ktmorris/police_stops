
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

###########################################
pre <- readRDS("temp/real_pre_match_hills.rds")

ids <- pre %>%
  mutate(id = row_number()) %>%
  select(id, voter_id, first_tr_year)

X <- pre %>%
  select(-voter_id, -treated, -GEOID, -amount_paid, -last_date,
         -v08, -v16, -v10, -reg_date, -v1, -v2, -v3) %>% 
  mutate_at(vars(white, black, latino, asian, male, dem, rep, paid), ~ ifelse(. == T, 1, 0)) %>% 
  select(first_tr_year, paid, civil, tampa_pd, everything())

genout <- readRDS("temp/genout_hills_no_prior.rds")


mout <- Matchby(Tr = pre$treated, X = X,
                by = c(X$first_tr_year,
                       X$white,
                       X$black,
                       X$latino,
                       X$asian,
                       X$male,
                       X$dem,
                       X$rep), estimand = "ATT", Weight.matrix = genout, M = 1,
                exact = c(rep(T, 4), rep(F, 14)), ties = T)



save(mout, file = "./temp/mout_hills_no_prior.RData")

load("temp/mout_hills_no_prior.RData")

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

saveRDS(matches, "temp/matches_hills_no_prior.rds")
