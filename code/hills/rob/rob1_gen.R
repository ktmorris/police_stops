
## needs to be run on nyu hpc or its way too slow
## set seed for reproducibility. this is a random 5-digit number: floor(runif(1, min = 10000, max = 99999))
## on nyu hpc with 50 processors, 50gb RAM: Total run time : 66 hours 14 minutes and 25 seconds
set.seed(45251)

library(Matching)
library(data.table)
library(scales)
library(kableExtra)
library(tidyverse)
require(snow)
require(parallel)


Sys.info()

NodeFile = Sys.getenv("MY_HOSTFILE")

print(NodeFile)

readLines(NodeFile)

cl<-makeCluster(c(readLines(NodeFile)), type="SOCK")
cl


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

samp <- pre %>% 
  group_by(treated, first_tr_year) %>% 
  sample_frac(0.05) %>% 
  ungroup()

match_data <- samp %>% 
  select(-voter_id, -treated, -GEOID, -amount_paid, -last_date,
         -name_first, -name_last, -birth_date, -v08, -v16, -pre, -v10) %>% 
  mutate_at(vars(white, black, latino, asian, male, dem, rep, v1, v2, v3), ~ ifelse(. == T, 1, 0)) %>% 
  mutate(reg_date = as.integer(reg_date)) %>% 
  select(first_tr_year, everything())

genout <- GenMatch(Tr = samp$treated, X = match_data, replace = T, cluster = cl, pop.size = 1000,
                   exact = c(T, rep(F, 18)))

saveRDS(genout, "temp/genout_hills_y.rds")


