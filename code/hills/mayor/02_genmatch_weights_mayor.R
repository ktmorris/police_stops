
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


Sys.info()

NodeFile = Sys.getenv("MY_HOSTFILE")

print(NodeFile)

readLines(NodeFile)

cl<-makeCluster(c(readLines(NodeFile)), type="SOCK")
cl

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

samp <- pre %>% 
  group_by(treated, first_tr_year) %>% 
  sample_frac(0.05) %>% 
  ungroup()

match_data <- samp %>% 
  select(-voter_id, -treated, -GEOID, -amount_paid, -last_date,
         -v15, -v19, -v07, -reg_date) %>% 
  mutate_at(vars(white, black, latino, asian, male, dem, rep, v1, v2, paid), ~ ifelse(. == T, 1, 0)) %>% 
  select(first_tr_year, paid, civil, tampa_pd, v1, v2, everything())


match_data <- match_data[complete.cases(match_data), ]

genout <- GenMatch(Tr = samp$treated, X = match_data, replace = T, pop.size = 1000,
                   exact = c(rep(T, 6), rep(F, 14)), cluster = cl)

saveRDS(genout, "temp/genout_hills_y_mayor.rds")


