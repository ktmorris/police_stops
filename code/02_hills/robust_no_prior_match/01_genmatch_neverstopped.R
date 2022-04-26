
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


hills_pre_match <- readRDS("temp/hills_pre_match.rds") %>% 
  mutate(tract = as.numeric(substring(GEOID, 1, 11)))
############## 2014

hills14 <- hills_pre_match  %>% 
  filter(fd <= "2014-11-04" |
           fd > "2018-11-06") %>% 
  mutate(treated = fd <= "2014-11-04") %>% 
  select(-v14, -v16, -pre, -tract)

samp <- hills14 %>% 
  group_by(treated) %>% 
  sample_frac(0.05) %>% 
  ungroup()

match_data <- samp %>% 
  select(-voter_id, -treated, -GEOID, -fd, -max_amount) %>% 
  mutate_at(vars(white, black, latino, asian, male, dem, rep), ~ ifelse(. == T, 1, 0)) %>% 
  mutate(reg_date = as.integer(reg_date))

genout <- GenMatch(Tr = samp$treated, X = match_data, replace = T, cluster = cl, pop.size = 1000)

saveRDS(genout, "temp/genout_hills_14.rds")
############## 2016

hills16 <- hills_pre_match %>% 
  filter(fd > "2014-11-04",
         fd <= "2016-11-08" |
           fd > "2018-11-06") %>% 
  mutate(treated = fd <= "2016-11-08") %>% 
  select(-v16, -pre, -tract)

samp <- hills16 %>% 
  group_by(treated) %>% 
  sample_frac(0.05) %>% 
  ungroup()

match_data <- samp %>% 
  select(-voter_id, -treated, -GEOID, -fd, -max_amount) %>% 
  mutate_at(vars(white, black, latino, asian, male, dem, rep), ~ ifelse(. == T, 1, 0)) %>% 
  mutate(reg_date = as.integer(reg_date))

genout <- GenMatch(Tr = samp$treated, X = match_data, replace = T, cluster = cl, pop.size = 1000)

saveRDS(genout, "temp/genout_hills_16.rds")

############## 2018

hills18 <- hills_pre_match %>% 
  filter(fd > "2016-11-08") %>% 
  mutate(treated = fd <= "2018-11-06") %>% 
  select(-pre, -tract)

samp <- hills18 %>% 
  group_by(treated) %>% 
  sample_frac(0.05) %>% 
  ungroup()

match_data <- samp %>% 
  select(-voter_id, -treated, -GEOID, -fd, -max_amount) %>% 
  mutate_at(vars(white, black, latino, asian, male, dem, rep), ~ ifelse(. == T, 1, 0)) %>% 
  mutate(reg_date = as.integer(reg_date))

genout <- GenMatch(Tr = samp$treated, X = match_data, replace = T, cluster = cl, pop.size = 1000)

saveRDS(genout, "temp/genout_hills_18.rds")


