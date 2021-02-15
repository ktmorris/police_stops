library(Matching)
library(data.table)
library(scales)
library(kableExtra)
library(tidyverse)
require(snow)
require(parallel)



#####
hills_pre_match <- readRDS("temp/hills_pre_match.rds") %>% 
  ungroup()

##########

ids <- hills_pre_match %>% 
  mutate(id = row_number()) %>% 
  select(id, LALVOTERID, fd)

# X <- hills_pre_match %>%
#   select(-LALVOTERID, -treated, -GEOID, -fd) %>% 
#   mutate_at(vars(white, black, latino, asian, male, dem, rep), ~ ifelse(. == T, 1, 0)) %>% 
#   mutate(reg_date = as.integer(reg_date))
# 
# 
# genout <- readRDS("./temp/genout_hills_civil_05.rds")
# 
# mout <- Matchby(Tr = hills_pre_match$treated, X = X,
#                 by = c(X$white,
#                        X$black,
#                        X$latino,
#                        X$asian,
#                        X$male,
#                        X$dem,
#                        X$rep), estimand = "ATT", Weight.matrix = genout, M = 1)
# 
# save(mout, file = "./temp/mout_hills_05.RData")

load("temp/mout_hills_05.RData")
#######################################

matches <- data.table(voter = c(mout$index.control,
                                mout$index.treated),
                      group = rep(mout$index.treated, 2),
                      weight = rep(mout$weights, 2)) %>% 
  group_by(voter, group) %>% 
  summarize(weight = sum(weight)) %>% 
  ungroup()


matches <- left_join(matches, ids, by = c("voter" = "id")) %>% 
  select(-voter, -fd) %>% 
  rename(voter = LALVOTERID)

matches <- left_join(matches, ids, by = c("group" = "id")) %>% 
  select(-group) %>% 
  rename(group = LALVOTERID)

#############################

hist <- readRDS("raw_data/fl_l2_hills/fl_l2_history_hills.rds") %>% 
  select(-state) %>% 
  pivot_longer(starts_with("Gener"), names_to = "year", values_to = "to") %>% 
  mutate(to = to == "Y")

matches <- matches %>% 
  mutate(year = as.Date(gsub("General_", "", year), "%Y_%m_%d"),
         post = year > fd) %>%
  arrange(voter, group, post, year) %>% 
  group_by(voter, group, post) %>% 
  mutate(p = row_number()) %>% 
  arrange(voter, group, post, desc(year)) %>% 
  group_by(voter, group, post) %>% 
  mutate(p2 = row_number()) %>% 
  filter(fd < as.Date("2018-11-06")) %>% 
  ungroup()


matches <- matches %>% 
  mutate(period = ifelse(post, p, -1*p2),
         period = ifelse(period > 0, period - 0.5, period + 0.5))


matches <- left_join(matches,
                     matches %>% 
                       filter(period == 1) %>% 
                       rename(first_tr_year = year) %>% 
                       select(voter, group, first_tr_year))

matches <- left_join(matches,
                     hills_pre_match %>% 
                       select(-GEOID, -treated, -fd),
                     by = c("voter" = "LALVOTERID"))

ll <- matches %>% 
  mutate(treated = voter == group) %>% 
  group_by(treated, period) %>% 
  summarize(to = mean(to))


ggplot(ll, aes(x = period, y = to, color = treated)) + geom_line() + geom_point() +
  scale_x_continuous(minor_breaks = seq(-4.5, 4.5, 1),
                     breaks = seq(-4.5, 4.5, 1)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bc()


m1 <- lm.cluster(to ~ I(voter == group)*post*white + as.factor(year), data = matches,
                 cluster = matches$group)

summary(m1)

##################
h <- matches %>% 
  filter(voter != group) %>% 
  group_by(voter) %>% 
  tally()
