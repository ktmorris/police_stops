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

#################################################################
########## 2014 #################################################
#################################################################
#################################################################
# hills14 <- hills_pre_match %>%
#   mutate(treated = fd <= "2014-11-04") %>%
#   select(-v14, -v16)
# 
# ids <- hills14 %>%
#   mutate(id = row_number()) %>%
#   select(id, LALVOTERID)
# 
# X <- hills14 %>%
#   select(-LALVOTERID, -treated, -GEOID, -fd, -max_amount) %>%
#   mutate_at(vars(white, black, latino, asian, male, dem, rep), ~ ifelse(. == T, 1, 0)) %>%
#   mutate(reg_date = as.integer(reg_date))
# 
# 
# genout <- readRDS("temp/genout_hills_14.rds")
# 
# mout <- Matchby(Tr = hills14$treated, X = X,
#                 by = c(X$white,
#                        X$black,
#                        X$latino,
#                        X$asian,
#                        X$male,
#                        X$dem,
#                        X$rep), estimand = "ATT", Weight.matrix = genout, M = 2)
# 
# save(mout, file = "./temp/mout_hills_14.RData")
# 
# load("temp/mout_hills_14.RData")
# 
# matches <- data.table(voter = c(mout$index.control,
#                                 mout$index.treated),
#                       group = rep(mout$index.treated, 2),
#                       weight = rep(mout$weights, 2)) %>%
#   group_by(voter, group) %>%
#   summarize(weight = sum(weight)) %>%
#   ungroup()
# 
# 
# matches <- left_join(matches, ids, by = c("voter" = "id")) %>%
#   select(-voter) %>%
#   rename(voter = LALVOTERID)
# 
# matches <- left_join(matches, ids, by = c("group" = "id")) %>%
#   select(-group) %>%
#   rename(group = LALVOTERID) %>%
#   mutate(first_tr_year = as.Date("2014-11-04"))
# saveRDS(matches, "temp/matches_hills_14.rds")
# #################################################################
# ########## 2016 #################################################
# #################################################################
# #################################################################
# hills16 <- hills_pre_match %>%
#   filter(fd > "2014-11-04") %>%
#   mutate(treated = fd <= "2016-11-08") %>%
#   select(-v16)
# 
# ids <- hills16 %>%
#   mutate(id = row_number()) %>%
#   select(id, LALVOTERID)
# 
# X <- hills16 %>%
#   select(-LALVOTERID, -treated, -GEOID, -fd, -max_amount) %>%
#   mutate_at(vars(white, black, latino, asian, male, dem, rep), ~ ifelse(. == T, 1, 0)) %>%
#   mutate(reg_date = as.integer(reg_date))
# 
# 
# genout <- readRDS("temp/genout_hills_16.rds")
# 
# mout <- Matchby(Tr = hills16$treated, X = X,
#                 by = c(X$white,
#                        X$black,
#                        X$latino,
#                        X$asian,
#                        X$male,
#                        X$dem,
#                        X$rep), estimand = "ATT", Weight.matrix = genout, M = 2)
# 
# save(mout, file = "./temp/mout_hills_16.RData")
# 
# load("temp/mout_hills_16.RData")
# 
# matches <- data.table(voter = c(mout$index.control,
#                                 mout$index.treated),
#                       group = rep(mout$index.treated, 2),
#                       weight = rep(mout$weights, 2)) %>%
#   group_by(voter, group) %>%
#   summarize(weight = sum(weight)) %>%
#   ungroup()
# 
# 
# matches <- left_join(matches, ids, by = c("voter" = "id")) %>%
#   select(-voter) %>%
#   rename(voter = LALVOTERID)
# 
# matches <- left_join(matches, ids, by = c("group" = "id")) %>%
#   select(-group) %>%
#   rename(group = LALVOTERID) %>%
#   mutate(first_tr_year = as.Date("2016-11-08"))
# saveRDS(matches, "temp/matches_hills_16.rds")
# #################################################################
# ########## 2018 #################################################
# #################################################################
# #################################################################
# hills18 <- hills_pre_match %>% 
#   filter(fd > "2016-11-08") %>% 
#   mutate(treated = fd <= "2018-11-06")
# 
# ids <- hills18 %>% 
#   mutate(id = row_number()) %>% 
#   select(id, LALVOTERID)
# 
# X <- hills18 %>%
#   select(-LALVOTERID, -treated, -GEOID, -fd, -max_amount) %>%
#   mutate_at(vars(white, black, latino, asian, male, dem, rep), ~ ifelse(. == T, 1, 0)) %>%
#   mutate(reg_date = as.integer(reg_date))
# 
# 
# genout <- readRDS("temp/genout_hills_18.rds")
# 
# mout <- Matchby(Tr = hills18$treated, X = X,
#                 by = c(X$white,
#                        X$black,
#                        X$latino,
#                        X$asian,
#                        X$male,
#                        X$dem,
#                        X$rep), estimand = "ATT", Weight.matrix = genout, M = 2)
# 
# save(mout, file = "./temp/mout_hills_18.RData")
# 
# load("temp/mout_hills_18.RData")
# 
# matches <- data.table(voter = c(mout$index.control,
#                                 mout$index.treated),
#                       group = rep(mout$index.treated, 2),
#                       weight = rep(mout$weights, 2)) %>% 
#   group_by(voter, group) %>% 
#   summarize(weight = sum(weight)) %>% 
#   ungroup()
# 
# 
# matches <- left_join(matches, ids, by = c("voter" = "id")) %>%
#   select(-voter) %>%
#   rename(voter = LALVOTERID)
# 
# matches <- left_join(matches, ids, by = c("group" = "id")) %>%
#   select(-group) %>%
#   rename(group = LALVOTERID) %>% 
#   mutate(first_tr_year = as.Date("2018-11-06"))
# saveRDS(matches, "temp/matches_hills_18.rds")
##############################################################
##############################################################
##############################################################
##############################################################

matches <- rbindlist(lapply(c(
  "temp/matches_hills_14.rds",
  "temp/matches_hills_16.rds",
  "temp/matches_hills_18.rds"), readRDS))

hist <- readRDS("raw_data/fl_l2_hills/fl_l2_history_hills.rds") %>%
  select(-state) %>%
  pivot_longer(c(starts_with("Gener"), starts_with("Loc")), names_to = "year", values_to = "to") %>%
  mutate(to = to == "Y")

mh <- matches

matches <- left_join(mh, hist %>% 
                       filter(grepl("Gene", year)), by = c("voter" = "LALVOTERID")) %>%
  mutate(year = as.Date(gsub("General_|Local_or_Municipal_", "", year), "%Y_%m_%d"),
         treated = voter == group)


periods <- fread("raw_data/period_lu.csv") %>% 
  mutate_at(vars(first_tr_year, year), as.Date)

matches <- left_join(matches, periods)

matches <- left_join(matches,
                     hills_pre_match %>%
                       select(-GEOID, -fd, -max_amount),
                     by = c("voter" = "LALVOTERID"))

matches <- left_join(matches,
                     hills_pre_match %>%
                       select(LALVOTERID, max_amount, fd),
                     by = c("group" = "LALVOTERID")) %>% 
  mutate(post = period >= 0.5)

ll <- matches %>%
  filter(first_tr_year - fd <= 90) %>%
  group_by(treated, period, black) %>%
  summarize(to = weighted.mean(to, weight)) %>% 
  mutate(treated = ifelse(treated, "Treated", "Control"),
         black = ifelse(black, "Black Voters", "Non-Black Voters"))

ll$treated <- factor(ll$treated, levels = c("Treated", "Control"))

ggplot(data = ll) + 
  facet_grid(~ black) +
  geom_rect(aes(xmin = 0.5-.125, xmax = 0.5, ymin = 0, ymax = Inf),
            alpha = 0.03, color = "black", fill = "yellow") +
  geom_line(data =ll, aes(x = period, y = to, linetype = treated)) +
  geom_point(data = ll, aes(x = period, y = to, shape = treated)) +
  scale_x_continuous(minor_breaks = seq(-3.5, 3.5, 1),
                     breaks = seq(-3.5, 3.5, 1),
                     labels = c("4 Elections\nBefore",
                                "3 Elections\nBefore",
                                "2 Elections\nBefore",
                                "1 Election\nBefore",
                                "1 Election\nAfter",
                                "2 Elections\nAfter",
                                "3 Elections\nAfter",
                                "4 Elections\nAfter")) +
  theme_bc(base_family = "LM Roman 10") +
  scale_y_continuous(labels = percent) +
  labs(x = "Period", y = "Turnout",
       linetype = "Treatment Group",
       shape = "Treatment Group",
       caption = "Treatment occurs inside of yellow band.") +
  coord_cartesian(ylim = c(0.05, 0.75))
##########################
ll <- matches %>%
  group_by(treated, period, black) %>%
  summarize(to = weighted.mean(to, weight)) %>% 
  mutate(treated = ifelse(treated, "Treated", "Control"),
         black = ifelse(black, "Black Voters", "Non-Black Voters"))

ll$treated <- factor(ll$treated, levels = c("Treated", "Control"))

ggplot(data = ll) + 
  facet_grid(~ black) +
  geom_rect(aes(xmin = -.49, xmax = 0.5, ymin = 0, ymax = Inf),
            alpha = 0.03, color = "black", fill = "yellow") +
  geom_line(data =ll, aes(x = period, y = to, linetype = treated)) +
  geom_point(data = ll, aes(x = period, y = to, shape = treated)) +
  scale_x_continuous(minor_breaks = seq(-3.5, 3.5, 1),
                     breaks = seq(-3.5, 3.5, 1),
                     labels = c("4 Elections\nBefore",
                                "3 Elections\nBefore",
                                "2 Elections\nBefore",
                                "1 Election\nBefore",
                                "1 Election\nAfter",
                                "2 Elections\nAfter",
                                "3 Elections\nAfter",
                                "4 Elections\nAfter")) +
  theme_bc(base_family = "LM Roman 10") +
  scale_y_continuous(labels = percent) +
  labs(x = "Period", y = "Turnout",
       linetype = "Treatment Group",
       shape = "Treatment Group",
       caption = "Treatment occurs inside of yellow band.") +
  coord_cartesian(ylim = c(0.05, 0.75))


############### overall

dat1 <- matches
dat2 <- filter(matches, first_tr_year - fd <= 90)

m1 <- to ~ treated * post + as.factor(year)
m2 <- to ~ treated * post * black + as.factor(year)

models1 <- lapply(c(m1, m2), function(f){
  m <- lm(f, data = dat1,
          weight = dat1$weight)
})

models2 <- lapply(c(m1, m2), function(f){
  m <- lm(f, data = dat2,
          weight = dat2$weight)
})


ses_cl <- list(
  summary(lm.cluster(formula = m1, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
  summary(lm.cluster(formula = m2, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
  summary(lm.cluster(formula = m1, data = dat2, weights = dat2$weight, cluster = dat2$group))[ , 2],
  summary(lm.cluster(formula = m2, data = dat2, weights = dat2$weight, cluster = dat2$group))[ , 2]
  )


stargazer(models1, models2,
          type = "text",
          column.labels = c("Stopped Any Time", "Stopped within 90 Days of Election"),
          column.separate = c(2, 2),
          omit.stat = c("f", "ser"),
          se = ses_cl,
          omit = c("as.fac"),
          covariate.labels = c("Treated",
                               "Post Treatment",
                               "Black",
                               "Treated X Post Treatment",
                               "Treated X Black",
                               "Post Treatment X Black",
                               "Treated X Post Treatment X Black"),
          table.layout = "-cm#-t-a-s-n")

############### first election

dat1 <- filter(matches, period <= 0.5)
dat2 <- filter(matches, first_tr_year - fd <= 90, period <= 0.5)

m1 <- to ~ treated * post + as.factor(year)
m2 <- to ~ treated * post * black + as.factor(year)

models1 <- lapply(c(m1, m2), function(f){
  m <- lm(f, data = dat1,
          weight = dat1$weight)
})

models2 <- lapply(c(m1, m2), function(f){
  m <- lm(f, data = dat2,
          weight = dat2$weight)
})


ses_cl <- list(
  summary(lm.cluster(formula = m1, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
  summary(lm.cluster(formula = m2, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
  summary(lm.cluster(formula = m1, data = dat2, weights = dat2$weight, cluster = dat2$group))[ , 2],
  summary(lm.cluster(formula = m2, data = dat2, weights = dat2$weight, cluster = dat2$group))[ , 2]
)


stargazer(models1, models2,
          type = "text",
          column.labels = c("Stopped Any Time", "Stopped within 90 Days of Election"),
          column.separate = c(2, 2),
          omit.stat = c("f", "ser"),
          se = ses_cl,
          omit = c("as.fac"),
          covariate.labels = c("Treated",
                               "Post Treatment",
                               "Black",
                               "Treated X Post Treatment",
                               "Treated X Black",
                               "Post Treatment X Black",
                               "Treated X Post Treatment X Black"),
          table.layout = "-cm#-t-a-s-n")
#############################################################################
#############################################################################
#############################################################################

##########################

demos <- matches %>% 
  filter(first_tr_year - fd <= 90) %>%
  group_by(treated) %>% 
  summarize_at(vars(white, black, latino, asian, median_income, age, male,
                    unem, some_college, dem, rep, reg_date), mean)
