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
hills14 <- hills_pre_match  %>%
  filter(fd <= "2014-11-04" |
           fd > "2018-11-06") %>%
  mutate(treated = fd <= "2014-11-04") %>%
  select(-v14, -v16, -pre)

pot_con_14 <- filter(hills14, !treated) %>%
  mutate(first_tr_year = as.Date("2014-11-04"))

# ids <- hills14 %>%
#   mutate(id = row_number()) %>%
#   select(id, voter_id)
# 
# X <- hills14 %>%
#   select(-voter_id, -treated, -GEOID, -fd, -max_amount) %>%
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
#                        X$rep), estimand = "ATT", Weight.matrix = genout, M = 2, ties = T)
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
#   rename(voter = voter_id)
# 
# matches <- left_join(matches, ids, by = c("group" = "id")) %>%
#   select(-group) %>%
#   rename(group = voter_id) %>%
#   mutate(first_tr_year = as.Date("2014-11-04"))
# saveRDS(matches, "temp/matches_hills_14.rds")
#################################################################
########## 2016 #################################################
#################################################################
#################################################################
hills16 <- hills_pre_match %>%
  filter(fd > "2014-11-04",
         fd <= "2016-11-08" |
           fd > "2018-11-06") %>%
  mutate(treated = fd <= "2016-11-08") %>%
  select(-v16, -pre)

pot_con_16 <- filter(hills16, !treated) %>%
  mutate(first_tr_year = as.Date("2016-11-08"))

# ids <- hills16 %>%
#   mutate(id = row_number()) %>%
#   select(id, voter_id)
# 
# X <- hills16 %>%
#   select(-voter_id, -treated, -GEOID, -fd, -max_amount) %>%
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
#                        X$rep), estimand = "ATT", Weight.matrix = genout, M = 2, ties = T)
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
#   rename(voter = voter_id)
# 
# matches <- left_join(matches, ids, by = c("group" = "id")) %>%
#   select(-group) %>%
#   rename(group = voter_id) %>%
#   mutate(first_tr_year = as.Date("2016-11-08"))
# saveRDS(matches, "temp/matches_hills_16.rds")
#################################################################
########## 2018 #################################################
#################################################################
#################################################################
hills18 <- hills_pre_match %>% 
  filter(fd > "2016-11-08") %>% 
  mutate(treated = fd <= "2018-11-06") %>% 
  select(-pre)

pot_con_18 <- filter(hills18, !treated) %>% 
  mutate(first_tr_year = as.Date("2018-11-06"))

# ids <- hills18 %>% 
#   mutate(id = row_number()) %>% 
#   select(id, voter_id)
# 
# X <- hills18 %>%
#   select(-voter_id, -treated, -GEOID, -fd, -max_amount) %>%
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
#                        X$rep), estimand = "ATT", Weight.matrix = genout, M = 2, ties = T)
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
#   rename(voter = voter_id)
# 
# matches <- left_join(matches, ids, by = c("group" = "id")) %>%
#   select(-group) %>%
#   rename(group = voter_id) %>% 
#   mutate(first_tr_year = as.Date("2018-11-06"))
# saveRDS(matches, "temp/matches_hills_18.rds")
##############################################################
##############################################################
##############################################################
##############################################################

matches_wide <- rbindlist(lapply(c(
  "temp/matches_hills_14.rds",
  "temp/matches_hills_16.rds",
  "temp/matches_hills_18.rds"), readRDS))

pot_con <- bind_rows(pot_con_14, pot_con_16, pot_con_18) %>% 
  select(first_tr_year, voter_id, black)

cleanup(c("matches_wide", "hills_pre_match", "pot_con"))

hist <- readRDS("temp/full_raw_coded_hills_w_bgs.rds") %>%
  select(voter_id, starts_with("v1")) %>%
  pivot_longer(starts_with("v1"), names_to = "year", values_to = "to")

elec_dates <- fread("raw_data/election_dates.csv")

hist <- left_join(hist, elec_dates) %>% 
  select(-year) %>% 
  rename(year = date) %>% 
  mutate(year = as.Date(year))


matches <- left_join(matches_wide, hist %>% 
                       filter(year(year) %% 2 == 0), by = c("voter" = "voter_id"))

pot_con <- left_join(pot_con, hist %>% 
                       filter(year(year) %% 2 == 0), by = c("voter_id" = "voter_id"))

periods <- fread("raw_data/period_lu.csv") %>% 
  mutate_at(vars(first_tr_year, year), as.Date)

matches <- left_join(matches, periods)

pot_con <- left_join(pot_con, periods) %>% 
  group_by(period, black) %>% 
  summarize(to = mean(to)) %>% 
  mutate(black = ifelse(black, "Black Voters", "Non-Black Voters"),
         treated = "All Controls")

matches <- left_join(matches,
                     hills_pre_match %>%
                       select(-GEOID, -fd, -max_amount),
                     by = c("voter" = "voter_id"))

matches <- left_join(matches,
                     hills_pre_match %>%
                       select(voter_id, max_amount, fd, black_t = black),
                     by = c("group" = "voter_id")) %>% 
  mutate(post = period >= 0.5,
         treated = voter == group)

ll <- matches %>%
  filter(period <= 1.5) %>% 
  filter(first_tr_year - fd <= 90) %>%
  group_by(treated, period, black_tt = max_amount <= 10) %>%
  summarize(to = weighted.mean(to, weight)) %>% 
  mutate(treated = ifelse(treated, "Treated", "Control"),
         black = ifelse(black_tt, "Black Voters", "Non-Black Voters"))


ll$treated <- factor(ll$treated, levels = c("Treated", "Control"))

p1 <- ggplot(data = ll) + 
  facet_grid(~ black) +
  geom_rect(aes(xmin = 0.5-.125, xmax = 0.5, ymin = 0, ymax = Inf),
            alpha = 0.03, color = "black", fill = "yellow") +
  geom_line(data =ll, aes(x = period, y = to, linetype = treated)) +
  geom_point(data = ll, aes(x = period, y = to, shape = treated)) +
  scale_x_continuous(minor_breaks = seq(-3.5, 3.5, 1),
                     breaks = seq(-3.5, 3.5, 1),
                     labels = c("-4",
                                "-3",
                                "-2",
                                "-1",
                                "0",
                                "1",
                                "2",
                                "3")) +
  theme_bc(base_family = "LM Roman 10") +
  scale_y_continuous(labels = percent) +
  labs(x = "t", y = "Turnout",
       linetype = "Treatment Group",
       shape = "Treatment Group",
       caption = "Treatment occurs inside of yellow band.") +
  coord_cartesian(ylim = c(0.05, 0.75))

saveRDS(p1, "temp/within90days.rds")
##########################
ll <- matches %>%
  filter(period <= 1.5) %>% 
  group_by(treated, period, black_t) %>%
  summarize(to = weighted.mean(to, weight)) %>% 
  mutate(treated = ifelse(treated, "Treated", "Control"),
         black = ifelse(black_t, "Black Voters", "Non-Black Voters"))

ll$treated <- factor(ll$treated, levels = c("Treated", "Control"))

p2 <- ggplot(data = ll) + 
  facet_grid(~ black) +
  geom_rect(aes(xmin = -.49, xmax = 0.5, ymin = 0, ymax = Inf),
            alpha = 0.03, color = "black", fill = "yellow") +
  geom_line(data =ll, aes(x = period, y = to, linetype = treated)) +
  geom_point(data = ll, aes(x = period, y = to, shape = treated)) +
  scale_x_continuous(minor_breaks = seq(-3.5, 3.5, 1),
                     breaks = seq(-3.5, 3.5, 1),
                     labels = c("-4",
                                "-3",
                                "-2",
                                "-1",
                                "0",
                                "1",
                                "2",
                                "3")) +
  theme_bc(base_family = "LM Roman 10") +
  scale_y_continuous(labels = percent) +
  labs(x = "t", y = "Turnout",
       linetype = "Treatment Group",
       shape = "Treatment Group",
       caption = "Treatment occurs inside of yellow band.") +
  coord_cartesian(ylim = c(0.05, 0.75))
p2
saveRDS(p2, "temp/stopped_any_time.rds")
############### overall
matches$first <- matches$period == 0.5
dat1 <- filter(matches, period <= 1.5)
dat2 <- filter(matches, first_tr_year - fd <= 90, period <= 1.5)

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
                               "Treated $\\times$ Post Treatment",
                               "Treated $\\times$ Black",
                               "Post Treatment $\\times$ Black",
                               "Treated $\\times$ Post Treatment $\\times$ Black"),
          table.layout = "-cm#-t-a-s-n",
          notes = "TO REPLACE",
          title = "\\label{tab:dind-table} Turnout Effects of Tickets",
          out = "temp/two_matches_reg.tex")

j <- fread("./temp/two_matches_reg.tex", header = F, sep = "+")

note.latex <- "\\multicolumn{5}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$. \\\\Robust standard errors (clustered at level of match) in parentheses.}}}"

j <- j %>% 
  mutate(n = row_number(),
         V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
         V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>% 
  filter(!grepl("Note:", V1))

insert1 <- "\\resizebox{1\\textwidth}{!}{%"
insert2 <- "}"

j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(5.1, nrow(j) + 1 - 0.01))) %>%
  arrange(n) %>%
  select(-n)

j <- mutate(j, n = row_number())

j$V1 <- gsub("Stopped within 90 Days of Election", "Stopped within 90", j$V1)

ins <- "&&&\\multicolumn{2}{c}{Days of Election} \\\\"

j <- bind_rows(j,
               data.table(V1 = ins,
                          n = 9.1)) %>% 
  arrange(n) %>% 
  select(-n)

write.table(j, "./temp/dind_reg.tex", quote = F, col.names = F,
            row.names = F)

# ############### first election
# 
# dat1 <- filter(matches, period <= 0.5)
# dat2 <- filter(matches, first_tr_year - fd <= 90, period <= 0.5)
# 
# models1 <- lapply(c(m1, m2), function(f){
#   m <- lm(f, data = dat1,
#           weight = dat1$weight)
# })
# 
# models2 <- lapply(c(m1, m2), function(f){
#   m <- lm(f, data = dat2,
#           weight = dat2$weight)
# })
# 
# 
# ses_cl <- list(
#   summary(lm.cluster(formula = m1, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
#   summary(lm.cluster(formula = m2, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
#   summary(lm.cluster(formula = m1, data = dat2, weights = dat2$weight, cluster = dat2$group))[ , 2],
#   summary(lm.cluster(formula = m2, data = dat2, weights = dat2$weight, cluster = dat2$group))[ , 2]
# )
# 
# 
# stargazer(models1, models2,
#           type = "text",
#           column.labels = c("Stopped Any Time", "Stopped within 90 Days of Election"),
#           column.separate = c(2, 2),
#           omit.stat = c("f", "ser"),
#           se = ses_cl,
#           omit = c("as.fac"),
#           covariate.labels = c("Treated",
#                                "Post Treatment",
#                                "Black",
#                                "Treated X Post Treatment",
#                                "Treated X Black",
#                                "Post Treatment X Black",
#                                "Treated X Post Treatment X Black"),
#           table.layout = "-cm#-t-a-s-n")
# #############################################################################
# #############################################################################
# #############################################################################
# 
# ##########################
# 
# demos <- matches %>%
#   filter(first_tr_year - fd <= 90) %>%
#   group_by(treated) %>%
#   summarize_at(vars(white, black, latino, asian, median_income, age, male,
#                     unem, some_college, dem, rep, reg_date), mean)

###############

demos_all_control <- filter(hills_pre_match, fd > "2018-11-06") %>% 
  select(-voter_id, -GEOID, -fd, -max_amount, -v14, -v16) %>% 
  summarize_all(mean) %>% 
  mutate(group = "All Untreated")

demos_reg <- inner_join(matches_wide, hills_pre_match, by = c("voter" = "voter_id")) %>% 
  mutate(group = ifelse(group == voter, "Treated", "Actual Controls")) %>% 
  select(-voter, -GEOID, -fd, -max_amount, -v14, -v16, -first_tr_year, -weight) %>% 
  group_by(group) %>% 
  summarize_all(mean)


bt <- bind_rows(demos_all_control, demos_reg) %>% 
  mutate_at(vars(!starts_with("group")), as.numeric) %>% 
  pivot_longer(cols = !starts_with("group")) %>% 
  pivot_wider(id_cols = "name", names_from = "group") %>% 
  mutate(mean_imp = 1 - abs((Treated - `Actual Controls`) / (Treated - `All Untreated`))) %>% 
  mutate_at(vars(c(Treated, `Actual Controls`, `All Untreated`)), ~ as.character(round(., 5))) %>%
  mutate_at(vars(c(Treated, `Actual Controls`, `All Untreated`)),
            ~ ifelse(name %in%
                       c("white", "black", "latino", "asian",
                         "male", "dem", "rep", "some_college",
                         "unem", "v10", "v12"),
                     percent(as.numeric(.), .1), .)) %>% 
  mutate_at(vars(c(Treated, `Actual Controls`, `All Untreated`)),
            ~ ifelse(name %in% c("age", "pre_12"), format(round(as.numeric(.), 2), nsmall = 2), .)) %>% 
  mutate_at(vars(c(Treated, `Actual Controls`, `All Untreated`)),
            ~ ifelse(name == "reg_date", 
                     as.character(as.numeric(.) + as.Date("2000-01-01")), .)) %>% 
  mutate_at(vars(c(Treated, `Actual Controls`, `All Untreated`)),
            ~ ifelse(name == "median_income", 
                     dollar(as.numeric(.), accuracy = 1), .)) %>% 
  mutate(mean_imp = percent(mean_imp, .01)) %>% 
  filter(name != "pre")

or <- fread("raw_data/var_orders.csv")

bt <- left_join(bt, or, by = c("name" = "Name")) %>% 
  select(Variable, `Treated`, `All Untreated`, `Actual Controls`, `Mean Improvement` = mean_imp)

saveRDS(bt, "temp/balance_table.rds")
