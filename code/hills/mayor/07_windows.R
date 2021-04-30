library(Matching)
library(tidyverse)
library(lubridate)
library(data.table)

lapply(c(1:23), function(i){
  pre <- readRDS("temp/real_pre_match_hills_mayor.rds") %>% 
    filter(((last_date >= (ymd(as.Date("2015-03-03")) %m+% months(-i))) &
              (last_date <= ymd(as.Date("2015-03-03")) %m+% months(i)) & (first_tr_year == 1)) |
             ((last_date >= (ymd(as.Date("2019-03-05")) %m+% months(-i))) &
                (last_date <= ymd(as.Date("2019-03-05")) %m+% months(i)) & (first_tr_year == 2)))
  
  
  ids <- pre %>%
    mutate(id = row_number()) %>%
    select(id, voter_id, first_tr_year)
  
  X <- pre %>%
    select(-voter_id, -treated, -GEOID, -amount_paid, -last_date,
           -v15, -v19, -v07, -reg_date) %>% 
    mutate_at(vars(white, black, latino, asian, male, dem, rep, v1, v2, paid), ~ ifelse(. == T, 1, 0)) %>% 
    select(first_tr_year, paid, civil, tampa_pd, v1, v2, everything())
  
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
                  exact = c(rep(T, 6), rep(F, 14)))
  
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
  
  saveRDS(matches, paste0("temp/matches_rob_", i, "_mayor.rds"))
})


#####################################
hist <- readRDS("temp/full_raw_coded_hills_w_bgs.rds") %>%
  select(voter_id, starts_with("v1"), v08, v07) %>%
  pivot_longer(!starts_with("vo"), names_to = "year", values_to = "to")

elec_dates <- fread("raw_data/election_dates.csv")

hist <- left_join(hist, elec_dates) %>% 
  select(-year) %>% 
  rename(year = date) %>% 
  mutate(year = as.Date(year, "%m/%d/%Y"))

hist <- hist %>% 
  filter(year(year) %% 2 == 1)
###############################

full_inters <- rbindlist(lapply(c(1:23), function(i){
  matches <- readRDS(paste0("temp/matches_rob_", i, "_mayor.rds"))
  
  hills_pre_match <- readRDS("temp/real_pre_match_hills_mayor.rds") %>% 
    ungroup()
  
  matches <- left_join(matches, select(hills_pre_match, voter_id, first_tr_year),
                       by = c("group" = "voter_id", "first_tr_year")) %>% 
    mutate(fd = first_tr_year,
           first_tr_year = ifelse(first_tr_year == 1, "2015-03-03",
                                  ifelse(first_tr_year == 2, "2019-03-05", "XX")),
           first_tr_year = as.Date(first_tr_year))
  
  matches <- left_join(matches, hist, by = c("voter" = "voter_id"))
  
  periods <- fread("raw_data/period_lu_mayor.csv") %>% 
    mutate_at(vars(first_tr_year, year), as.Date, "%m/%d/%Y")
  
  matches <- left_join(matches, periods) %>% 
    filter(period %in% c(-2.5, -1.5, -.5, .5, 1.5))
  
  matches <- left_join(matches,
                       hills_pre_match %>%
                         select(-GEOID, -amount_paid, -last_date),
                       by = c("voter" = "voter_id", "fd" = "first_tr_year"))
  
  matches <- left_join(matches,
                       hills_pre_match %>%
                         select(voter_id, amount_paid, last_date, first_tr_year, black_t = black,
                                dem_t = dem),
                       by = c("group" = "voter_id", "fd" = "first_tr_year")) %>% 
    mutate(post = period >= 0.5,
           treated = voter == group)
  
  
  dat1 <- filter(matches, period <= 0.5)
  
  m1 <- to ~ treated * post + as.factor(year) +
    white + black + latino + asian + male +
    dem + rep + age + reg_date + pre_stops + v1 + v2 +
    median_income + some_college + unem + civil + paid + tampa_pd
  
  m2 <- to ~ treated * post*black + as.factor(year) +
    white + black + latino + asian + male +
    dem + rep + age + reg_date + pre_stops + v1 + v2 +
    median_income + some_college + unem + civil + paid + tampa_pd
  
  m <- lm.cluster(formula = m2, data = dat1, weights = dat1$weight, cluster = dat1$group)
  
  j <- data.table(confint(m))
  j2 <- cbind(j, rownames(confint(m))) %>% 
    mutate(month = i)
  
  h <- glht(m, linfct = c("treatedTRUE:postTRUE + treatedTRUE:postTRUE:blackTRUE = 2"))
  
  temp <- data.table(V2 = "black_add",
                     `2.5 %` = confint(h)[["confint"]][[2]],
                     `97.5 %` = confint(h)[["confint"]][[3]],
                     month = i)
  
  j2 <- bind_rows(j2, temp) %>% 
    mutate(model = "inter")
  
  ###################
  m <- lm.cluster(formula = m1, data = dat1, weights = dat1$weight, cluster = dat1$group)
  
  j <- data.table(confint(m))
  
  ax <- cbind(j, rownames(confint(m))) %>% 
    mutate(month = i,
           model = "straight")
  
  ###################
  return(bind_rows(j2, ax))
}))

saveRDS(full_inters, "temp/rob_intervals_mayor.rds")

full_inters <- readRDS("temp/rob_intervals_mayor.rds")

colnames(full_inters) <- c("lb", "ub", "var", "month", "model")

#######################################
inter_data <- filter(full_inters, var %in% c("black_add"),
                     model == "inter") %>% 
  mutate(estimate = (lb + ub) / 2)


ggplot(inter_data, aes(x = month, y = estimate, shape = var, linetype = var)) + geom_point() +
  geom_line() + theme_bc(base_family = "LM Roman 10") +
  geom_errorbar(aes(ymin = lb, ymax = ub)) +
  scale_x_continuous(breaks = c(1:23)) +
  scale_y_continuous(labels = percent) +
  labs(x = "Months Before / After Election",
       y = "Estimated Treatment Effect (Treated × Post-Treatment)") +
  geom_hline(yintercept = 0, linetype = "dashed")
########################################

straight_data <- filter(full_inters, var %in% c("treatedTRUE:postTRUE"),
                       model == "straight") %>% 
  mutate(estimate = (lb + ub) / 2)


p2 <- ggplot(straight_data, aes(x = month, y = estimate)) + geom_point() +
  geom_line() + theme_bc(base_family = "LM Roman 10") +
  geom_errorbar(aes(ymin = lb, ymax = ub)) +
  scale_x_continuous(breaks = c(1:23)) +
  scale_y_continuous(labels = percent) +
  labs(x = "Months Before / After Election",
       y = "Estimated Treatment Effect (Treated × Post-Treatment)") +
  geom_hline(yintercept = 0, linetype = "dashed")
p2

saveRDS(p2, "temp/windows_rob_mayor.rds")
