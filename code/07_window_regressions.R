hist <- readRDS("temp/hist_rolls.rds") %>%
  select(voter_id, starts_with("v1"), v08) %>%
  pivot_longer(!starts_with("vo"), names_to = "year", values_to = "to")

elec_dates <- fread("raw_data/election_dates.csv")

hist <- left_join(hist, elec_dates) %>% 
  select(-year) %>% 
  rename(year = date) %>% 
  mutate(year = as.Date(year, "%m/%d/%Y"))

hist <- hist %>% 
  filter(year(year) %% 2 == 0)

month_matches <- readRDS("temp/month_matches.rds")
###############################

full_inters <- rbindlist(lapply(c(1:23), function(i){
  matches <- filter(month_matches, month == i) %>% 
    select(-month)
  
  nt <- nrow(filter(matches, voter == group))
  
  hills_pre_match <- readRDS("temp/real_pre_match_hills_anon.rds") %>% 
    ungroup()
  
  matches <- left_join(matches, select(hills_pre_match, voter_id, first_tr_year),
                       by = c("group" = "voter_id", "first_tr_year")) %>% 
    mutate(fd = first_tr_year,
           first_tr_year = ifelse(first_tr_year == 1, "2014-11-04",
                                  ifelse(first_tr_year == 2, "2016-11-08",
                                         ifelse(first_tr_year == 3, "2018-11-06", "XX"))),
           first_tr_year = as.Date(first_tr_year))
  
  
  matches <- left_join(matches, hist, by = c("voter" = "voter_id"))
  
  periods <- fread("raw_data/period_lu.csv") %>% 
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
    dem + rep + age + reg_date + pre_stops + v1 + v2 + v3 +
    median_income + some_college + unem + civil + paid + tampa_pd
  
  m2 <- to ~ treated * post*black + as.factor(year) +
    white + black + latino + asian + male +
    dem + rep + age + reg_date + pre_stops + v1 + v2 + v3 +
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
  return(bind_rows(j2, ax) %>% 
           mutate(ntreat = nt))
}))

saveRDS(full_inters, "temp/rob_intervals.rds")

full_inters <- readRDS("temp/rob_intervals.rds")

colnames(full_inters) <- c("lb", "ub", "var", "month", "model", "ntreat")

#######################################
inter_data <- filter(full_inters, var %in% c("black_add"),
                      model == "inter") %>% 
  mutate(estimate = (lb + ub) / 2)
  

p <- ggplot(inter_data, aes(x = month, y = estimate, shape = var, linetype = var)) + geom_point() +
  geom_line() + theme_bc(base_family = "Latin Modern Roman",  legend.position = "NONE") +
  geom_errorbar(aes(ymin = lb, ymax = ub)) +
  scale_x_continuous(breaks = c(1:23)) +
  scale_y_continuous(labels = percent) +
  labs(x = "Months Before / After Election",
       y = "Estimated Treatment Effect (Treated × Post-Treatment × Black)") +
  geom_hline(yintercept = 0, linetype = "dashed")
saveRDS(p, "temp/black_overall_effect.rds")
p
inter_data <- filter(full_inters, var %in% c("treatedTRUE:postTRUE:blackTRUE"),
                     model == "inter") %>% 
  mutate(estimate = (lb + ub) / 2)

p <- ggplot(inter_data, aes(x = month, y = estimate, shape = var, linetype = var)) + geom_point() +
  geom_line() + theme_bc(base_family = "Latin Modern Roman", legend.position = "NONE") +
  geom_errorbar(aes(ymin = lb, ymax = ub)) +
  scale_x_continuous(breaks = c(1:23)) +
  scale_y_continuous(labels = percent) +
  labs(x = "Months Before / After Election",
       y = "Estimated Treatment Effect (Treated × Post-Treatment × Black)") +
  geom_hline(yintercept = 0, linetype = "dashed")
p
saveRDS(p, "temp/black_relative_effect.rds")

inter_data <- filter(full_inters, var %in% c("treatedTRUE:postTRUE"),
                     model == "inter") %>% 
  mutate(estimate = (lb + ub) / 2)

p <- ggplot(inter_data, aes(x = month, y = estimate, shape = var, linetype = var)) + geom_point() +
  geom_line() + theme_bc(base_family = "Latin Modern Roman", legend.position = "NONE") +
  geom_errorbar(aes(ymin = lb, ymax = ub)) +
  scale_x_continuous(breaks = c(1:23)) +
  scale_y_continuous(labels = percent) +
  labs(x = "Months Before / After Election",
       y = "Estimated Treatment Effect (Treated × Post-Treatment)") +
  geom_hline(yintercept = 0, linetype = "dashed")
p
saveRDS(p, "temp/non_black_effect.rds")

########################################
inter_data <- filter(full_inters, var %in% c("treatedTRUE:postTRUE", "black_add"),
                     model == "inter") %>% 
  mutate(estimate = (lb + ub) / 2,
         var = ifelse(var == "black_add", "Black Voters", "Non-Black Voters"))

p <- ggplot(inter_data, aes(x = month, y = estimate)) + geom_point() +
  geom_line() + theme_bc(base_family = "Latin Modern Roman", legend.position = "NONE") +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0) +
  scale_x_continuous(breaks = seq(1, 25, 5)) +
  scale_y_continuous(labels = percent) +
  labs(x = "Months Before / After Election",
       y = "Estimated Treatment Effect") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(~var)
p
saveRDS(p, "temp/window_plot_good.rds")

########################################

straight_data <- filter(full_inters, var %in% c("treatedTRUE:postTRUE"),
                      model == "straight") %>% 
  mutate(estimate = (lb + ub) / 2)


p2 <- ggplot() + geom_point(data = straight_data, aes(x = month, y = estimate)) +
  geom_line(data = straight_data, aes(x = month, y = estimate)) + theme_bc(base_family = "Latin Modern Roman") +
  geom_errorbar(data = straight_data, aes(ymin = lb, ymax = ub, y = estimate, x = month)) +
  scale_x_continuous(breaks = c(1:23)) +
  scale_y_continuous(labels = percent) +
  labs(x = "Months Before / After Election",
       y = "Estimated Treatment Effect (Treated × Post-Treatment)") +
  geom_hline(yintercept = 0, linetype = "dashed")
p2

saveRDS(p2, "temp/overall_effect.rds")
