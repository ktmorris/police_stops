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

###############################


month_matches <- readRDS("temp/month_matches.rds")
hills_pre_match <- readRDS("temp/real_pre_match_hills_anon.rds") %>% 
  ungroup()

full_inters <- rbindlist(lapply(c(1:23), function(i){
  matches <- filter(month_matches, month == i) %>% 
    select(-month)
  
  nt <- nrow(filter(matches, voter == group))

  
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
  
  m1 <- to ~ treated * post*black + as.factor(year) +
    white + black + latino + asian + male +
    dem + rep + age + reg_date + pre_stops + v1 + v2 + v3 +
    median_income + some_college + unem + civil + paid + tampa_pd
  
  m <- lm.cluster(formula = m1, data = dat1, weights = dat1$weight, cluster = dat1$group)
  
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
  
  
  m <- feols(fml = m1, data = dat1, weights = dat1$weight, cluster = dat1$group)
  
  saveRDS(m, paste0("temp/month_", i, "_reg.rds"))
  return(j2 %>% 
           mutate(ntreat = nt))
}))

saveRDS(full_inters, "temp/rob_intervals.rds")

full_inters <- readRDS("temp/rob_intervals.rds")

colnames(full_inters) <- c("lb", "ub", "var", "month", "model", "ntreat")

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

############################

mods <- list(
  readRDS("temp/month_1_reg.rds"),
  readRDS("temp/month_2_reg.rds"),
  readRDS("temp/month_3_reg.rds"),
  readRDS("temp/month_4_reg.rds"),
  readRDS("temp/month_5_reg.rds"),
  readRDS("temp/month_6_reg.rds")
)

names(mods) <- c("1 month",
                 "2",
                 "3",
                 "4",
                 "5",
                 "6")

rows <- tribble(~term, ~m1,  ~m2, ~m3, ~m4, ~m5, ~m6,
                "Year Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
attr(rows, 'position') <- c(53:53)

modelsummary(mods,
             statistic = "std.error",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = c("treatedTRUE:postTRUE" = "Treated $\\times$ Post Treatment",
                          "treatedTRUE:postTRUE:blackTRUE" = "Treated $\\times$ Post Treatment $\\times$ Black",
                          "treatedTRUE" = "Treated",
                          "postTRUE" = "Post Treatment",
                          "blackTRUE" = "Black",
                          "whiteTRUE" = "White",
                          "latinoTRUE" = "Latino",
                          "asianTRUE" = "Asian",
                          "maleTRUE" = "Male",
                          "demTRUE" = "Democrat",
                          "repTRUE" = "Republican",
                          "age" = "Age",
                          "reg_date" = "Registration Date",
                          "pre_stops" = "Traffic Stops before Period",
                          "v1TRUE" = "Turnout (t = -3)",
                          "v2TRUE" = "Turnout (t = -2)",
                          "v3TRUE" = "Turnout (t = -1)",
                          "median_income" = "Nhood Median Income",
                          "some_college" = "Nhood w/ Some College",
                          "unem" = "Nhood Unemployment Rate",
                          "civil" = "Civil Infraction",
                          "paidTRUE" = "Paid Money on Stop",
                          "tampa_pd" = "Stopped by Tampa Police Department",
                          "treatedTRUE:blackTRUE" = "Treated $\\times$ Black",
                          "postTRUE:blackTRUE" = "Post Treatment $\\times$ Black",
                          "(Intercept)" = "Intercept"),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             title = "Individual-Level Turnout",
             latex_options = "scale_down",
             add_rows = rows,
             output = paste0("temp/months_1_6.tex"),
             escape = FALSE)

j <- fread("temp/months_1_6.tex", header = F, sep = "+") %>%
  mutate(n = row_number())

insert1 <- "\\resizebox*{!}{0.95\\textheight}{%"
insert2 <- "}"

j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(4.1, nrow(j) - 0.01))) %>%
  arrange(n) %>%
  select(-n)


write.table(j, "temp/months_1_6.tex", quote = F, col.names = F,
            row.names = F)
#################################

mods <- list(
  readRDS("temp/month_7_reg.rds"),
  readRDS("temp/month_8_reg.rds"),
  readRDS("temp/month_9_reg.rds"),
  readRDS("temp/month_10_reg.rds"),
  readRDS("temp/month_11_reg.rds"),
  readRDS("temp/month_12_reg.rds")
)

names(mods) <- c("7 months",
                 "8",
                 "9",
                 "10",
                 "11",
                 "12")

modelsummary(mods,
             statistic = "std.error",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = c("treatedTRUE:postTRUE" = "Treated $\\times$ Post Treatment",
                          "treatedTRUE:postTRUE:blackTRUE" = "Treated $\\times$ Post Treatment $\\times$ Black",
                          "treatedTRUE" = "Treated",
                          "postTRUE" = "Post Treatment",
                          "blackTRUE" = "Black",
                          "whiteTRUE" = "White",
                          "latinoTRUE" = "Latino",
                          "asianTRUE" = "Asian",
                          "maleTRUE" = "Male",
                          "demTRUE" = "Democrat",
                          "repTRUE" = "Republican",
                          "age" = "Age",
                          "reg_date" = "Registration Date",
                          "pre_stops" = "Traffic Stops before Period",
                          "v1TRUE" = "Turnout (t = -3)",
                          "v2TRUE" = "Turnout (t = -2)",
                          "v3TRUE" = "Turnout (t = -1)",
                          "median_income" = "Nhood Median Income",
                          "some_college" = "Nhood w/ Some College",
                          "unem" = "Nhood Unemployment Rate",
                          "civil" = "Civil Infraction",
                          "paidTRUE" = "Paid Money on Stop",
                          "tampa_pd" = "Stopped by Tampa Police Department",
                          "treatedTRUE:blackTRUE" = "Treated $\\times$ Black",
                          "postTRUE:blackTRUE" = "Post Treatment $\\times$ Black",
                          "(Intercept)" = "Intercept"),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             title = "Individual-Level Turnout",
             latex_options = "scale_down",
             add_rows = rows,
             output = paste0("temp/months_7_12.tex"),
             escape = FALSE)

j <- fread("temp/months_7_12.tex", header = F, sep = "+") %>%
  mutate(n = row_number())

insert1 <- "\\resizebox*{!}{0.95\\textheight}{%"
insert2 <- "}"

j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(4.1, nrow(j) - 0.01))) %>%
  arrange(n) %>%
  select(-n)


write.table(j, "temp/months_7_12.tex", quote = F, col.names = F,
            row.names = F)

#################################

mods <- list(
  readRDS("temp/month_13_reg.rds"),
  readRDS("temp/month_14_reg.rds"),
  readRDS("temp/month_15_reg.rds"),
  readRDS("temp/month_16_reg.rds"),
  readRDS("temp/month_17_reg.rds"),
  readRDS("temp/month_18_reg.rds")
)

names(mods) <- c("13 months",
                 "14",
                 "15",
                 "16",
                 "17",
                 "18")

modelsummary(mods,
             statistic = "std.error",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = c("treatedTRUE:postTRUE" = "Treated $\\times$ Post Treatment",
                          "treatedTRUE:postTRUE:blackTRUE" = "Treated $\\times$ Post Treatment $\\times$ Black",
                          "treatedTRUE" = "Treated",
                          "postTRUE" = "Post Treatment",
                          "blackTRUE" = "Black",
                          "whiteTRUE" = "White",
                          "latinoTRUE" = "Latino",
                          "asianTRUE" = "Asian",
                          "maleTRUE" = "Male",
                          "demTRUE" = "Democrat",
                          "repTRUE" = "Republican",
                          "age" = "Age",
                          "reg_date" = "Registration Date",
                          "pre_stops" = "Traffic Stops before Period",
                          "v1TRUE" = "Turnout (t = -3)",
                          "v2TRUE" = "Turnout (t = -2)",
                          "v3TRUE" = "Turnout (t = -1)",
                          "median_income" = "Nhood Median Income",
                          "some_college" = "Nhood w/ Some College",
                          "unem" = "Nhood Unemployment Rate",
                          "civil" = "Civil Infraction",
                          "paidTRUE" = "Paid Money on Stop",
                          "tampa_pd" = "Stopped by Tampa Police Department",
                          "treatedTRUE:blackTRUE" = "Treated $\\times$ Black",
                          "postTRUE:blackTRUE" = "Post Treatment $\\times$ Black",
                          "(Intercept)" = "Intercept"),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             title = "Individual-Level Turnout",
             latex_options = "scale_down",
             add_rows = rows,
             output = paste0("temp/months_13_18.tex"),
             escape = FALSE)

j <- fread("temp/months_13_18.tex", header = F, sep = "+") %>%
  mutate(n = row_number())

insert1 <- "\\resizebox*{!}{0.95\\textheight}{%"
insert2 <- "}"

j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(4.1, nrow(j) - 0.01))) %>%
  arrange(n) %>%
  select(-n)


write.table(j, "temp/months_13_18.tex", quote = F, col.names = F,
            row.names = F)

#################################

mods <- list(
  readRDS("temp/month_19_reg.rds"),
  readRDS("temp/month_20_reg.rds"),
  readRDS("temp/month_21_reg.rds"),
  readRDS("temp/month_22_reg.rds"),
  readRDS("temp/month_23_reg.rds")
)

names(mods) <- c("19 months",
                 "20",
                 "21",
                 "22",
                 "23")

rows <- rows[,c(1:6)]

modelsummary(mods,
             statistic = "std.error",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = c("treatedTRUE:postTRUE" = "Treated $\\times$ Post Treatment",
                          "treatedTRUE:postTRUE:blackTRUE" = "Treated $\\times$ Post Treatment $\\times$ Black",
                          "treatedTRUE" = "Treated",
                          "postTRUE" = "Post Treatment",
                          "blackTRUE" = "Black",
                          "whiteTRUE" = "White",
                          "latinoTRUE" = "Latino",
                          "asianTRUE" = "Asian",
                          "maleTRUE" = "Male",
                          "demTRUE" = "Democrat",
                          "repTRUE" = "Republican",
                          "age" = "Age",
                          "reg_date" = "Registration Date",
                          "pre_stops" = "Traffic Stops before Period",
                          "v1TRUE" = "Turnout (t = -3)",
                          "v2TRUE" = "Turnout (t = -2)",
                          "v3TRUE" = "Turnout (t = -1)",
                          "median_income" = "Nhood Median Income",
                          "some_college" = "Nhood w/ Some College",
                          "unem" = "Nhood Unemployment Rate",
                          "civil" = "Civil Infraction",
                          "paidTRUE" = "Paid Money on Stop",
                          "tampa_pd" = "Stopped by Tampa Police Department",
                          "treatedTRUE:blackTRUE" = "Treated $\\times$ Black",
                          "postTRUE:blackTRUE" = "Post Treatment $\\times$ Black",
                          "(Intercept)" = "Intercept"),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             title = "Individual-Level Turnout",
             latex_options = "scale_down",
             add_rows = rows,
             output = paste0("temp/months_19_23.tex"),
             escape = FALSE)

j <- fread("temp/months_19_23.tex", header = F, sep = "+") %>%
  mutate(n = row_number())

insert1 <- "\\resizebox*{!}{0.95\\textheight}{%"
insert2 <- "}"

j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(4.1, nrow(j) - 0.01))) %>%
  arrange(n) %>%
  select(-n)


write.table(j, "temp/months_19_23.tex", quote = F, col.names = F,
            row.names = F)
