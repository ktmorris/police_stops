
#####
hills_pre_match <- readRDS("temp/real_pre_match_hills_anon.rds") %>% 
  ungroup()

matches <- readRDS("temp/matches_hills.rds")

matches <- left_join(matches, select(hills_pre_match, voter_id, first_tr_year),
                     by = c("group" = "voter_id", "first_tr_year")) %>% 
  mutate(fd = first_tr_year,
         first_tr_year = ifelse(first_tr_year == 1, "2014-11-04",
                                ifelse(first_tr_year == 2, "2016-11-08",
                                       ifelse(first_tr_year == 3, "2018-11-06", "XX"))),
         first_tr_year = as.Date(first_tr_year))

hist <- readRDS("temp/hist_rolls.rds") %>%
  select(voter_id, starts_with("v1"), v08) %>%
  pivot_longer(!starts_with("vo"), names_to = "year", values_to = "to")

elec_dates <- fread("raw_data/election_dates.csv")

hist <- left_join(hist, elec_dates) %>% 
  select(-year) %>% 
  rename(year = date) %>% 
  mutate(year = as.Date(year, "%m/%d/%Y"))


matches <- left_join(matches, hist %>% 
                       filter(year(year) %% 2 == 0), by = c("voter" = "voter_id"))

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

cleanup("matches")

##########################################################
ll <- bind_rows(mutate(matches, first_tr_year = as.character(first_tr_year)),
                mutate(matches, first_tr_year = "Overall")) %>%
  filter(period <= 0.5) %>% 
  group_by(treated, period, black_t, first_tr_year) %>%
  summarize(to = weighted.mean(to, weight)) %>% 
  mutate(treated = ifelse(treated, "Treated", "Control"),
         black = ifelse(black_t, "Black Voters", "Non-Black Voters"))

ll$treated <- factor(ll$treated, levels = c("Treated", "Control"))

ll <- ll %>% 
  mutate(first_tr_year = ifelse(first_tr_year == "Overall", first_tr_year,
                                paste("t = 0\nin", substring(first_tr_year, 1, 4))))

ll$first_tr_year <- factor(ll$first_tr_year, levels = unique(ll$first_tr_year))

p2 <- ggplot(data = ll) + 
  facet_grid(first_tr_year ~ black) +
  geom_rect(aes(xmin = -.49, xmax = 0.5, ymin = -Inf, ymax = Inf),
            alpha = 0.3, color = "black", fill = "gray") +
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
  theme_bc(base_family = "Latin Modern Roman") +
  scale_y_continuous(labels = percent) +
  labs(x = "t", y = "Turnout",
       linetype = "Treatment Group",
       shape = "Treatment Group",
       caption = "Treatment occurs inside of shaded band.
Full regression tables in section 3 of SI.", ) +
  coord_cartesian(ylim = c(0.05, 0.75))
p2
saveRDS(p2, "temp/stopped_any_time.rds")

#####################
matches$first_tr_year <- as.character(matches$first_tr_year)
saveRDS(matches, "temp/full_reg_data.rds")

matches <- readRDS("temp/full_reg_data.rds")

m1 <- to ~ treated * post + as.factor(year)

m2 <- to ~ treated * post + as.factor(year) +
  white + black + latino + asian + male +
  dem + rep + age + reg_date + pre_stops + v1 + v2 + v3 +
  median_income + some_college + unem + civil + paid + tampa_pd

m3 <- to ~ treated * post * black + as.factor(year)

m4 <- to ~ treated * post * black + as.factor(year) +
  white + black + latino + asian + male +
  dem + rep + age + reg_date + pre_stops + v1 + v2 + v3 +
  median_income + some_college + unem + civil + paid + tampa_pd



for(gg in c("overall", "2014-11-04", "2016-11-08", "2018-11-06")){
  
  if(gg == "overall"){
    dat1 <- filter(matches, period <= 0.5)
    tit = "\\label{tab:dind-table} Overall Treatment Effect\\\\\nDependent Variable: Individual-Level Turnout"
    ooo = c(1, 2, 4, 23, 24, 22, 25)
  }else{
    dat1 <- dplyr::filter(matches, period <= 0.5, first_tr_year == gg)
    tit = paste0("\\label{tab:dind-table-", substring(gg, 1, 4), "}Treatment Effect for Voters Stopped before ", substring(gg, 1, 4), " Election\\\\\nDependent Variable: Individual-Level Turnout")
    ooo = c(1, 2, 7, 23, 24, 22, 25)
  }
 
  models1 <- lapply(c(m1, m2, m3, m4), function(f){
    m <- feols(f, data = dat1,
            weight = ~ weight, cluster = "group")
  })
  
  c1 <- confint(models1[[1]])
  c1$term <- rownames(c1)
  c1 <- c1 %>% 
    mutate(model = 1,
           year = gg,
           t = "Overall") %>% 
    filter(term == "treatedTRUE:postTRUE")
  
  c2 <- confint(models1[[2]])
  c2$term <- rownames(c2)
  c2 <- c2 %>% 
    mutate(model = 2,
           year = gg,
           t = "Overall") %>% 
    filter(term == "treatedTRUE:postTRUE")
  
  c3 <- confint(models1[[3]])
  c3$term <- rownames(c3)
  c3 <- c3 %>% 
    mutate(model = 3,
           year = gg,
           t = "Non-Black") %>% 
    filter(term == "treatedTRUE:postTRUE")
  
  h <- glht(models1[[3]], linfct = c("treatedTRUE:postTRUE + treatedTRUE:postTRUE:blackTRUE = 2"))
  c4 <- data.table(t = "Black",
                   `2.5 %` = confint(h)[["confint"]][[2]],
                   `97.5 %` = confint(h)[["confint"]][[3]],
                   year = gg,
                   model = 3)
  
  c5 <- confint(models1[[4]])
  c5$term <- rownames(c5)
  c5 <- c5 %>% 
    mutate(model = 4,
           year = gg,
           t = "Non-Black") %>% 
    filter(term == "treatedTRUE:postTRUE")
  
  h <- glht(models1[[4]], linfct = c("treatedTRUE:postTRUE + treatedTRUE:postTRUE:blackTRUE = 2"))
  c6 <- data.table(t = "Black",
                   `2.5 %` = confint(h)[["confint"]][[2]],
                   `97.5 %` = confint(h)[["confint"]][[3]],
                   year = gg,
                   model = 4)
  
  c <- bind_rows(c1, c2, c3, c4, c5, c6)
  saveRDS(c, paste0("temp/cints_full_", gg, ".rds"))
  ##################################
  rows <- tribble(~term, ~m1,  ~m2, ~m3, ~m4,
                  "Year Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                  "Matching Covariates Included", "", "$\\checkmark$", "", "$\\checkmark$")
  
  attr(rows, 'position') <- c(17:18)
  
  modelsummary(models1,
               statistic = "std.error",
               stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
               coef_map = c("treatedTRUE:postTRUE" = "Treated $\\times$ Post Treatment",
                            "treatedTRUE:postTRUE:blackTRUE" = "Treated $\\times$ Post Treatment $\\times$ Black",
                            "treatedTRUE" = "Treated",
                            "postTRUE" = "Post Treatment",
                            "blackTRUE" = "Black",
                            "treatedTRUE:blackTRUE" = "Treated $\\times$ Black",
                            "postTRUE:blackTRUE" = "Post Treatment $\\times$ Black",
                            "(Intercept)" = "Intercept"),
               gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
               title = tit,
               latex_options = "scale_down",
               add_rows = rows,
               output = paste0("temp/small_reg_", gg, ".tex"),
               escape = FALSE)
  
  j <- fread(paste0("temp/small_reg_", gg, ".tex"), header = F, sep = "+") %>%
    mutate(n = row_number())
  
  for(i in c(2:nrow(j))){
    j$n[i] <- ifelse(grepl("Intercept", j$V1[i-1]), j$n[i] + 1.5, j$n[i])
  }
  
  j <- j %>% 
    arrange(n) %>% 
    select(-n)
  
  write.table(j, paste0("temp/small_reg_", gg, ".tex"), quote = F, col.names = F,
              row.names = F)
  
  ##########################
  rows <- tribble(~term, ~m1,  ~m2, ~m3, ~m4,
                  "Year Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
  attr(rows, 'position') <- c(53:53)
  
  modelsummary(models1,
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
               title = tit,
               latex_options = "scale_down",
               add_rows = rows,
               output = paste0("temp/big_reg_", gg, ".tex"),
               escape = FALSE)
  
  j <- fread(paste0("temp/big_reg_", gg, ".tex"), header = F, sep = "+") %>%
    mutate(n = row_number())
  
  for(i in c(2:nrow(j))){
    j$n[i] <- ifelse(grepl("Intercept", j$V1[i-1]), j$n[i] + 1.5, j$n[i])
  }
  
  l <- filter(j, grepl("dind-table", V1)) %>% 
    select(n) %>% 
    pull()
  
  add <- data.frame(V1 = "\\captionsetup{justification=centering}",
                    n = (l - 0.01))
  
  j <- bind_rows(j, add)

  insert1 <- "\\resizebox*{!}{0.95\\textheight}{%"
  insert2 <- "}"

  j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(5.1, max(j$n) - 0.01))) %>%
    arrange(n) %>%
    select(-n)


  write.table(j, paste0("temp/big_reg_", gg, ".tex"), quote = F, col.names = F,
              row.names = F)
   
}

########################## coefficient plot
cints <- rbindlist(lapply(c("overall", "2014-11-04", "2016-11-08", "2018-11-06"), function(gg){
  readRDS(paste0("temp/cints_full_", gg, ".rds"))
})) %>% 
  mutate(model = ifelse(t == "Overall", model, model - 2))

colnames(cints) <- c("lb", "ub", "term", "model", "year", "t")

cints <- mutate(cints, estimate = (lb + ub) / 2,
                model = ifelse(model == 1, "No Controls", "With Controls"),
                year = ifelse(year == "overall", "Overall",
                              paste("t = 0\nin", substring(year, 1, 4))))

cints$year <- factor(cints$year, levels = c("t = 0\nin 2014",
                                            "t = 0\nin 2016",
                                            "t = 0\nin 2018",
                                            "Overall"))

p <- ggplot(data = cints, aes(y = t, x = estimate, xmin = lb, 
                              xmax = ub, linetype = model)) +
  ggstance::geom_pointrangeh(aes(y = t, x = estimate, 
                                 xmin = lb, xmax = ub,
                                 shape = model), position = ggstance::position_dodgev(height = 0.5), 
                             fill = "white", show.legend = T) +
  facet_grid(year ~.) +
  theme_bc(base_family = "Latin Modern Roman", legend.position = "bottom") +
  labs(shape = "Model",
       linetype = "Model",
       x = "Estimate",
       y = NULL) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_x_continuous(labels = scales::percent)
p
saveRDS(p, "temp/coef_plot.rds")

#################### event study

cleanup()

matches <- readRDS("temp/full_reg_data.rds") %>% 
  filter(period <= 0.5)

m1 <- to ~ treated * post + as.factor(year)
m2 <- to ~ treated * post + as.factor(year) +
  white + black + latino + asian + male +
  dem + rep + age + reg_date + pre_stops + v1 + v2 + v3 +
  median_income + some_college + unem + civil + paid + tampa_pd

es <- rbindlist(lapply(seq(-1.5, 0.5, 1), function(y){
  dat1 <- filter(matches, period <= y)
  dat1$post <- dat1$period == y
  
  mod1 <- feols(m1, dat1, weights = ~ weight, cluster = ~ group)
  mod2 <- feols(m2, dat1, weights = ~ weight, cluster = ~ group)
  
  saveRDS(mod1, paste0("temp/es_reg_mod1_", y, ".rds"))
  saveRDS(mod2, paste0("temp/es_reg_mod2_", y, ".rds"))
  
  c <- confint(mod1) %>% 
    mutate(model = 1,
           year = y)
  c <- c[rownames(c) == "treatedTRUE:postTRUE",]
  
  c2 <- confint(mod2) %>% 
    mutate(model = 2,
           year = y)
  c2 <- c2[rownames(c2) == "treatedTRUE:postTRUE",]
  
  return(bind_rows(
    c,
    c2
  ))
})) %>% 
  rename(lower = `2.5 %`,
         upper = `97.5 %`) %>% 
  mutate(estimate = (upper + lower) / 2,
         model = ifelse(model == 1, "Without Covariates", "With Covariates"))

p <- ggplot(data = filter(es), aes(y = year, x = estimate, xmin = lower, 
                                   xmax = upper, shape = model,
                                   linetype = model)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggstance::geom_pointrangeh(aes(y = year, x = estimate, 
                                 xmin = lower, xmax = upper, shape = model,
                                 linetype = model),
                             position = ggstance::position_dodgev(height = .5), 
                             fill = "white", fatten = 3, size = 0.8, show.legend = T) +
  coord_flip() + theme_bc(base_family = "Latin Modern Roman") +
  theme(legend.position = "bottom") +
  labs(y = "t = 0", x = "Estimate", shape = "Model",
       linetype = "Model") + scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = c(-2, -1, 0), breaks = c(-1.5, -.5, .5))
p
saveRDS(p, "temp/event_study.rds")

mods <- list(
  readRDS(paste0("temp/es_reg_mod1_-1.5.rds")),
  readRDS(paste0("temp/es_reg_mod2_-1.5.rds")),
  readRDS(paste0("temp/es_reg_mod1_-0.5.rds")),
  readRDS(paste0("temp/es_reg_mod2_-0.5.rds")),
  readRDS(paste0("temp/es_reg_mod1_0.5.rds")),
  readRDS(paste0("temp/es_reg_mod2_0.5.rds"))
)

names(mods) <- c("t = -2", "t = -2", "t = -1", "t = -1", "t = 0", "t = 0")

rows <- tribble(~term, ~m1,  ~m2, ~m3, ~m4, ~m5, ~m6,
                "Year Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
attr(rows, 'position') <- c(47:47)

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
             title = "\\label{tab:es-reg}Individual-Level Turnout",
             latex_options = "scale_down",
             add_rows = rows,
             output = "temp/es_reg.tex",
             escape = FALSE)

j <- fread("temp/es_reg.tex", header = F, sep = "+") %>%
  mutate(n = row_number())

insert1 <- "\\resizebox*{!}{0.95\\textheight}{%"
insert2 <- "}"

j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(4.1, max(j$n) - 0.01))) %>%
  arrange(n) %>%
  select(-n)


write.table(j, "temp/es_reg.tex", quote = F, col.names = F,
            row.names = F)
