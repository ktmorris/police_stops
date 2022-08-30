
#####
matches <- readRDS("temp/real_pre_match_hills_anon.rds") %>% 
  ungroup()


matches <- matches %>% 
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
                       filter(year(year) %% 2 == 0), by = c("voter_id" = "voter_id"))

periods <- fread("raw_data/period_lu.csv") %>% 
  mutate_at(vars(first_tr_year, year), as.Date, "%m/%d/%Y")

matches <- left_join(matches, periods) %>% 
  filter(period %in% c(-2.5, -1.5, -.5, .5, 1.5))

matches <- matches %>% 
  mutate(post = period >= 0.5)
matches$weight <- 1
cleanup("matches")
gc()

##########################################################
ll <- bind_rows(mutate(matches, first_tr_year = as.character(first_tr_year)),
                mutate(matches, first_tr_year = "Overall")) %>%
  filter(period <= 0.5) %>% 
  group_by(treated, period, black, first_tr_year) %>%
  summarize(to = weighted.mean(to, weight)) %>% 
  mutate(treated = ifelse(treated, "Treated", "Control"),
         black = ifelse(black, "Black Voters", "Non-Black Voters"))

ll$treated <- factor(ll$treated, levels = c("Treated", "Control"))

ll <- ll %>% 
  mutate(first_tr_year = ifelse(first_tr_year == "Overall", first_tr_year,
                                paste("t = 0\nin", substring(first_tr_year, 1, 4))))

ll$first_tr_year <- factor(ll$first_tr_year, levels = unique(ll$first_tr_year))

p2 <- ggplot(data = ll) + 
  facet_grid(first_tr_year ~ black) +
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
  theme_bc(base_family = "Latin Modern Roman") +
  scale_y_continuous(labels = percent) +
  labs(x = "t", y = "Turnout",
       linetype = "Treatment Group",
       shape = "Treatment Group",
       caption = "Treatment occurs inside of yellow band.
Full regression tables in section 3 of SI.", ) +
  coord_cartesian(ylim = c(0.05, 0.75))
p2
saveRDS(p2, "temp/stopped_any_time_no_matching.rds")

#####################

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

matches$first_tr_year <- as.character(matches$first_tr_year)
saveRDS(matches, "temp/full_reg_data_no_matching.rds")
for(gg in c("overall", "2014-11-04", "2016-11-08", "2018-11-06")){
  
  if(gg == "overall"){
    dat1 <- filter(matches, period <= 0.5)
    tit = "\\label{tab:dind-table} Overall Treatment Effect"
    ooo = c(1, 2, 4, 23, 24, 22, 25)
  }else{
    dat1 <- dplyr::filter(matches, period <= 0.5, first_tr_year == gg)
    tit = paste0("\\label{tab:dind-table-", substring(gg, 1, 4), "}Treatment Effect for Voters Stopped before ", substring(gg, 1, 4), " Election")
    ooo = c(1, 2, 7, 23, 24, 22, 25)
  }
  
  models1 <- lapply(c(m1, m2, m3, m4), function(f){
    m <- feols(f, data = dat1,
               weight = ~ weight, cluster = "voter_id")
  })
  
  rows <- tribble(~term, ~m1,  ~m2, ~m3, ~m4,
                  "Year Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
  
  attr(rows, 'position') <- c(13)
  
  modelsummary(models1,
               statistic = "std.error",
               stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
               coef_map = c("treatedTRUE:postTRUE" = "Treated $\\times$ Post Treatment",
                            "treatedTRUE:postTRUE:blackTRUE" = "Treated $\\times$ Post Treatment $\\times$ Black",
                            "treatedTRUE" = "Treated",
                            "postTRUE" = "Post Treatment",
                            "blackTRUE" = "Black",
                            "(Intercept)" = "Intercept"),
               gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
               title = tit,
               latex_options = "scale_down",
               add_rows = rows,
               output = paste0("temp/small_two_matches_reg_", gg, "_no_matching.tex"),
               escape = FALSE)
  
  attr(rows, 'position') <- 53
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
               output = paste0("temp/two_matches_reg_", gg, "_no_matching.tex"),
               escape = FALSE)
  
  j <- fread(paste0("temp/two_matches_reg_", gg, "_no_matching.tex"), header = F, sep = "+") %>% 
    mutate(n = row_number())
  
  insert1 <- "\\resizebox*{!}{0.95\\textheight}{%"
  insert2 <- "}"
  
  j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(4.1, nrow(j) - 0.01))) %>% 
    mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>% 
    arrange(n) %>% 
    select(-n)
  
  
  write.table(j, paste0("./temp/dind_reg_y", gg, "_no_matching.tex"), quote = F, col.names = F,
              row.names = F)
  
}



