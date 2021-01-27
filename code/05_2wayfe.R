
cities1 <- readRDS("temp/cog_cities.rds") %>% 
  select(GEOID, lndper, total_revenue, share_state_fed, share_taxes, place_id) %>% 
  mutate(year = 2017)


cities2 <- inner_join(select(cities1, place_id, GEOID),
                      readRDS("temp/cog_12.rds")) %>% 
  select(GEOID, lndper, total_revenue, share_state_fed, share_taxes) %>% 
  mutate(year = 2012)

cities <- bind_rows(cities1, cities2) %>% 
  select(-place_id)

census <- bind_rows(
  readRDS("temp/census_12.rds") %>% 
    mutate(year = 2012),
  readRDS("temp/census_data.rds") %>% 
    mutate(year = 2017)
)

cities <- left_join(cities, census)

to <- readRDS("temp/city_to_14_reg.rds") %>% 
  group_by(plasub, state) %>%
  summarize_at(vars(to_18, to_16, to_14, to_12, to_10),
               ~ weighted.mean(., count)) %>% 
  select(GEOID = plasub, state, to_18, to_14) %>% 
  pivot_longer(cols = starts_with("to_"), names_to = "year", values_to = "to") %>% 
  mutate(year = as.integer(gsub("to_", "20", year)),
         year = ifelse(year == 2018, 2017, 2012))

to2 <- readRDS("temp/city_to_14_reg.rds") %>% 
  filter(EthnicGroups_EthnicGroup1Desc %in% c("European", "Likely African-American")) %>% 
  mutate(race = ifelse(EthnicGroups_EthnicGroup1Desc == "European", "White", "Nonwhite")) %>% 
  group_by(plasub, state, race) %>%
  summarize_at(vars(to_18, to_16, to_14, to_12, to_10),
               ~ sum(. * count)) %>% 
  select(GEOID = plasub, race, state, to_18, to_14) %>% 
  pivot_longer(cols = starts_with("to_"), names_to = "year", values_to = "to") %>% 
  mutate(year = as.integer(gsub("to_", "20", year)),
         year = ifelse(year == 2018, 2017, 2012)) %>% 
  pivot_wider(id_cols = c("GEOID", "year", "state"), names_from = "race", values_from = "to")


cities1 <- left_join(cities, to)

cities_bal1 <- cities1[complete.cases(cities1), ] %>% 
  select(-ends_with("cvap")) %>% 
  group_by(GEOID) %>% 
  filter(n() == 2) %>% 
  mutate(state = as.factor(state),
         year = as.factor(year),
         GEOID = as.factor(GEOID))

cities2 <- left_join(cities, to2)

cities_bal2 <- cities2[complete.cases(cities2), ] %>% 
  filter(White / white_cvap <= 1,
         Nonwhite / black_cvap <= 1) %>% 
  mutate(to_gap = (White / white_cvap) - (Nonwhite / black_cvap)) %>% 
  select(-White, -Nonwhite, -ends_with("cvap")) %>% 
  filter(!is.infinite(to_gap)) %>% 
  group_by(GEOID) %>% 
  filter(n() == 2) %>% 
  mutate(state = as.factor(state),
         year = as.factor(year),
         GEOID = as.factor(GEOID))


m1 <- plm(to ~ lndper + nh_white + nh_black + latino + pop_dens +
            median_income + some_college + median_age + share_over_64 +
            share_taxes + share_state_fed + total_revenue, 
          data = cities_bal1, 
          index = c("GEOID", "year"), 
          model = "within", 
          effect = "twoways")

m2 <- plm(to_gap ~ lndper + nh_white + nh_black + latino + pop_dens +
            median_income + some_college + median_age + share_over_64 +
            share_taxes + share_state_fed + total_revenue, 
          data = cities_bal2, 
          index = c("GEOID", "year"), 
          model = "within", 
          effect = "twoways")

stargazer(m1, m2, type = "text",
          covariate.labels = c("Log(Dollars / Resident)",
                               "Share non-Hispanic White",
                               "Share non-Hispanic Black",
                               "Share Latinx",
                               "Population Density",
                               "Median Income",
                               "Share with Some College",
                               "Median Age",
                               "Share over 64 Years Old",
                               "Share of Revenue from Taxes",
                               "Share of Revenue from State / Federal Government",
                               "Total Revenue"),
          dep.var.labels = c("Turnout", "White-Black Turnout Gap"),
          out = "temp/2wfe_reg.tex",
          notes = "TO REPLACE",
          title = "\\label{tab:tab1} Two-Way Fixed Effects Models")

j <- fread("./temp/2wfe_reg.tex", header = F, sep = "+")

note.latex <- "\\multicolumn{2}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$.}}}"

j <- j %>%
  mutate(n = row_number(),
         V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
         V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>%
  filter(!grepl("Note:", V1))

insert1 <- "\\resizebox{!}{.5\\textheight}{%"
insert2 <- "}"

j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(5.1, nrow(j) + 1 - 0.01))) %>%
  mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>%
  arrange(n) %>%
  select(-n)

write.table(j, "./temp/2wfe_reg_clean.tex", quote = F, col.names = F,
            row.names = F)

# marg <- ggeffect(m1, "lndper [0.01476405, 1.113368, 3.856901, 7.9801]")
# 
# ################################################
# bars <- data.frame(x = c(quantile(cities_bal$lndper, 0.1),
#                          quantile(cities_bal$lndper, 0.9)),
#                    y = c(marg$predicted[2:3])) %>% 
#   mutate(y = percent(y, accuracy = 0.1))
# 
# p2 <- ggplot() + 
#   geom_histogram(aes(x = lndper, y = ..count../3000), position="identity", linetype=1,
#                  fill="gray60", data = cities_bal, alpha=0.5, bins = 30) + 
#   geom_line(aes(x = x, y = predicted), data = marg, color = "black") +
#   geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "black", alpha=0.25, data = marg) +
#   xlab("Dollars per Resident") +
#   ylab("Turnout") + scale_x_continuous(breaks = c(log(1), log(2), log(11), log(101), log(201)),
#                                                labels = c("$0", "$1", "$10", "$100", "$200")) +
#   scale_y_continuous(labels = percent) +
#   coord_cartesian(xlim = c(-.3, log(201)), ylim = c(0, .825)) +
#   labs(caption = "Notes: Distribution of Dollars per Resident shown at bottom. Robust standard errors clustered by state.
# Covariates: % White, % Black, % Latinx, Median Income, % with Some College, Median Age, % Over 64 Years Old, 
# Population Density, Total Revenue, Share of Revenue from Taxes, Share of Revenue from State / Fed. Gov.
# Includes State, Year, and Municipality Fixed Effects.") +
#   theme_bw() + theme(plot.caption = element_text(hjust = 0),
#                      text = element_text(family = "LM Roman 10")) +
#   geom_vline(xintercept = bars$x[1], color = "black", linetype = "dashed") +
#   geom_vline(xintercept = bars$x[2], color = "black", linetype = "dashed") +
#   geom_text(aes(bars$x[1], .72, label = paste0(" Predicted Turnout at
#  10th Percentile:
#  ", bars$y[1]),
#                 family = "LM Roman 10", hjust = 0)) +
#   geom_text(aes(bars$x[2], .7, label = paste0(" Predicted Turnout at
#  90th Percentile:
#  ", bars$y[2]),
#                 family = "LM Roman 10", hjust = 0))
# 
# p2
# 
# saveRDS(p2, "temp/mef_2wfe.rds")
