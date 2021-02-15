
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

to <- bind_rows(readRDS("temp/city_to_14_reg.rds") %>% 
                  mutate(race = "overall"),
                readRDS("temp/city_to_14_reg.rds")%>%
                  filter(EthnicGroups_EthnicGroup1Desc %in% c("European",
                                                              "Likely African-American")) %>% 
                  mutate(race = ifelse(EthnicGroups_EthnicGroup1Desc == "European",
                                       "white",
                                       "black"))) %>% 
  group_by(state, GEOID = plasub, race) %>%
  summarize(to_18 = sum(count * to_18) / sum(count),
            to_14 = sum(count * to_14) / sum(count),
            voters = sum(count)) %>% 
  pivot_longer(cols = starts_with("to_"), names_to = "year", values_to = "to") %>% 
  mutate(year = as.integer(gsub("to_", "20", year)),
         year = ifelse(year == 2018, 2017, 2012)) %>% 
  pivot_wider(id_cols = c("GEOID", "year", "state"), names_from = "race",
              values_from = c("to", "voters"))


cities1 <- left_join(cities, to) %>% 
  mutate(to_overall = (to_overall * voters_overall) / cvap,
         to_white = (to_white * voters_white) / white_cvap,
         to_black = (to_black * voters_black) / black_cvap)

m1 <- plm(to_overall ~ lndper + nh_white + nh_black + latino + pop_dens +
            median_income + some_college + median_age + share_over_64 +
            share_taxes + share_state_fed + total_revenue + state, 
          data = filter(cities1, !is.na(to_overall), !is.infinite(to_overall)), 
          index = c("GEOID", "year"), 
          model = "within", 
          effect = "twoways")

m2 <- plm(to_white ~ lndper + nh_white + nh_black + latino + pop_dens +
            median_income + some_college + median_age + share_over_64 +
            share_taxes + share_state_fed + total_revenue + state, 
          data = filter(cities1, !is.na(to_white), !is.infinite(to_white), white_cvap > 100), 
          index = c("GEOID", "year"), 
          model = "within", 
          effect = "twoways")

m3 <- plm(to_black ~ lndper * nh_white + nh_black + latino + pop_dens +
            median_income + some_college + median_age + share_over_64 +
            share_taxes + share_state_fed + total_revenue + state, 
          data = filter(cities1, !is.na(to_black), !is.infinite(to_black),
                        to_black <= 1),
          index = c("GEOID", "year"), 
          model = "within", 
          effect = "twoways")

stargazer(m1, m2, m3, type = "text",
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
          dep.var.labels = c("Overall Turnout", "White Turnout", "Black Turnout"),
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

insert1 <- "\\resizebox{1\\textwidth}{.5\\textheight}{%"
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
