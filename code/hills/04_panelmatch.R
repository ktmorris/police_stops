hills_pre_match <- readRDS("temp/hills_pre_match.rds")

hist <- readRDS("raw_data/fl_l2_hills/fl_l2_history_hills.rds") %>%
  select(-state, -starts_with("Loc")) %>%
  pivot_longer(c(starts_with("Gener")), names_to = "year", values_to = "to") %>%
  mutate(to = to == "Y")

hills_pre_match <- left_join(hills_pre_match, hist) %>% 
  mutate(year = as.Date(gsub("General_|Local_or_Municipal_", "", year), "%Y_%m_%d"),
         treated = year > fd)


hills_pre_match <- as.data.frame(hills_pre_match %>% 
                                   mutate(LALVOTERID = as.integer(gsub("LALFL", "", LALVOTERID)),
                                          year = as.integer((year(year) / 2) - 1005)))

pm_tot <- PanelMatch(lag = 4, time.id = "year", unit.id = "LALVOTERID", 
                       treatment = "treated", refinement.method = "mahalanobis", 
                       data = hills_pre_match, match.missing = TRUE,
                       covs.formula = ~ white + black + asian + latino +
                         male + dem + rep + age +
                         reg_date + pre_12 + median_income + some_college + unem +
                         I(lag(to, 1:4)), 
                       exact.match.variables = c("dem", "rep", "male", "white", "black",
                                                 "latino", "asian"),
                       size.match = 5, qoi = "att", outcome.var = "to",
                       forbid.treatment.reversal = FALSE, 
                       use.diagonal.variance.matrix = TRUE)

saveRDS(pm_tot, "temp/pm_tot.rds")

pm_black <- PanelMatch(lag = 4, time.id = "year", unit.id = "LALVOTERID", 
                       treatment = "treated", refinement.method = "mahalanobis", 
                       data = hills_pre_match[hills_pre_match$black == T, ], match.missing = TRUE,
                       covs.formula = ~ male + dem + rep + age +
                         reg_date + pre_12 + median_income + some_college + unem +
                         I(lag(to, 1:4)), 
                       exact.match.variables = c("dem", "rep", "male"),
                       size.match = 5, qoi = "att", outcome.var = "to",
                       forbid.treatment.reversal = FALSE, 
                       use.diagonal.variance.matrix = TRUE)
saveRDS(pm_black, "temp/pm_black.rds")

pm_white <- PanelMatch(lag = 4, time.id = "year", unit.id = "LALVOTERID", 
                       treatment = "treated", refinement.method = "mahalanobis", 
                       data = hills_pre_match[hills_pre_match$white == T, ], match.missing = TRUE,
                       covs.formula = ~ male + dem + rep + age +
                         reg_date + pre_12 + median_income + some_college + unem +
                         I(lag(to, 1:2)), 
                       exact.match.variables = c("dem", "rep", "male"),
                       size.match = 5, qoi = "att", outcome.var = "to",
                       forbid.treatment.reversal = FALSE, 
                       use.diagonal.variance.matrix = TRUE)
saveRDS(pm_white, "temp/pm_white.rds")