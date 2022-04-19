
test_set <- readRDS("temp/full_set_for_mae.rds") %>% 
  filter(!is.na(share_electorate_black), !is.na(cvap_share_black))


adj_regs <- fread("raw_data/adj_regs.csv") %>% 
  mutate(state = str_pad(state, width = 2, side = "left", pad = "0"))


test_set <- left_join(test_set, adj_regs)

test_set$share_electorate_black <- test_set$share_electorate_black + test_set$to_add
################################

m1 <- mae(filter(test_set,
                 population <= quantile(test_set$population, 0.25))$cvap_share_black,
          filter(test_set,
                 population <= quantile(test_set$population, 0.25))$share_electorate_black)

m2 <- mae(filter(test_set,
                 population > quantile(test_set$population, 0.25),
                 population <= quantile(test_set$population, 0.5))$cvap_share_black,
          filter(test_set,
                 population > quantile(test_set$population, 0.25),
                 population <= quantile(test_set$population, 0.5))$share_electorate_black)

m3 <- mae(filter(test_set,
                 population > quantile(test_set$population, 0.5),
                 population <= quantile(test_set$population, 0.75))$cvap_share_black,
          filter(test_set,
                 population > quantile(test_set$population, 0.5),
                 population <= quantile(test_set$population, 0.75))$share_electorate_black)

m4 <- mae(filter(test_set,
                 population > quantile(test_set$population, 0.75))$cvap_share_black,
          filter(test_set,
                 population > quantile(test_set$population, 0.75))$share_electorate_black)

m5 <- mae(test_set$cvap_share_black, test_set$share_electorate_black)


dat <- data.table("Bottom Quartile" = m1,
                  "Second Quartile" = m2,
                  "Third Quartile" = m3,
                  "Fourth Quartile" = m4,
                  "Overall" = m5) %>% 
  mutate_all(percent, accuracy = .02)

saveRDS(dat, "temp/maes.rds")

#######################################################

test_set$er <- abs(test_set$share_electorate_black - test_set$cvap_share_black)

m1a <- feols(er ~ lndper + nh_white + nh_black + latino + asian + lpd +
            median_income + some_college + median_age + share_over_64 +
            total_revenue + share_taxes + share_s_fed | state, data = test_set,
            cluster = ~ state)

modelsummary(m1a,
             statistic = "std.error",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = c("lndper" = "Log((Dollars / Resident) + 1)",
                          "nh_white" = "Share non-Hispanic White",
                          "nh_black" = "Share non-Hispanic Black",
                          "latino" = "Share Latinx",
                          "asian" = "Share Asian",
                          "lpd" = "Log(Population Density)",
                          "median_income" = "Median Income (\\$10,000s)",
                          "some_college" = "Share with Some College",
                          "median_age" = "Median Age",
                          "share_over_64" = "Share over 64 Years Old",
                          "total_revenue" = "Log(Total Revenue)",
                          "share_taxes" = "Share of Revenue from Taxes",
                          "share_s_fed" = "Share of Revenue from State / Federal Government",
                          "(Intercept)" = "Intercept"),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             title = "\\label{tab:mae-reg} Absolute Value of Difference Between CVAP, Electorate Share Black",
             output = "latex",
             escape = FALSE,
             notes = c("State fixed effects not shown.", "Robust standard errors clustered by state.")) %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  save_kable("temp/mae_clean.tex")
