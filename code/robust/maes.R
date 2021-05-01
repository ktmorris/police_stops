
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

m1a <- lm(er ~ lndper + nh_white + nh_black + latino + asian + lpd +
            median_income + some_college + median_age + share_over_64 +
            state + total_revenue + share_taxes + share_s_fed, data = test_set)

m1 <- summary(lm.cluster(er ~ lndper + nh_white + nh_black + latino + asian + lpd +
                           median_income + some_college + median_age + share_over_64 +
                           state + total_revenue + share_taxes + share_s_fed, data = test_set, cluster = test_set$state))[,2]


stargazer(m1a, type = "text", omit = c("state"),
          column.labels = "Error",
          dep.var.labels = "",
          covariate.labels = c("Log((Dollars / Resident) + 1)",
                               "% nonHispanic White",
                               "% nonHispanic Black",
                               "% Latinx",
                               "% Asian",
                               "Log(Population Density)",
                               "Median Income (dollarsign10,000s)",
                               "% with Some College",
                               "Median Age",
                               "Share over 64",
                               "Total Revenue",
                               "% of Rev from Taxes",
                               "% of Rev from State / Fed Gov."),
          se = list(m1),
          add.lines = list(c("State fixed effects", "X", "X", "X")),
          notes = "TO REPLACE",
          omit.stat = c("F", "ser"),
          out = "temp/cog_rob_mae.tex",
          table.placement = "H",
          title = "\\label{tab:mae:reg} Absolute Value of Difference Between CVAP, Electorate Share Black")

j <- fread("./temp/cog_rob_mae.tex", header = F, sep = "+") %>% 
  mutate(V1 = gsub("[%]", "Share", V1))

note.latex <- "\\multicolumn{2}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$.\\\\Robust standard errors clustered by state.}}}"

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

write.table(j[c(3:nrow(j)),], "./temp/mae_clean.tex", quote = F, col.names = F,
            row.names = F)