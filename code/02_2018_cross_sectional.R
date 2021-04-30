

cog <- readRDS("temp/cog_cities.rds") %>% 
  mutate(state = as.factor(state),
         fines_rev = dper / total_revenue)

place_to <- readRDS("temp/place_to.rds") %>% 
  rename(plasub = place)

county_s_to <- readRDS("temp/county_s_to.rds") %>% 
  rename(plasub = county_s)

county_s_to <- county_s_to[!(county_s_to$plasub %in% place_to$plasub),]

city_to <- bind_rows(place_to, county_s_to)

city_to <- city_to[city_to$plasub %in% cog$GEOID, ]

to <- bind_rows(city_to %>% 
                  mutate(race = "overall"),
                city_to%>%
                  mutate(race = ifelse(EthnicGroups_EthnicGroup1Desc == "Likely African-American",
                                       "black",
                                       "nonblack")),
                city_to%>%
                  mutate(race = ifelse(EthnicGroups_EthnicGroup1Desc == "European",
                                       "white",
                                       "nonwhite")),
                city_to%>%
                  filter(!(EthnicGroups_EthnicGroup1Desc %in% c("European", "Likely African-American"))) %>% 
                  mutate(race = ifelse(EthnicGroups_EthnicGroup1Desc == "Hispanic and Portuguese", "latino",
                                       ifelse(EthnicGroups_EthnicGroup1Desc == "East and South Asian", "asian", "other")))) %>% 
  group_by(plasub, race) %>%
  mutate(votes_18 = count * to_18,
         votes_16 = count * to_16,
         votes_10 = count * to_10) %>% 
  summarize(to_18 = sum(votes_18) / sum(count),
            to_16 = sum(votes_16) / sum(count),
            votes_18 = sum(votes_18),
            votes_16 = sum(votes_16),
            votes_10 = sum(votes_10)) %>% 
  pivot_wider(id_cols = "plasub", names_from = "race", values_from = c("to_18",
                                                                       "to_16",
                                                                       "votes_18",
                                                                       "votes_16",
                                                                       "votes_10"))

full_set <- inner_join(cog, to, by = c("GEOID" = "plasub")) %>% 
  mutate(vap_to_18_overall = votes_18_overall / cvap,
         vap_to_10_overall = votes_10_overall / cvap,
         vap_to_18_white = votes_18_white / white_cvap,
         vap_to_10_white = votes_10_white / white_cvap,
         vap_to_18_black = votes_18_black / black_cvap,
         vap_to_10_black = votes_10_black / black_cvap,
         vap_to_18_nonwhite = votes_18_nonwhite / nonwhite_cvap,
         vap_to_10_nonwhite = votes_10_nonwhite / nonwhite_cvap,
         vap_to_18_nonblack = votes_18_nonblack / (cvap - black_cvap),
         vap_to_10_nonblack = votes_10_nonblack / (cvap - black_cvap),
         vap_to_18_latino = votes_18_latino / latino_cvap,
         vap_to_10_latino = votes_10_latino / latino_cvap,
         vap_to_18_asian = votes_18_asian / asian_cvap,
         vap_to_10_asian = votes_10_asian / asian_cvap,
         vap_to_18_other = votes_18_other / other_cvap,
         vap_to_10_other = votes_10_other / other_cvap)

p1 <- ggplot(full_set, aes(x = lndper, y = vap_to_18_black)) + geom_point(shape = 1) + geom_smooth(method = "lm") +
  xlab("Dollars per Resident") +
  ylab("Turnout in 2018") + scale_x_continuous(breaks = c(log(1), log(2), log(11), log(101), log(201)),
                                                                           labels = c("$0", "$1", "$10", "$100", "$200")) +
  scale_y_continuous(labels = percent) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10")) +
  coord_cartesian(xlim = c(0, log(201)),
                  ylim = c(0, .825)) +
  labs(caption = "
       
       ")
p1

#################################################

full_set <- rename(full_set, share_s_fed = share_state_fed) %>% 
  mutate(median_income = median_income / 10000,
         lpd = log(pop_dens))

covars <- gsub("\\n|            ", "", "lndper + nh_white + nh_black + latino + asian + lpd +
            median_income + some_college + median_age + share_over_64 +
            state + total_revenue + share_taxes + share_s_fed")

ms <- data.frame(m = c("vap_to_18_overall", "vap_to_18_black", "vap_to_18_nonblack"),
                 name = c("Overall Turnout", "Black Turnout", "Non-Black Turnout"))


models <- lapply(ms$m, function(f){
  lm(as.formula(paste0(f, " ~ ", covars)),
     data = filter(full_set, !!sym(f) <= 1))
})

ses <- lapply(ms$m, function(f){
  summary(lm.cluster(as.formula(paste0(f, " ~ ", covars)),
                     data = filter(full_set, !!sym(f) <= 1),
                     cluster = filter(full_set, !!sym(f) <= 1)$state))[,2]
})


stargazer(models, type = "text", omit = c("state"),
          column.labels = ms$name,
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
          se = ses,
          add.lines = list(c("State fixed effects", "X", "X", "X")),
          notes = "TO REPLACE",
          omit.stat = c("F", "ser"),
          out = "temp/cog_cross_raw.tex",
          table.placement = "H",
          title = "\\label{tab:cog-cross-reg} Fees and Fines and 2020 Turnout")


j <- fread("./temp/cog_cross_raw.tex", header = F, sep = "+") %>% 
  mutate(V1 = gsub("[%]", "Share", V1))

note.latex <- "\\multicolumn{4}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$.\\\\Robust standard errors clustered by state.}}}"

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

write.table(j[c(3:nrow(j)),], "./temp/cog_cross_clean.tex", quote = F, col.names = F,
            row.names = F)

#############################
minq <- round(min(full_set$lndper),digits = 3)
q10 <-  round(quantile(full_set$lndper, 0.1), digits = 3)
q25 <-  round(quantile(full_set$lndper, 0.25), digits = 3)
q75 <-  round(quantile(full_set$lndper, 0.75), digits = 3)
q90 <-  round(quantile(full_set$lndper, 0.9), digits = 3)
maxq <- round(max(full_set$lndper), digits = 3)

marg1 <- ggeffect(models[[2]], terms = paste("lndper [0", minq, q10, q25, q75, q90, maxq, "1]", sep = ", "),
                  vcov.fun = "vcovCR", vcov.type = "CR0", 
                 vcov.args = list(cluster = filter(covars, vap_to_18_overall <= 1)$state)) %>% 
  mutate(model = "Black")

marg2 <- ggeffect(models[[3]], terms = paste("lndper [0", minq, q10, q25, q75, q90, maxq, "1]", sep = ", "),
                  vcov.fun = "vcovCR", vcov.type = "CR0", 
                  vcov.args = list(cluster = filter(full_set, vap_to_18_nonblack <= 1)$state)) %>% 
  mutate(model = "Non-Black")

marg3 <- ggeffect(models[[1]], terms = paste("lndper [0", minq, q10, q25, q75, q90, maxq, "1]", sep = ", "),
                  vcov.fun = "vcovCR", vcov.type = "CR0", 
                  vcov.args = list(cluster = filter(covars, vap_to_18_white <= 1)$state)) %>% 
  mutate(model = "Overall")


marg <- bind_rows(marg1, marg2)

bars <- data.frame(x = c(q10, q90),
                   y1 = c(
                     filter(marg1,
                            x == q10) %>% 
                       select(predicted) %>% 
                       distinct() %>% 
                       pull(),
                     filter(marg1,
                            x == q90) %>% 
                       select(predicted) %>% 
                       distinct() %>% 
                       pull()
                   ),
                   y2 = c(
                     filter(marg2,
                            x == q10) %>% 
                       select(predicted) %>% 
                       distinct() %>% 
                       pull(),
                     filter(marg2,
                            x == q90) %>% 
                       select(predicted) %>% 
                       distinct() %>% 
                       pull()
                   )) %>% 
  mutate(y1 = percent(y1, accuracy = 0.1),
         y2 = percent(y2, accuracy = 0.1))

p2 <- ggplot() + 
  geom_histogram(aes(x = lndper, y = ..count../3000), position="identity", linetype=1,
                 fill="gray60", data = full_set, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted, color = model), data = marg) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = model), alpha=0.25, data = marg) +
  xlab("Dollars per Resident") +
  ylab("Predicted Turnout in 2018") + scale_x_continuous(breaks = c(log(1), log(2), log(11), log(101), log(201)),
                                               labels = c("$0", "$1", "$10", "$100", "$200")) +
  scale_y_continuous(labels = percent) +
  coord_cartesian(xlim = c(-.3, log(201)), ylim = c(0, .825)) +
  labs(caption = "Notes: Distribution of Dollars per Resident shown at bottom. Robust standard errors clustered by state.
Covariates: % White, % Black, % Latinx, % Asian, Median Income, % with Some College, Median Age, % Over 64 Years Old, 
Population Density, Total Revenue, Share of Revenue from Taxes, Share of Revenue from State / Fed. Gov, State Fixed Effects.") +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10")) +
  geom_vline(xintercept = bars$x[1], color = "black", linetype = "dashed") +
  geom_vline(xintercept = bars$x[2], color = "black", linetype = "dashed") +
  geom_text(aes(bars$x[1], .6, label = paste0(" Non-Black Turnout
 at 10th Percentile:
 ", bars$y2[1]),
                family = "LM Roman 10", hjust = 0)) +
  geom_text(aes(bars$x[2], .6, label = paste0(" Non-Black Turnout
 at 90th Percentile:
 ", bars$y2[2]),
                family = "LM Roman 10", hjust = 0)) +
  geom_text(aes(bars$x[1], .3, label = paste0(" Black Turnout
 at 10th Percentile:
 ", bars$y1[1]),
                family = "LM Roman 10", hjust = 0)) +
  geom_text(aes(bars$x[2], .3, label = paste0(" Black Turnout
 at 90th Percentile:
 ", bars$y1[2]),
                family = "LM Roman 10", hjust = 0)) +
  labs(color = "Model",
       fill = "Model")

p2
save(p2, bars, file = "temp/mef_to.rdata")
