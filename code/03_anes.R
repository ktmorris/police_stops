
anes_pre <- fread("F:/regular_data/anes/anes_timeseries_2020_csv_20210719/anes_timeseries_2020_csv_20210719.csv")


anes_pre <- anes_pre %>% 
  mutate(ideol = V201200,
         age = V201507x,
         to = (V201022 == 1 & V201023 == 1) | V202066 == 4,
         white = V201549x == 1,
         black = V201549x == 2,
         latino = V201549x == 3,
         asian = V201549x == 4,
         party = ifelse(V201228 == 1 | V201230 == 3, "DEM",
                        ifelse(V201228 == 2 | V201230 == 1, "REP", "OTH")),
         vote16 = as.integer(V201101 == 1 | V201102 == 1),
         imp = V202500x)

anes_pre$party <- factor(anes_pre$party, levels = c("DEM", "REP", "OTH"))

income <- fread("../regular_data/anes/income_lu.csv")

anes_pre <- left_join(anes_pre, income) %>% 
  mutate(income = income / 10000,
         cd = paste(V203000, V203002))

reg_d <- select(anes_pre, ideol, white, black, latino, party, V200002,
                age, income, V200010b, to, asian, V201600, V201201,
                state = V201011, V201510, V202456, V202457,
                police_therm = V202171,
                fair_police = V202493x,
                self_descrim = V202537,
                linked_fate_black = V202507,
                blm = V202174, vote16, cd, imp) %>% 
  filter(V202456 %in% c(1, 2),
         V202457 %in% c(1, 2)) %>% 
  mutate(stopped = V202456 == 1,
         arrested = V202457 == 1,
         ideol = ifelse(ideol < 0, "Ideology Missing",
                        ifelse(ideol == 1, "Extremely Liberal",
                               ifelse(ideol == 2, "Liberal",
                                      ifelse(ideol == 3, "Slightly Liberal",
                                             ifelse(ideol == 4, "Moderate",
                                                    ifelse(ideol == 5, "Slightly Conservative",
                                                           ifelse(ideol == 6, "Conservative",
                                                                  ifelse(ideol == 7, "Extremely Conservative", "Don't Know Ideology")))))))),
         ideol = ifelse(ideol == "Don't Know Ideology" & V201201 == 1, "Slightly Liberal",
                        ifelse(ideol == "Don't Know Ideology" & V201201 == 2, "Slightly Conservative",
                               ideol)),
         ideol = factor(ideol, levels = c("Moderate", "Ideology Missing", "Don't Know Ideology",
                                          "Extremely Liberal", "Liberal", "Slightly Liberal",
                                          "Slightly Conservative", "Conservative", "Extremely Conservative")),
         educ = ifelse(V201510 < 0, "Education Missing",
                       ifelse(V201510 == 1, "No High School Diploma",
                              ifelse(V201510 == 2, "High School Diploma",
                                     ifelse(V201510 == 3, "Some College, No Degree",
                                            ifelse(V201510 %in% c(4:5), "Associate's Degree",
                                                   ifelse(V201510 == 6, "Bachelor's Degree",
                                                          "Post-Graduate Education")))))),
         educ = factor(educ,
                       levels = c("High School Diploma",
                                  "Education Missing",
                                  "No High School Diploma",
                                  "Some College, No Degree",
                                  "Associate's Degree",
                                  "Bachelor's Degree",
                                  "Post-Graduate Education")),
         sex = ifelse(V201600 == -9, "Refused Sex Question",
                      ifelse(V201600 == 1, "Male", "Female")),
         hied = educ %in% c("Bachelor's Degree",
                            "Post-Graduate Education"),
         hiinc = income > 10,
         vote16 = factor(vote16)) %>% 
  mutate_at(vars(white, latino, asian, to, black, stopped, arrested), ~ ifelse(., 1, 0))


m_stop <- lm(to ~ stopped*black + white + latino + asian + age + party + income + sex +
               ideol + educ + vote16, filter(reg_d), weight = V200010b)

m_stop2 <- lm(to ~ stopped*asian + white + latino + black + age + party + income + sex +
                ideol + educ + vote16, filter(reg_d), weight = V200010b)

m_stop3 <- lm(to ~ stopped*latino + white + asian + black + age + party + income + sex +
                ideol + educ + vote16, filter(reg_d), weight = V200010b)

m_stop4 <- lm(to ~ stopped*blm + age + party + income + sex +
                ideol + educ + vote16, filter(reg_d, black == 1, blm >= 0), weight = V200010b)

m_arrest <- lm(to ~ arrested*black + white + asian + latino + age + party + income + sex +
                 ideol + educ + vote16, filter(reg_d), weight = V200010b)


modelsummary(list(m_stop, m_stop2, m_stop3, m_arrest),
          statistic = "std.error",
          stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
          coef_map = c("stopped" = "Stopped in Past 12 Months",
                               "arrested" = "Ever Arrested",
                               "black" = "Black",
                               "white" = "White",
                               "asian" = "Asian",
                               "latino" = "Latinx",
                               "age" = "Age",
                               "partyREP" = "Republican",
                               "partyOTH"= "Other Party",
                               "income" = "Income (\\$10,000s)",
                               "sexMale" = "Male",
                               "sexRefused Sex Question" = "Refused Sex Question",
                               "ideolIdeology Missing" = "Ideology Missing",
                               "ideolDon't Know Ideology" = "Don't Know Ideology",
                               "ideolExtremely Liberal" = "Extremely Liberal",
                               "ideolLiberal" = "Liberal",
                               "ideolSlightly Liberal" = "Slightly Liberal",
                               "ideolSlightly Conservative" = "Slightly Conservative",
                               "ideolConservative" = "Conservative",
                               "ideolExtremely Conservative" = "Extremely Conservative",
                               "educEducation Missing" = "Education Missing",
                               "educNo High School Diploma" = "No High School Diploma",
                               "educSome College, No Degree" = "Some College, No Degree",
                               "educAssociate's Degree" = "Associate's Degree",
                               "educBachelor's Degree" = "Bachelor's Degree",
                               "educPost-Graduate Education" = "Post-Graduate Education",
                               "vote16" = "Voted in 2016",
                               "stopped:black" = "Stopped in Past 12 Months $\\times$ Black",
                               "stopped:asian" = "Stopped in Past 12 Months $\\times$ Asian",
                               "stopped:latino" = "Stopped in Past 12 Months $\\times$ Latinx",
                               "arrested:black" = "Ever Arrested $\\times$ Black"),
          gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
          output = "temp/anes_raw.tex",
          escape = F,
          title = "\\label{tab:anes-reg} Criminal Legal System Contact and 2020 Turnout",
          longtable = T)

j <- fread("./temp/anes_raw.tex", header = F, sep = "+")

note.latex <- "\\multicolumn{5}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$.\\\\Race dummies relative to \"other.\" Party dummies relative to \"Democrat.\" Sex dummies relative to \"Female.\" Ideology dummies relative to \"Moderate.\" Education dummies relative to \"High School Diploma.\"}}}"

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

write.table(j, "./temp/anes_clean.tex", quote = F, col.names = F,
            row.names = F)


#######################################
h <- ggeffect(m_stop, terms = c("stopped", "black")) %>% 
  mutate(group = ifelse(group == 1, "Black", "Not Black"),
         model = "Stopped in Past 12 Months")

ggplot(h, aes(x = x, y = predicted, color = group)) + 
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .05) +
  theme_bc(base_family = "LM Roman 10") +
  scale_x_continuous(breaks = c(0, 1),
                     labels = c("Not Stopped", "Stopped")) +
  labs(color = "Racial Group",
       x = NULL,
       y = "Predicted Turnout",
       caption = "Note: Covariates include race / ethnicity; age; party; ideology; income; education.") +
  scale_y_continuous(labels = percent)

h2 <- ggeffect(m_arrest, terms = c("arrested", "black")) %>% 
  mutate(group = ifelse(group == 1, "Black", "Not Black"),
         model = "Ever Arrested")

h3 <- bind_rows(h, h2)

ggplot(h2, aes(x = x, y = predicted, color = group)) + 
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .05) +
  theme_bc(base_family = "LM Roman 10") +
  scale_x_continuous(breaks = c(0, 1),
                     labels = c("Never Arrested", "Arrested")) +
  labs(color = "Racial Group",
       x = NULL,
       y = "Predicted Turnout") +
  scale_y_continuous(labels = percent)

h3$model <- factor(h3$model, levels = c("Stopped in Past 12 Months", "Ever Arrested"))

p2 <- ggplot(h3, aes(x = x, y = predicted, color = group, shape = group, linetype = group)) + 
  facet_wrap(~ model) +
  geom_line() + geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .05, linetype = "solid") +
  theme_bc(base_family = "LM Roman 10") +
  scale_x_continuous(breaks = c(0, 1),
                     labels = c("No", "Yes")) +
  labs(color = "Racial Group",
       shape = "Racial Group",
       linetype = "Racial Group",
       x = NULL,
       y = "Predicted Turnout",
       caption = "Note: Covariates include race / ethnicity; age; party; ideology; income; education; sex; 2016 turnout.
Full regression tables in section 4 of SI.") +
  scale_y_continuous(labels = percent) +
  scale_linetype_manual(values = c("solid", "dashed"))
p2
save(p2, file = "temp/anes_plot.rdata")

#############################

bl <- filter(reg_d, black == 1, linked_fate_black > 0)

m_stop2 <- lm(to ~ stopped*linked_fate_black + white + latino + asian + age + party + income + sex +
                  ideol + vote16 + educ + state, filter(bl), weight = bl$V200010b)

summary(m_stop2)

h <- ggeffect(m_stop2, terms = c("stopped", "linked_fate_black"))
plot(h)
