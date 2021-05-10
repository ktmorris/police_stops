
#####
hills_pre_match <- readRDS("temp/real_pre_match_hills_mayor.rds") %>% 
  ungroup()

matches <- readRDS("temp/matches_hills_y_mayor.rds")

matches <- left_join(matches, select(hills_pre_match, voter_id, first_tr_year),
                     by = c("group" = "voter_id", "first_tr_year")) %>% 
  mutate(fd = first_tr_year,
         first_tr_year = ifelse(first_tr_year == 1, "2015-03-03",
                                ifelse(first_tr_year == 2, "2019-03-05", "XX")),
         first_tr_year = as.Date(first_tr_year))

hist <- readRDS("temp/full_raw_coded_hills_w_bgs.rds") %>%
  select(voter_id, starts_with("v1"), v08, v07) %>%
  pivot_longer(!starts_with("vo"), names_to = "year", values_to = "to")

elec_dates <- fread("raw_data/election_dates.csv")

hist <- left_join(hist, elec_dates) %>% 
  select(-year) %>% 
  rename(year = date) %>% 
  mutate(year = as.Date(year, "%m/%d/%Y"))


matches <- left_join(matches, hist %>% 
                       filter(year(year) %% 2 == 1), by = c("voter" = "voter_id"))

periods <- fread("raw_data/period_lu_mayor.csv") %>% 
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
gc()
##########################
ll <- matches %>%
  filter(period <= .5, period >= -1.5) %>% 
  group_by(treated, period, black_t) %>%
  summarize(to = weighted.mean(to, weight)) %>% 
  mutate(treated = ifelse(treated, "Treated", "Control"),
         black = ifelse(black_t, "Black Voters", "Non-Black Voters"))

ll$treated <- factor(ll$treated, levels = c("Treated", "Control"))

p2 <- ggplot(data = ll) + 
  facet_grid( ~ black) +
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
  theme_bc(base_family = "LM Roman 10") +
  scale_y_continuous(labels = percent) +
  labs(x = "t", y = "Turnout",
       linetype = "Treatment Group",
       shape = "Treatment Group",
       caption = "Treatment occurs inside of yellow band.")
p2
ggsave(plot = p2, file = "markdown/local_fig.png")
#####################

dat1 <- filter(matches, period <= 0.5)

m1 <- to ~ treated * post + as.factor(year)

m2 <- to ~ treated * post + as.factor(year) +
  white + black + latino + asian + male +
  dem + rep + age + reg_date + pre_stops + v1 + v2 +
  median_income + some_college + unem + civil + paid + tampa_pd

m3 <- to ~ treated * post * black + as.factor(year)

m4 <- to ~ treated * post * black + as.factor(year) +
  white + black + latino + asian + male +
  dem + rep + age + reg_date + pre_stops + v1 + v2 +
  median_income + some_college + unem + civil + paid + tampa_pd

models1 <- lapply(c(m1, m2, m3, m4), function(f){
  m <- lm(f, data = dat1,
          weight = dat1$weight)
})


ses_cl <- list(
  summary(lm.cluster(formula = m1, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
  summary(lm.cluster(formula = m2, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
  summary(lm.cluster(formula = m3, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
  summary(lm.cluster(formula = m4, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2]
)


stargazer(models1,
          type = "text",
          omit.stat = c("f", "ser"),
          se = ses_cl,
          omit = c("as.fac"),
          covariate.labels = c("Treated",
                               "Post Treatment",
                               "Treated $\\times$ Post Treatment",
                               "Black",
                               "Treated $\\times$ Black",
                               "Post Treatment $\\times$ Black",
                               "Treated $\\times$ Post Treatment $\\times$ Black"),
          table.layout = "-cm#-t-a-s-n",
          keep = c("treatedTRUE", "post", "blackTRUE", "Constant"),
          order = c(1, 2, 4, 22, 23, 21, 24),
          notes = "TO REPLACE",
          table.placement = "H",
          title = "\\label{tab:dind-table} Turnout Effects of Tickets",
          out = "temp/two_matches_reg_y_loc.tex",
          add.lines = list(c("Includes Matched Covariates", "", "X", "", "X"),
                           c("Includes Year Fixed Effects", "X", "X", "X", "X")))

j <- fread("./temp/two_matches_reg_y_loc.tex", header = F, sep = "+")

note.latex <- "\\multicolumn{5}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$. \\\\Robust standard errors (clustered at level of match) in parentheses.}}}"

j <- j %>% 
  mutate(n = row_number(),
         V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
         V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>% 
  filter(!grepl("Note:", V1))


j <- j %>%
  arrange(n) %>%
  select(-n)


write.table(j, "./temp/dind_reg_y_loc.tex", quote = F, col.names = F,
            row.names = F)
