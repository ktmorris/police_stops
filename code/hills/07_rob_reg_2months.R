
#####
hills_pre_match <- readRDS("temp/real_pre_match_hills.rds") %>% 
  ungroup()

matches <- readRDS("temp/matches_hills_y.rds") %>% 
  select(-first_tr_year.y) %>% 
  rename(first_tr_year = first_tr_year.x)

matches <- left_join(matches, select(hills_pre_match, voter_id, first_tr_year),
                     by = c("group" = "voter_id", "first_tr_year")) %>% 
  mutate(fd = first_tr_year,
         first_tr_year = ifelse(first_tr_year == 1, "2014-11-04",
                                ifelse(first_tr_year == 2, "2016-11-08",
                                       ifelse(first_tr_year == 3, "2018-11-06", "XX"))),
         first_tr_year = as.Date(first_tr_year))

hist <- readRDS("temp/full_raw_coded_hills_w_bgs.rds") %>%
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
gc()
ll <- matches %>%
  filter(first_tr_year - last_date <= 90, period <= 0.5) %>%
  group_by(treated, period, black_t, paid) %>%
  summarize(to = weighted.mean(to, weight)) %>% 
  mutate(treated = ifelse(treated, "Treated", "Control"),
         black = ifelse(black_t, "Black Voters", "Non-Black Voters"),
         paid = ifelse(paid, "Paid Fine", "Did Not Pay Fine"))


ll$treated <- factor(ll$treated, levels = c("Treated", "Control"))

p1 <- ggplot(data = ll) + 
  facet_grid(paid ~ black) +
  geom_rect(aes(xmin = 0.5-.125, xmax = 0.5, ymin = 0, ymax = Inf),
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
       caption = "Treatment occurs inside of yellow band.") +
  coord_cartesian(ylim = c(0.05, 0.75))
p1
saveRDS(p1, "temp/within90days_y.rds")
##########################
ll <- matches %>%
  filter(period <= 1.5) %>% 
  group_by(treated, period, black_t, paid = civil) %>%
  summarize(to = weighted.mean(to, weight)) %>% 
  mutate(treated = ifelse(treated, "Treated", "Control"),
         black = ifelse(black_t, "Black Voters", "Non-Black Voters"),
         paid = ifelse(paid, "Paid Fine", "Did Not Pay Fine"))

ll$treated <- factor(ll$treated, levels = c("Treated", "Control"))

p2 <- ggplot(data = ll) + 
  facet_grid(paid ~ black) +
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
       caption = "Treatment occurs inside of yellow band.") +
  coord_cartesian(ylim = c(0.05, 0.75))
p2
saveRDS(p2, "temp/stopped_any_time_y.rds")
#####################
matches
matches$days_before = as.numeric(matches$first_tr_year - matches$last_date)

dat1 <- filter(matches, period <= 0.5)

m1 <- to ~ treated * post + as.factor(year)
m2 <- to ~ treated * post * days_before + as.factor(year)
m3 <- to ~ treated * post * black_t + as.factor(year)
m4 <- to ~ treated * post * civil + as.factor(year)
m5 <- to ~ treated * post * paid + as.factor(year)

models1 <- lapply(c(m1, m2, m3, m4, m5), function(f){
  m <- lm(f, data = dat1,
          weight = dat1$weight)
})


ses_cl <- list(
  summary(lm.cluster(formula = m1, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
  summary(lm.cluster(formula = m2, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
  summary(lm.cluster(formula = m3, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
  summary(lm.cluster(formula = m4, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
  summary(lm.cluster(formula = m5, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2]
)


stargazer(models1,
          type = "text",
          # column.labels = c("Stopped Any Time", "Stopped within 90 Days of Election"),
          # column.separate = c(2, 2),
          omit.stat = c("f", "ser"),
          se = ses_cl,
          omit = c("as.fac"),
          covariate.labels = c("Treated $\\times$ Post Treatment",
                               "Treated $\\times$ Post Treatment $\\times$ Days before Election",
                               "Treated $\\times$ Post Treatment $\\times$ Black",
                               "Treated $\\times$ Post Treatment $\\times$ Civil Infraction",
                               "Treated $\\times$ Post Treatment $\\times$ Paid Fine"),
            
          table.layout = "-cm#-t-a-s-n",
          keep = c("treatedTRUE:post"),
          notes = "TO REPLACE",
          title = "\\label{tab:dind-table} Turnout Effects of Tickets",
          out = "temp/two_matches_reg_y.tex",
          add.lines = list(c("Includes All Interactions", "X", "X", "X", "X", "X")))

j <- fread("./temp/two_matches_reg_y.tex", header = F, sep = "+")

note.latex <- "\\multicolumn{6}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$. \\\\Robust standard errors (clustered at level of match) in parentheses.}}}"

j <- j %>% 
  mutate(n = row_number(),
         V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
         V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>% 
  filter(!grepl("Note:", V1))

insert1 <- "\\resizebox{1\\textwidth}{!}{%"
insert2 <- "}"

j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(5.1, nrow(j) + 1 - 0.01))) %>%
  arrange(n) %>%
  select(-n)


write.table(j, "./temp/dind_reg_y.tex", quote = F, col.names = F,
            row.names = F)

########################

dat1$year <- as.factor(dat1$year)
dat1$lnfee <- log(dat1$max_amount)

tt <- lm(to ~ treated * post * amount_paid + year, dat1, weights = weight)

h <- ggeffect(tt, terms = c("days_before[all]", "treated", "post"))

h <- filter(h, facet == "TRUE")

h <- h %>% 
  mutate(group = ifelse(group == "TRUE",
                        "Treated",
                        "Control"))

inte <- reconPlots::curve_intersect(
  filter(h, group == "Treated") %>% 
    select(x, y = predicted),
  filter(h, group == "Control") %>% 
    select(x, y = predicted)
)

inte$x <- -inte$x

dat1$days_before <- -dat1$days_before
h$x <- -h$x

ggplot() + 
  geom_histogram(aes(x = days_before, y = ..count../2500000), position="identity", linetype=1,
                 fill="gray60", data = dat1, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted, color = group), data = h) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha=0.25, data = h) +
  xlab("Days before Election") +
  ylab("Predicted Turnout") + scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = percent) +
  labs(caption = "Notes: Distribution of days of tickets shown at bottom.",
       color = "Treatment Group",
       fill = "Treatment Group") +
  theme(plot.caption = element_text(hjust = 0)) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10")) +
  geom_vline(xintercept = inte$x, linetype = "dashed")

