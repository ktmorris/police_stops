
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
ll <- matches %>%
  filter(first_tr_year - last_date <= 90, period <= 0.5, period >= -1.5) %>%
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
       caption = "Treatment occurs inside of yellow band.") 
p1
saveRDS(p1, "temp/within90days_y.rds")
##########################
ll <- matches %>%
  filter(period <= .5, period >= -1.5) %>% 
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
       caption = "Treatment occurs inside of yellow band.")
p2
saveRDS(p2, "temp/stopped_any_time_y.rds")
#####################
