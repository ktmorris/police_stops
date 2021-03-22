

hills_pre_match <- readRDS("temp/hills_pre_match.rds") %>% 
  ungroup()

matches_wide <- rbindlist(lapply(c(
  "temp/matches_hills_14_rob.rds",
  "temp/matches_hills_16_rob.rds",
  "temp/matches_hills_18_rob.rds"), readRDS))

hist <- readRDS("temp/full_raw_coded_hills_w_bgs.rds") %>%
  select(voter_id, starts_with("v1")) %>%
  pivot_longer(starts_with("v1"), names_to = "year", values_to = "to")

elec_dates <- fread("raw_data/election_dates.csv")

hist <- left_join(hist, elec_dates) %>% 
  select(-year) %>% 
  rename(year = date) %>% 
  mutate(year = as.Date(year))


matches <- left_join(matches_wide, hist %>% 
                       filter(year(year) %% 2 == 0), by = c("voter" = "voter_id"))

periods <- fread("raw_data/period_lu.csv") %>% 
  mutate_at(vars(first_tr_year, year), as.Date)

matches <- left_join(matches, periods)

matches <- left_join(matches,
                     hills_pre_match %>%
                       select(-GEOID, -fd, -max_amount),
                     by = c("voter" = "voter_id"))

matches <- left_join(matches,
                     hills_pre_match %>%
                       select(voter_id, max_amount, fd, black_t = black),
                     by = c("group" = "voter_id")) %>% 
  mutate(post = period >= 0.5,
         treated = voter == group)

ll <- matches %>%
  filter(period <= 1.5) %>% 
  group_by(treated, period, black_t) %>%
  summarize(to = weighted.mean(to, weight)) %>% 
  mutate(treated = ifelse(treated, "Treated", "Control"),
         black = ifelse(black_t, "Black Voters", "Non-Black Voters"))


ll$treated <- factor(ll$treated, levels = c("Treated", "Control"))

p1 <- ggplot(data = ll) + 
  facet_grid(~ black) +
  geom_rect(aes(xmin = 0.5+.125, xmax = 0.5, ymin = 0, ymax = Inf),
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
