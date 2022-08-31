

raw <- readRDS("temp/stopped_any_time_no_matching.rds")[["data"]] %>% 
  mutate(type = "Unmatched")
matched <- readRDS("temp/stopped_any_time.rds")[["data"]] %>% 
  mutate(type = "Matched\n(With Prior Turnout)")

matched2 <- readRDS("temp/stopped_any_time_no_prior.rds")[["data"]] %>% 
  mutate(type = "Matched\n(No Prior Turnout)")

full <- bind_rows(raw, matched, matched2) %>% 
  filter(first_tr_year == "Overall")

full$type <- factor(full$type, levels = c("Unmatched", "Matched\n(No Prior Turnout)",
                                          "Matched\n(With Prior Turnout)"))

p2 <- ggplot(data = full) + 
  facet_grid(type ~ black) +
  geom_rect(aes(xmin = -.49, xmax = 0.5, ymin = -Inf, ymax = Inf),
            alpha = 0.3, color = "black", fill = "gray") +
  geom_line(data = full, aes(x = period, y = to, linetype = treated)) +
  geom_point(data = full, aes(x = period, y = to, shape = treated)) +
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
  theme_bc(base_family = "Latin Modern Roman") +
  scale_y_continuous(labels = percent) +
  labs(x = "t", y = "Turnout",
       linetype = "Treatment Group",
       shape = "Treatment Group",
       caption = "Treatment occurs inside of shaded band.
Full regression tables in section 3 of SI.", ) +
  coord_cartesian(ylim = c(0.0, 0.6))
p2

saveRDS(p2, "temp/ts.rds")
