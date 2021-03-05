window <- 90



hist <- readRDS("raw_data/fl_l2_hills/fl_l2_history_hills.rds") %>% 
  select(LALVOTERID, v18 = General_2018_11_06) %>% 
  mutate(v18 = ifelse(v18 == "Y", 1, 0))

match_data <- readRDS("temp/hills_pre_match.rds")

match_data <- left_join(match_data, hist)

m14 <- match_data %>% 
  filter(fd >= as.Date("2014-11-04") - window,
         fd <= as.Date("2014-11-04") + window) %>% 
  mutate(treated = fd <= as.Date("2014-11-04")) %>% 
  rename(v_t1 = v10,
         v_t2 = v12,
         to = v14)

m16 <- match_data %>% 
  filter(fd >= as.Date("2016-11-08") - window,
         fd <= as.Date("2016-11-08") + window) %>% 
  mutate(treated = fd <= as.Date("2016-11-08")) %>% 
  rename(v_t1 = v12,
         v_t2 = v14,
         to = v16)

m18 <- match_data %>% 
  filter(fd >= as.Date("2018-11-06") - window,
         fd <= as.Date("2018-11-06") + window) %>% 
  mutate(treated = fd <= as.Date("2018-11-06")) %>% 
  rename(v_t1 = v14,
         v_t2 = v16,
         to = v18)


tot <- bind_rows(m14, m16, m18)

m1 <- to ~ treated
m1b <- to ~ treated + factor(floor(year(fd) / 2))
m2 <- to ~ treated + black + max_amount + white + black + latino + asian + male + dem +
  rep + age + reg_date + median_income + some_college + unem + v_t1 + v_t2
m2b <- to ~ treated + black + max_amount + white + black + latino + asian + male + dem +
  rep + age + reg_date + median_income + some_college + unem + v_t1 + v_t2 +
  factor(floor(year(fd) / 2))

models <- list(
  lm(m1 , data = m14),
  lm(m1 , data = m16),
  lm(m1 , data = m18),
  lm(m1b, data = tot)
)

models2 <- list(
  lm(m2 , data = m14),
  lm(m2 , data = m16),
  lm(m2 , data = m18),
  lm(m2b, data = tot)
)

cints <- bind_rows(
  as.data.frame(confint(models[[1]])),
  as.data.frame(confint(models[[2]])),
  as.data.frame(confint(models[[3]])),
  as.data.frame(confint(models[[4]]))
)  %>% 
  rownames_to_column() %>% 
  select(lb = `2.5 %`, up = `97.5 %`, var = rowname) %>% 
  filter(grepl("treatedTRUE", var)) %>% 
  mutate(estimate = (lb + up) / 2,
         model = "No Covariates")

cints$year <- c("2014", "2016", "2018", "Overall")

cints2 <- bind_rows(
  as.data.frame(confint(models2[[1]])),
  as.data.frame(confint(models2[[2]])),
  as.data.frame(confint(models2[[3]])),
  as.data.frame(confint(models2[[4]]))
)  %>% 
  rownames_to_column() %>% 
  select(lb = `2.5 %`, up = `97.5 %`, var = rowname) %>% 
  filter(grepl("treatedTRUE", var)) %>% 
  mutate(estimate = (lb + up) / 2,
         model = "Plus Covariates")
cints2$year <- c("2014", "2016", "2018", "Overall")


cints <- bind_rows(cints, cints2)


ci_level = 0.95
legend.title = "Model"
facet.label.pos = "top"
n_models <- length(unique(cints$model))
oshapes <- c(21:25, 15:18, 3, 4, 8)
shapes <- oshapes[seq_len(n_models)]
colors <- get_colors("CUD Bright", n_models)
################

p <- ggplot(data = filter(cints)) + 
  ggstance::geom_pointrangeh(aes(y = year, x = estimate, 
                                 xmin = lb, xmax = up, colour = model, 
                                 shape = model), position = ggstance::position_dodgev(height = -.5), 
                             fill = "white", fatten = 3, size = 0.8, show.legend = T) +
  geom_vline(xintercept = 0, linetype = 2, 
             size = 0.25) + scale_colour_manual(values = colors, name = legend.title) + 
  scale_shape_manual(values = shapes, name = legend.title) + theme_nice(legend.pos = "right") + 
  drop_y_gridlines() + 
  scale_x_continuous(labels = percent) +
  labs(x = "Estimate", y = NULL) +
  ggtitle("Turnout Effect of Being Ticketed") +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 10),
        panel.grid.major.x = element_line(linetype = "solid")) + 
  theme_bc(base_family = "LM Roman 10")

p
