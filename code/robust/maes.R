
test_set <- bind_rows(
  readRDS("temp/ga_real_model_city.rds"),
  readRDS("temp/nc_real_model_city.rds"),
  readRDS("temp/fl_real_model_city.rds")
)

ggplot(test_set, aes(x = share_black_vf, y = share_black_l2)) + geom_point() +
  theme_bc(base_family = "LM Roman 10") +
  scale_y_continuous(name = "Predicted Share Black",
                     labels = percent) +
  scale_x_continuous(name = "Actual Share Black",
                     labels = percent) +
  geom_abline(slope = 1)


cog <- readRDS("temp/cog_cities.rds") %>% 
  mutate(state = as.factor(state),
         fines_rev = dper / total_revenue)

test_set <- left_join(test_set, cog, by = c("plasub" = "GEOID"))

#####################

m1 <- mae(filter(test_set,
                 population <= quantile(test_set$population, 0.25))$share_black_vf,
          filter(test_set,
                 population <= quantile(test_set$population, 0.25))$share_black_l2)

m2 <- mae(filter(test_set,
                 population > quantile(test_set$population, 0.25),
                 population <= quantile(test_set$population, 0.5))$share_black_vf,
          filter(test_set,
                 population > quantile(test_set$population, 0.25),
                 population <= quantile(test_set$population, 0.5))$share_black_l2)

m3 <- mae(filter(test_set,
                 population > quantile(test_set$population, 0.5),
                 population <= quantile(test_set$population, 0.75))$share_black_vf,
          filter(test_set,
                 population > quantile(test_set$population, 0.5),
                 population <= quantile(test_set$population, 0.75))$share_black_l2)

m4 <- mae(filter(test_set,
                 population > quantile(test_set$population, 0.75))$share_black_vf,
          filter(test_set,
                 population > quantile(test_set$population, 0.75))$share_black_l2)

m5 <- mae(test_set$share_black_vf, test_set$share_black_l2)


dat <- data.table("Bottom Quartile" = m1,
                  "Second Quartile" = m2,
                  "Third Quartile" = m3,
                  "Fourth Quartile" = m4,
                  "Overall" = m5) %>% 
  mutate_all(percent, accuracy = .02)

saveRDS(dat, "temp/maes.rds")

