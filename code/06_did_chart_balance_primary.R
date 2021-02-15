lb <- 0.7
ub <- 0.9

cities <- readRDS("temp/cog_cities.rds") %>%
  mutate(pct_change = dper / dper_12) %>%
  filter(!is.na(pct_change))

cities <- cities %>%
  filter(pct_change <= quantile(cities$pct_change, lb) |
           pct_change >= quantile(cities$pct_change, ub)) %>%
  mutate(treated = pct_change >= quantile(cities$pct_change, ub)) %>%
  dplyr::select(place_id, treated, GEOID)

pot_con <- filter(cities, !treated)

census <- readRDS("temp/census_12.rds") %>%
  select(population, asian, latino, nh_black, nh_white,
         median_income, some_college, median_age,
         share_no_car, GEOID)

cities <- left_join(cities, census)

cities <- cities[complete.cases(cities), ]

match_data <- cities %>%
  dplyr::select(-place_id, -treated, -GEOID)

genout <- readRDS(paste0("temp/genout_pct_", lb, "_", ub, ".rds"))

mout <- Match(Tr = cities$treated, X = match_data,
              estimand = "ATT", Weight.matrix = genout, M = 2)

ids <- data.frame("id" = c(mout$index.treated, mout$index.control),
                  "group" = rep(mout$index.treated, 2),
                  "weight" = rep(mout$weights, 2)) %>% 
  group_by(id, group) %>% 
  summarize(weight = sum(weight))

cities <- left_join(
  cities %>% 
    mutate(id = row_number()),
  ids
) %>% 
  mutate(weight = ifelse(is.na(weight), 0, weight))
##################################
# 
# varnames <- c("population", "asian", "latino", "nh_black", "nh_white",
#               "median_income", "some_college", "median_age",
#               "share_no_car")
# 
# balance <- MatchBalance(treated ~ population + asian + latino + nh_black + nh_white +
#                           median_income + some_college + median_age +
#                           share_no_car, data = cities, match.out = mout)
# TrMean <- c()
# PreMean <- c()
# PreQQmed <- c()
# PreQQmean <- c()
# PreQQmax <- c()
# PostMean <- c()
# PostQQmed <- c()
# PostQQmean <- c()
# PostQQmax <- c()
# 
# for(i in c(1:length(balance$BeforeMatching))){
#   TrMean <- unlist(c(TrMean, balance$BeforeMatching[[i]][3][1]))
#   PreMean <- unlist(c(PreMean, balance$BeforeMatching[[i]][4][1]))
#   PreQQmed <- unlist(c(PreQQmed, balance$BeforeMatching[[i]]$qqsummary[2]))
#   PreQQmean <- unlist(c(PreQQmean, balance$BeforeMatching[[i]]$qqsummary[1]))
#   PreQQmax <- unlist(c(PreQQmax, balance$BeforeMatching[[i]]$qqsummary[3]))
#   
#   PostMean <- unlist(c(PostMean, balance$AfterMatching[[i]][4][1]))
#   PostQQmed <- unlist(c(PostQQmed, balance$AfterMatching[[i]]$qqsummary[2]))
#   PostQQmean <- unlist(c(PostQQmean, balance$AfterMatching[[i]]$qqsummary[1]))
#   PostQQmax <- unlist(c(PostQQmax, balance$AfterMatching[[i]]$qqsummary[3]))
# }
# 
# 
# 
# df <- data.frame("TrMean" = TrMean,
#                  "TrMean2" = TrMean,
#                  "PreMean" = PreMean,
#                  "PreQQmed" = PreQQmed,
#                  "PreQQmean" = PreQQmean,
#                  "PreQQmax" = PreQQmax,
#                  "PostMean" = PostMean,
#                  "PostQQmed" = PostQQmed,
#                  "PostQQmean" = PostQQmean,
#                  "PostQQmax" = PostQQmax,
#                  "names" = varnames) %>%
#   mutate(change_mean = 1 - (abs(TrMean - PostMean) / abs(TrMean - PreMean)),
#          change_eqqmed = 1 - abs(PostQQmed / PreQQmed),
#          change_eqqmean = 1 - abs(PostQQmean / PreQQmean),
#          change_eqqmax = 1 - abs(PostQQmax / PreQQmax)) %>%
#   mutate_at(vars(TrMean, PreMean, TrMean2, PostMean), ~ comma(round(., 3), accuracy = .001)) %>%
#   mutate_at(vars(change_mean, change_eqqmed, change_eqqmean, change_eqqmax), ~ round(. * 100, 2)) %>%
#   filter(names != "voted_primary")
# 
# df <- full_join(df,
#                 fread("./raw_data/var_names.csv"),
#                 by = c("names" = "variable")) %>%
#   select(name, TrMean, PreMean, TrMean2, PostMean, change_mean, change_eqqmed, change_eqqmean, change_eqqmax) %>%
#   filter(!is.na(TrMean))
# 
# 
# df <- df %>%
#   mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
#             ~ ifelse(name == "Median Income", dollar(round(as.numeric(gsub(",", "", .)))), .)) %>%
#   mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
#             ~ ifelse(name %in% c("Population"), comma(round(as.numeric(gsub(",", "", .))), 1), .)) %>%
#   mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
#             ~ ifelse(name %in% c("Median Age"), round(as.numeric(gsub(",", "", .)), digits = 1), .)) %>%
#   mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
#             ~ ifelse(substring(name, 1, 1) == "%", percent(as.numeric(.), accuracy = .1), .)) %>%
#   filter(!is.na(name))
# 
# colnames(df) <- c("", "Treated", "Control", "Treated", "Control", "Mean Diff", "eQQ Med", "eQQ Mean", "eQQ Max")

##########################
dper <- readRDS("temp/cog_cities.rds") %>%
  select(GEOID, dper, dper_12) %>%
  pivot_longer(cols = c("dper", "dper_12"), names_to = "year") %>%
  mutate(year = ifelse(year == "dper", "2017", "2012"))

change <- left_join(cities, dper) %>%
  group_by(treated, year) %>%
  summarize(dper = weighted.mean(value, weight))

ggplot(change, aes(x = year, y = dper, fill = treated), xlab="Age Group") +
  geom_bar(stat="identity", width=.5, position = "dodge")

###############################
to <- readRDS("temp/city_to_early_reg.rds") %>% 
  group_by(plasub) %>%
  summarize_at(vars(to_18, to_16, to_14, to_12, to_10),
               ~ weighted.mean(., count)) %>% 
  pivot_longer(starts_with("to_"), names_to = "year") %>% 
  mutate(year = as.integer(gsub("to_", "20", year))) %>% 
  rename(to = value)

cities2 <- left_join(cities, to, by = c("GEOID" = "plasub")) %>% 
  filter(weight != 0)

pot_con <- left_join(pot_con, to, by = c("GEOID" = "plasub")) %>%
  group_by(year) %>%
  summarize(to = mean(to)) %>%
  mutate(treated = paste0("All Bottom ", (lb * 100), "tile"))

ll <- cities2 %>%
  group_by(treated, year) %>%
  summarize(to = weighted.mean(to, weight)) %>%
  mutate(treated = ifelse(treated, paste0("Top ", (1 - ub) * 100, "tile"),
                          paste0("Controls from Bottom ", (lb * 100), "tile")))

ll <- bind_rows(ll, pot_con)

ll$treated <- factor(ll$treated, levels = c(paste0("Top ", (1-ub) * 100, "tile"),
                                            paste0("Controls from Bottom ", (lb * 100), "tile"),
                                            paste0("All Bottom ", (lb * 100), "tile")))

ggplot(filter(ll, !grepl("All", treated)), aes(x = year, y = to, color = treated)) + geom_line() +
  theme_bc() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Turnout", color = "Group") +
  geom_vline(xintercept = 2012, linetype = "dashed") +
  geom_vline(xintercept = 2017, linetype = "dashed") +
  labs(caption = "Notes: End years of Census periods in dashed lines.")

ggsave("temp/did_plot.png")
