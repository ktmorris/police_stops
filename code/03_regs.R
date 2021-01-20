

cog <- readRDS("temp/cog_cities.rds") %>% 
  mutate(state = as.factor(state))

to <- readRDS("temp/city_to.rds") %>% 
  group_by(plasub) %>%
  mutate(votes_18 = count * to_18,
         votes_16 = count * to_16,
         votes_10 = count * to_10) %>% 
  summarize(to_18 = sum(votes_18) / sum(count),
            to_16 = sum(votes_16) / sum(count),
            votes_18 = sum(votes_18),
            votes_16 = sum(votes_16),
            votes_10 = sum(votes_10))

full_set <- inner_join(cog, to, by = c("GEOID" = "plasub")) %>% 
  mutate(vap_to_18 = votes_18 / cvap,
         vap_to_10 = votes_10 / cvap)

p1 <- ggplot(full_set, aes(x = lndper, y = vap_to_18)) + geom_point(shape = 1) + geom_smooth(method = "lm") +
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
m1 <- lm.cluster(to_18 ~ lndper + nh_white + nh_black + latino +
           median_income + some_college + median_age + share_over_64 +
           state, data = full_set, cluster = full_set$state)

m2 <- lm(vap_to_18 ~ lndper + nh_white + nh_black + latino +
           median_income + some_college + median_age + share_over_64 +
           state, data = filter(full_set, vap_to_18 <= 1))

m3 <- lm(vap_to_18 ~ lndper + nh_white + nh_black + latino +
           median_income + some_college + median_age + share_over_64 +
           state, data = filter(full_set, vap_to_18 <= 1))

stargazer(m2, m3, type = "text", omit = c("state"))


marg <- ggeffect(m2, "lndper [all]", vcov.fun = "vcovCR", vcov.type = "CR0", 
                 vcov.args = list(cluster = full_set$state))


bars <- data.frame(x = c(quantile(full_set$lndper, 0.1),
                         quantile(full_set$lndper, 0.9)),
                   y = c(
                     filter(marg,
                            x == round(quantile(full_set$lndper, 0.1), digits = 3)) %>% 
                       select(predicted) %>% 
                       pull(),
                     filter(marg,
                            x == round(quantile(full_set$lndper, 0.9), digits = 3)) %>% 
                       select(predicted) %>% 
                       pull()
                   )) %>% 
  mutate(y = percent(y, accuracy = 0.1))

p2 <- ggplot() + 
  geom_histogram(aes(x = lndper, y = ..count../3000), position="identity", linetype=1,
                 fill="gray60", data = full_set, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg, color = "black") +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "black", alpha=0.25, data = marg) +
  xlab("Dollars per Resident") +
  ylab("Turnout in 2018") + scale_x_continuous(breaks = c(log(1), log(2), log(11), log(101), log(201)),
                                               labels = c("$0", "$1", "$10", "$100", "$200")) +
  scale_y_continuous(labels = percent) +
  coord_cartesian(xlim = c(-.3, log(201)), ylim = c(0, .825)) +
  labs(caption = "Notes: Distribution of Dollars per Resident shown at bottom. Robust standard errors clustered by state.
Covariates: % White, % Black, % Latinx, Median Income, % with Some College, Median Age,
% Over 64 Years Old, State Fixed Effects.") +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10")) +
  geom_vline(xintercept = bars$x[1], color = "black", linetype = "dashed") +
  geom_vline(xintercept = bars$x[2], color = "black", linetype = "dashed") +
  geom_text(aes(bars$x[1], .6, label = paste0(" Predicted Turnout at
 10th Percentile:
 ", bars$y[1]),
                family = "LM Roman 10", hjust = 0)) +
  geom_text(aes(bars$x[2], .6, label = paste0(" Predicted Turnout at
 90th Percentile:
 ", bars$y[2]),
                family = "LM Roman 10", hjust = 0))

p2
saveRDS(p1, "temp/simple_scat.rds")
save(p2, bars, file = "temp/mef_to.rdata")
#########################################

gap <- readRDS("temp/city_to.rds")

gap1 <- gap %>% 
  ungroup() %>% 
  filter(EthnicGroups_EthnicGroup1Desc == "European") %>% 
  mutate(votes_18_white = count * to_18) %>% 
  select(plasub, to_18_white = to_18, to_16_white = to_16, votes_18_white)

gap2 <- gap %>% 
  filter(EthnicGroups_EthnicGroup1Desc != "European") %>% 
  mutate(votes_18 = count * to_18,
         votes_16 = count * to_16) %>% 
  group_by(plasub) %>% 
  summarize(to_18_nonwhite = sum(votes_18) / sum(count),
            to_16_nonwhite = sum(votes_16) / sum(count),
            votes_18_nonwhite = sum(votes_18),
            votes_16_nonwhite = sum(votes_16))

gap3 <- gap %>% 
  ungroup() %>% 
  filter(EthnicGroups_EthnicGroup1Desc == "Likely African-American") %>% 
  mutate(votes_18_black = count * to_18) %>% 
  select(plasub, to_18_black = to_18, to_16_black = to_16, votes_18_black)

gap4 <- gap %>% 
  ungroup() %>% 
  filter(EthnicGroups_EthnicGroup1Desc == "Hispanic and Portuguese") %>% 
  mutate(votes_18_latino = count * to_18) %>% 
  select(plasub, to_18_latino = to_18, to_16_latino = to_16, votes_18_latino)

gap <- full_join(gap1, full_join(gap2, full_join(gap3, gap4))) %>% 
  mutate(gap_18 = to_18_white - to_18_nonwhite,
         gap_16 = to_16_white - to_16_nonwhite,
         gap_18_bw = to_18_white - to_18_black)


gap_set <- inner_join(filter(cog), gap, by = c("GEOID" = "plasub")) %>% 
  mutate(to_white_cvap = votes_18_white / white_cvap,
         to_nonwhite_cvap = votes_18_nonwhite / nonwhite_cvap,
         to_black_cvap = votes_18_black / black_cvap,
         to_latino_cvap = votes_18_latino / latino_cvap,
         vap_gap_18 = to_white_cvap - to_nonwhite_cvap,
         vap_gap_18_bw = to_white_cvap - to_black_cvap,
         vap_gap_18_lw = to_white_cvap - to_latino_cvap)

p3 <- ggplot(filter(gap_set), aes(x = lndper, y = vap_gap_18)) + geom_point(shape = 1) +
  xlab("Dollars per Resident") +
  ylab("Turnout Gap (white - nonwhite) in 2018") +
  scale_x_continuous(breaks = c(log(1), log(2), log(11), log(101), log(201)),
                     labels = c("$0", "$1", "$10", "$100", "$200")) +
  scale_y_continuous(labels = percent) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10")) +
  coord_cartesian(ylim = c(-1, 1),
                  xlim = c(-.3, log(201))) +
    labs(caption = "
       
       ")
p3
m1 <- lm(vap_gap_18 ~ lndper + nh_white + nh_black + latino + state + 
           median_income + some_college + median_age + share_over_64, data = filter(gap_set, !is.infinite(vap_gap_18)))

m2 <- lm(gap_18 ~ lndper + nh_white + nh_black + latino + state + 
           median_income + some_college + median_age + share_over_64, data = filter(gap_set, vap_gap_18 < 1))

stargazer(m1, m2, type = "text", omit = "state")

marg <- ggeffect(m1, c("lndper [all]"), vcov.fun = "vcovCR", vcov.type = "CR0", 
                 vcov.args = list(cluster = gap_set$state)) %>% 
  mutate(group = percent(as.numeric(paste(group))))

bars2 <- data.frame(x = c(quantile(gap_set$lndper, 0.1),
                         quantile(gap_set$lndper, 0.9)))

p4 <- ggplot() + 
  geom_histogram(aes(x = lndper, y = ..count../15000), position="identity", linetype=1,
                 fill="gray60", data = gap_set, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted, color = group), data = marg) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha=0.25, data = marg) +
  xlab("Dollars per Resident") +
  ylab("Turnout Gap (white - nonwhite) in 2018") +
  scale_x_continuous(breaks = c(log(1), log(2), log(11), log(101), log(201)),
                     labels = c("$0", "$1", "$10", "$100", "$200")) +
  scale_y_continuous(labels = percent) +
  labs(caption = "Notes: Distribution of Dollars per Resident shown at bottom. Robust standard errors clustered by state.
Covariates: % White, % Black, % Latinx, Median Income, % with Some College, Median Age,
% Over 64 Years Old, State Fixed Effects.",
       color = "Non-Hispanic White\nShare of Municipality",
       fill = "Non-Hispanic White\nShare of Municipality") +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10")) +
  coord_cartesian(ylim = c(-.1, 1),
                  xlim = c(-.3, log(201))) +
  geom_vline(xintercept = bars2$x[1], color = "black", linetype = "dashed") +
  geom_vline(xintercept = bars2$x[2], color = "black", linetype = "dashed") +
  geom_text(aes(bars2$x[1], .31, label = " 10th Percentile",
                family = "LM Roman 10", hjust = 0)) +
  geom_text(aes(bars2$x[2], .31, label = " 90th Percentile",
                family = "LM Roman 10", hjust = 0))
  
p4
saveRDS(p3, "temp/simple_scat_gap.rds")
save(p4, bars2, file = "temp/mef_gap.rdata")

##############################################

p5 <- ggplot(filter(gap_set, black_cvap > 0), aes(x = lndper, y = vap_gap_18_bw)) + geom_point(shape = 1) +
  xlab("Dollars per Resident") + geom_smooth(method = "lm") +
  ylab("Turnout Gap (white - Black) in 2018") +
  scale_x_continuous(breaks = c(log(1), log(2), log(11), log(101), log(201)),
                     labels = c("$0", "$1", "$10", "$100", "$200")) +
  scale_y_continuous(labels = percent) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10")) +
  coord_cartesian(ylim = c(-.25, .75),
                  xlim = c(-.3, log(201))) +
  labs(caption = "
       
       ")
p5


m1 <- lm(vap_gap_18_bw ~ lndper + nh_white + nh_black + latino + state + 
           median_income + some_college + median_age + share_over_64,
         data = filter(gap_set, !is.infinite(vap_gap_18_bw)))

stargazer(m1, type = "text", omit = "state")

marg <- ggeffect(m1, c("lndper [all]"), vcov.fun = "vcovCR", vcov.type = "CR0", 
                 vcov.args = list(cluster = gap_set$state)) %>% 
  mutate(group = percent(as.numeric(paste(group))))

bars3 <- data.frame(x = c(quantile(filter(gap_set, black_cvap > 0)$lndper, 0.1),
                         quantile(filter(gap_set, black_cvap > 0)$lndper, 0.9)),
                   y = c(
                     "28.5%",
                     "19.6%"))

p6 <- ggplot() + 
  geom_histogram(aes(x = lndper, y = ..count../3000), position="identity", linetype=1,
                 fill="gray60", data = filter(gap_set, black_cvap > 0), alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg, color = "black") +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "black", alpha=0.25, data = marg) +
  xlab("Dollars per Resident") +
  ylab("Turnout Gap (white - Black) in 2018") + scale_x_continuous(breaks = c(log(1), log(2), log(11), log(101), log(201)),
                                               labels = c("$0", "$1", "$10", "$100", "$200")) +
  scale_y_continuous(labels = percent) +
  coord_cartesian(xlim = c(-.3, log(201)), ylim = c(-.25, .75)) +
  labs(caption = "Notes: Distribution of Dollars per Resident shown at bottom. Robust standard errors clustered by state.
Covariates: % White, % Black, % Latinx, Median Income, % with Some College, Median Age,
% Over 64 Years Old, State Fixed Effects.") +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10")) +
  geom_vline(xintercept = bars3$x[1], color = "black", linetype = "dashed") +
  geom_vline(xintercept = bars3$x[2], color = "black", linetype = "dashed") +
  geom_text(aes(bars3$x[1], .4, label = paste0(" Predicted Turnout Gap
 at 10th Percentile:
 ", bars3$y[1]),
                family = "LM Roman 10", hjust = 0)) +
  geom_text(aes(bars3$x[2], .32, label = paste0(" Predicted Turnout Gap
 at 90th Percentile:
 ", bars3$y[2]),
                family = "LM Roman 10", hjust = 0))

p6

saveRDS(p5, "temp/simple_scat_gap_bw.rds")
save(p6, bars3, file = "temp/mef_gap_bw.rdata")
