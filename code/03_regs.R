

cog <- readRDS("temp/cog_cities.rds") %>% 
  mutate(state = as.factor(state))

to <- readRDS("temp/city_to.rds") %>% 
  group_by(plasub) %>% 
  summarize(to_18 = weighted.mean(to, count),
            to_16 = weighted.mean(to_old, count))

full_set <- inner_join(cog, to, by = c("GEOID" = "plasub"))

p1 <- ggplot(full_set, aes(x = lndper, y = to_18)) + geom_point(shape = 1) + geom_smooth(method = "lm") +
  xlab("Dollars per Resident") +
  ylab("Turnout in 2018") + scale_x_continuous(breaks = c(log(2), log(11), log(101), log(1001)),
                                                                           labels = c("$1", "$10", "$100", "$1,000")) +
  scale_y_continuous(labels = percent) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10")) +
  coord_cartesian(xlim = c(-.3, log(1001))) +
  labs(caption = "
       
       ")

m1 <- lm.cluster(to_18 ~ lndper + nh_white + nh_black + latino +
           median_income + some_college + median_age + share_over_64 +
           state, data = full_set, cluster = full_set$state)

m2 <- lm(to_18 ~ lndper + nh_white +  + latino +
           median_income + some_college + median_age + share_over_64 +
           state, full_set)

stargazer(m1, m2, type = "text", omit = c("state"))


marg <- ggeffect(m2, "lndper [all]", vcov.fun = "vcovCR", vcov.type = "CR0", 
                 vcov.args = list(cluster = full_set$state))


p2 <- ggplot() + 
  geom_histogram(aes(x = lndper, y = ..count../5000), position="identity", linetype=1,
                 fill="gray60", data = full_set, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg, color = "black") +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "black", alpha=0.25, data = marg) +
  xlab("Dollars per Resident") +
  ylab("Turnout in 2018") + scale_x_continuous(breaks = c(log(2), log(11), log(101), log(1001)),
                                               labels = c("$1", "$10", "$100", "$1,000")) +
  scale_y_continuous(labels = percent) +
  coord_cartesian(xlim = c(-.3, log(1001)), ylim = c(0, 1)) +
  labs(caption = "Notes: Distribution of Dollars per Resident shown at bottom. Robust standard errors clustered by state.
Covariates: % White, % Black, % Latinx, Median Income, % with Some College, Median Age,
% Over 64 Years Old, State Fixed Effects.") +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10")) +
  geom_vline(xintercept = quantile(full_set$lndper, 0.1), color = "red") +
  geom_vline(xintercept = quantile(full_set$lndper, 0.9), color = "red") +
  geom_text(aes(quantile(full_set$lndper, 0.1), .75, label = " Predicted Turnout at
 10th Percentile:
 62.2%",
                family = "LM Roman 10", hjust = 0)) +
  geom_text(aes(quantile(full_set$lndper, 0.9), .75, label = " Predicted Turnout at
 90th Percentile:
 60.6%",
                family = "LM Roman 10", hjust = 0))


saveRDS(p1, "temp/simple_scat.rds")
save(p2, full_set, file = "temp/mef_to.rdata")
#########################################

gap <- readRDS("temp/city_to.rds")

gap1 <- gap %>% 
  ungroup() %>% 
  filter(EthnicGroups_EthnicGroup1Desc == "European") %>% 
  select(plasub, to_18_white = to, to_16_white = to_old)

gap2 <- gap %>% 
  filter(EthnicGroups_EthnicGroup1Desc != "European") %>% 
  group_by(plasub) %>% 
  summarize(to_18_nonwhite = weighted.mean(to, count, na.rm = T),
            to_16_nonwhite = weighted.mean(to_old, count, na.rm = T))

gap <- inner_join(gap1, gap2) %>% 
  mutate(gap_18 = to_18_white - to_18_nonwhite,
         gap_16 = to_16_white - to_16_nonwhite)


gap_set <- inner_join(filter(cog, nh_white < 0.9), gap, by = c("GEOID" = "plasub"))

p3 <- ggplot(gap_set, aes(x = lndper, y = gap_18)) + geom_point(shape = 1) +
  xlab("Dollars per Resident") +
  ylab("Turnout Gap (white versus nonwhite) in 2018") + scale_x_continuous(breaks = c(log(2), log(11), log(101), log(1001)),
                                               labels = c("$1", "$10", "$100", "$1,000")) +
  scale_y_continuous(labels = percent) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10")) +
  coord_cartesian(ylim = c(-.2, .4),
                  xlim = c(-.3, log(1001))) +
    labs(caption = "
       
       ")
p3
m1 <- lm(gap_18 ~ lndper*nh_white + nh_black + latino + state + 
           median_income + some_college + median_age + share_over_64, data = gap_set)

marg <- ggeffect(m1, c("lndper [all]", "nh_white [0.25, 0.5, 0.75]"), vcov.fun = "vcovCR", vcov.type = "CR0", 
                 vcov.args = list(cluster = gap_set$state)) %>% 
  mutate(group = percent(as.numeric(paste(group))))


p4 <- ggplot() + 
  geom_histogram(aes(x = lndper, y = ..count../25000), position="identity", linetype=1,
                 fill="gray60", data = full_set, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted, color = group), data = marg) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha=0.25, data = marg) +
  xlab("Dollars per Resident") +
  ylab("Turnout Gap (white versus nonwhite) in 2018") + scale_x_continuous(breaks = c(log(2), log(11), log(101), log(1001)),
                                               labels = c("$1", "$10", "$100", "$1,000")) +
  scale_y_continuous(labels = percent) +
  labs(caption = "Notes: Distribution of Dollars per Resident shown at bottom. Robust standard errors clustered by state.
Covariates: % White, % Black, % Latinx, Median Income, % with Some College, Median Age,
% Over 64 Years Old, State Fixed Effects.",
       color = "Non-Hispanic White\nShare of Municipality",
       fill = "Non-Hispanic White\nShare of Municipality") +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10")) +
  coord_cartesian(ylim = c(-.2, .4),
                  xlim = c(-.3, log(1001)))
p4

saveRDS(p3, "temp/simple_scat_gap.rds")
save(p4, file = "temp/mef_gap.rdata")
