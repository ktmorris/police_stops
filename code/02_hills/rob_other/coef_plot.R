
matches <- readRDS("temp/full_reg_data_no_matching.rds")
m1 <- to ~ treated * post + as.factor(year)
m2 <- to ~ treated * post + as.factor(year) +
  white + black + latino + asian + male +
  dem + rep + age + reg_date + pre_stops + v1 + v2 + v3 +
  median_income + some_college + unem + civil + paid + tampa_pd

for(gg in c("overall", "2014-11-04", "2016-11-08", "2018-11-06")){
  
  if(gg == "overall"){
    dat1 <- filter(matches, period <= 0.5)
  }else{
    dat1 <- dplyr::filter(matches, period <= 0.5, first_tr_year == gg)
  }
  
  c1 <- confint(
    feols(m1, dat1, weights = ~ weight, cluster = ~ group)
  )
  c1$term <- rownames(c1)
  c1 <- c1 %>% 
    mutate(model = 1,
           year = gg,
           t = "Overall")
  
  c2 <- confint(
    feols(m2, dat1, weights = ~ weight, cluster = ~ group)
  )
  c2$term <- rownames(c2)
  c2 <- c2 %>% 
    mutate(model = 2,
           year = gg,
           t = "Overall")
  ######
  c3 <- confint(
    feols(m1, filter(dat1, !black), weights = ~ weight, cluster = ~ group)
  )
  c3$term <- rownames(c3)
  c3 <- c3 %>% 
    mutate(model = 3,
           year = gg,
           t = "Non-Black")
  
  c4 <- confint(
    feols(m1, filter(dat1, black), weights = ~ weight, cluster = ~ group)
  )
  c4$term <- rownames(c4)
  c4 <- c4 %>% 
    mutate(model = 3,
           year = gg,
           t = "Black")
  ######
  c5 <- confint(
    feols(m2, filter(dat1, !black), weights = ~ weight, cluster = ~ group)
  )
  c5$term <- rownames(c5)
  c5 <- c5 %>% 
    mutate(model = 4,
           year = gg,
           t = "Non-Black")
  
  c6 <- confint(
    feols(m2, filter(dat1, black), weights = ~ weight, cluster = ~ group)
  )
  c6$term <- rownames(c6)
  c6 <- c6 %>% 
    mutate(model = 4,
           year = gg,
           t = "Black")
  
  c <- bind_rows(c1, c2, c3, c4, c5, c6)
  saveRDS(c, paste0("temp/cints_full_", gg, "_no_matching.rds"))
}

cleanup()

cints <- rbindlist(lapply(c("overall", "2014-11-04", "2016-11-08", "2018-11-06"), function(gg){
  readRDS(paste0("temp/cints_full_", gg, "_no_matching.rds"))
})) %>% 
  filter(term == "treatedTRUE:postTRUE") %>% 
  mutate(model = ifelse(t == "Overall", model, model - 2))

colnames(cints) <- c("lb", "ub", "term", "model", "year", "t")

cints <- mutate(cints, estimate = (lb + ub) / 2,
                model = ifelse(model == 1, "No Controls", "With Controls"),
                year = ifelse(year == "overall", "Overall",
                              paste("t = 0\nin", substring(year, 1, 4))))

cints$year <- factor(cints$year, levels = c("t = 0\nin 2014",
                                            "t = 0\nin 2016",
                                            "t = 0\nin 2018",
                                            "Overall"))

p <- ggplot(data = cints, aes(y = t, x = estimate, xmin = lb, 
                              xmax = ub, linetype = model)) +
  ggstance::geom_pointrangeh(aes(y = t, x = estimate, 
                                 xmin = lb, xmax = ub,
                                 shape = model), position = ggstance::position_dodgev(height = 0.5), 
                             fill = "white", show.legend = T) +
  facet_grid(year ~.) +
  theme_bc(base_family = "LM Roman 10", legend.position = "bottom") +
  labs(shape = "Model",
       linetype = "Model",
       x = "Estimate",
       y = NULL) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_x_continuous(labels = scales::percent)
p
saveRDS(p, "temp/coef_plot_no_matching.rds")

#################################################################
cleanup()


matches <- readRDS("temp/full_reg_data_no_matching.rds")
m1 <- to ~ treated * post + as.factor(year)
m2 <- to ~ treated * post + as.factor(year) +
  white + black + latino + asian + male +
  dem + rep + age + reg_date + pre_stops + v1 + v2 + v3 +
  median_income + some_college + unem + civil + paid + tampa_pd

es <- rbindlist(lapply(seq(-1.5, 0.5, 1), function(y){
  dat1 <- filter(matches, period <= y)
  dat1$post <- dat1$period == y
  
  confint(feols(m1, dat1, weights = ~ weight, cluster = ~ group)) %>% 
    mutate(model = 1,
           year = y)
  
  c <- confint(feols(m1, dat1, weights = ~ weight, cluster = ~ group)) %>% 
    mutate(model = 1,
           year = y)
  c <- c[rownames(c) == "treatedTRUE:postTRUE",]
  
  c2 <- confint(feols(m2, dat1, weights = ~ weight, cluster = ~ group)) %>% 
    mutate(model = 2,
           year = y)
  c2 <- c2[rownames(c2) == "treatedTRUE:postTRUE",]
  
  return(bind_rows(
    c,
    c2
  ))
})) %>% 
  rename(lower = `2.5 %`,
         upper = `97.5 %`) %>% 
  mutate(estimate = (upper + lower) / 2,
         model = ifelse(model == 1, "Without Covariates", "With Covariates"))

p <- ggplot(data = filter(es), aes(y = year, x = estimate, xmin = lower, 
                                   xmax = upper, shape = model,
                                   linetype = model)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggstance::geom_pointrangeh(aes(y = year, x = estimate, 
                                 xmin = lower, xmax = upper, shape = model,
                                 linetype = model),
                             position = ggstance::position_dodgev(height = .5), 
                             fill = "white", fatten = 3, size = 0.8, show.legend = T) +
  coord_flip() + theme_bc(base_family = "LM Roman 10") +
  theme(legend.position = "bottom", text = element_text(family = "LM Roman 10")) +
  labs(y = "t = 0", x = "Estimate", shape = "Model",
       linetype = "Model") + scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = c(-2, -1, 0), breaks = c(-1.5, -.5, .5))
p
saveRDS(p, "temp/event_study_no_matching.rds")
