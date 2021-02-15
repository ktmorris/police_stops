# floor(runif(1, min = 10000, max = 99999))
set.seed(45251)

library(Matching)
library(data.table)
library(scales)
library(kableExtra)
library(tidyverse)
require(snow)
require(parallel)

cl <- makeCluster(c(readLines(Sys.getenv("MY_HOSTFILE"))), type="SOCK")

to <- readRDS("temp/city_to_early_reg.rds") %>% 
  group_by(plasub) %>%
  summarize_at(vars(to_18, to_16, to_14, to_12, to_10),
               ~ weighted.mean(., count)) %>% 
  pivot_longer(starts_with("to_"), names_to = "year") %>% 
  mutate(year = as.integer(gsub("to_", "20", year))) %>% 
  rename(to = value)

to2 <- readRDS("temp/city_to_early_reg.rds") %>% 
  filter(EthnicGroups_EthnicGroup1Desc %in% c("European",
                                              "Likely African-American")) %>% 
  mutate(race = ifelse(EthnicGroups_EthnicGroup1Desc == "European",
                       "white",
                       "black")) %>% 
  group_by(plasub, race) %>%
  summarize_at(vars(to_18, to_16, to_14, to_12, to_10),
               ~ weighted.mean(., count)) %>% 
  pivot_longer(starts_with("to_"), names_to = "year") %>% 
  mutate(year = as.integer(gsub("to_", "20", year))) %>% 
  rename(to = value) %>% 
  pivot_wider(id_cols = c("plasub", "year"), names_from = "race", values_from = "to", names_prefix = "to_")

to <- left_join(to, to2)

for(lb in c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8)){
  for(ub in c(0.85, 0.9, 0.95)){
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
    
    if(!file.exists(paste0("temp/genout_pct_", lb, "_", ub, ".rds"))){
      genout <- GenMatch(Tr = cities$treated, X = match_data, replace = T, pop.size = 1000, cluster = cl)
      saveRDS(genout, paste0("temp/genout_pct_", lb, "_", ub, ".rds"))
    }else{
      genout <- readRDS(paste0("temp/genout_pct_", lb, "_", ub, ".rds"))
    }
    
    mout <- Match(Tr = cities$treated, X = match_data,
                  estimand = "ATT", Weight.matrix = genout, M = 2)
    ###################################################
    
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
    

    ##############################
    
    cities2 <- left_join(cities, to, by = c("GEOID" = "plasub")) %>% 
      filter(weight != 0)

    cities2$midterm <- cities2$year %% 4 == 2
    # 
    # m1 <- lm(to ~ treated * I(year > 2014) + treated * midterm,
    #          data = cities2, weights = weight)
    # m1ses <- lm.cluster(to ~ treated * I(year > 2014) + treated * midterm,
    #                     data = cities2, weights = cities2$weight, cluster = cities2$group)
    # 
    # stargazer(m1, type = "text",
    #           se = list(summary(m1ses)[,2]),
    #           dep.var.labels = paste0(lb, "-", ub))
    # 
    # save(m1, m1ses, file = paste0("temp/regs_pct_", lb, "_", ub, ".rdata"))
    
    m2 <- lm(to_white ~ treated * I(year > 2014) + treated * midterm,
             data = cities2, weights = weight)
    m2ses <- lm.cluster(to_white ~ treated * I(year > 2014) + treated * midterm,
                        data = cities2, weights = cities2$weight, cluster = cities2$group)
    
    save(m2, m2ses, file = paste0("temp/regs_pct_", lb, "_", ub, "_white.rdata"))
    
    m3 <- lm(to_black ~ treated * I(year > 2014) + treated * midterm,
             data = cities2, weights = weight)
    m3ses <- lm.cluster(to_black ~ treated * I(year > 2014) + treated * midterm,
                        data = cities2, weights = cities2$weight, cluster = cities2$group)
    

    save(m3, m3ses, file = paste0("temp/regs_pct_", lb, "_", ub, "_black.rdata"))
  }
}

###############################################################################################

fs <- list.files("temp", pattern = "^regs_pct_", full.names = T)
fs <- fs[!(grepl("white", fs))]
fs <- fs[!(grepl("black", fs))]

models <- lapply(fs, function(f){
  load(f)
  return(m1)
})

ses <- lapply(fs, function(f){
  load(f)
  return(summary(m1ses)[,2])
})

reg_labels <- gsub("_", "-",
                   gsub("temp/regs_pct_", "",
                        gsub(".rdata", "", fs)))

stargazer(models, type = "text",
          se = ses,
          omit.stat = c("f", "ser"),
          column.labels = c(reg_labels),
          out = "temp/sens_regs.txt")
#########################

cints <- rbindlist(lapply(fs, function(f){
  load(f)
  cint <- as.data.frame(confint(m1ses)) %>% 
    mutate(Model = gsub("_", "-",
                        gsub("temp/regs_pct_", "",
                             gsub(".rdata", "", f)))) %>% 
    rownames_to_column("vars") %>% 
    filter(vars == "treatedTRUE:I(year > 2014)TRUE")
}))

colnames(cints) <- c("vars", "conf.low", "conf.high", "Model")

cints$estimate <- (cints$conf.low + cints$conf.high) / 2


###############################################################################################

fs <- list.files("temp", pattern = "^regs_pct_", full.names = T)
fs <- fs[(grepl("white", fs))]

models <- lapply(fs, function(f){
  load(f)
  return(m2)
})

ses <- lapply(fs, function(f){
  load(f)
  return(summary(m2ses)[,2])
})

reg_labels <- gsub("_", "-",
                   gsub("temp/regs_pct_", "",
                        gsub(".rdata", "", fs)))

stargazer(models, type = "text",
          se = ses,
          omit.stat = c("f", "ser"),
          column.labels = c(reg_labels),
          out = "temp/sens_regs.txt")
#########################

cints <- rbindlist(lapply(fs, function(f){
  load(f)
  cint <- as.data.frame(confint(m2ses)) %>% 
    mutate(Model = gsub("_", "-",
                        gsub("temp/regs_pct_", "",
                             gsub("_white.rdata", "", f)))) %>% 
    rownames_to_column("vars") %>% 
    filter(vars == "treatedTRUE:I(year > 2014)TRUE")
}))

colnames(cints) <- c("vars", "conf.low", "conf.high", "Model")

cints$estimate <- (cints$conf.low + cints$conf.high) / 2



cints <- cSplit(cints, "Model", "-", type.convert = F) %>% 
  rename(treatment_group = Model_2,
         control_group = Model_1) %>% 
  mutate(treatment_group = paste0("Top ", 100 - (as.numeric(treatment_group)*100), "th Percentile"),
         control_group = paste0("Bottom ", (as.numeric(control_group)*100), "th Percentile"))

cints$treatment_group <- factor(cints$treatment_group, levels = c("Top 5th Percentile",
                                                                  "Top 10th Percentile",
                                                                  "Top 15th Percentile"))

lls <- cints %>% 
  group_by(treatment_group) %>% 
  summarize(mean_eff = mean(estimate))

p <- ggplot(data = cints) +
  ggstance::geom_pointrangeh(aes(y = treatment_group, x = estimate, 
                                 xmin = conf.low, xmax = conf.high, colour = control_group),
                             position = ggstance::position_dodgev(height = -.5), 
                             fill = "white", fatten = 3, size = 0.8, show.legend = T)+
  geom_vline(xintercept = 0, linetype = 2, 
             size = 0.25) + 
  theme_bc(legend.pos = "right", base_family = "BentonSans") +
  theme(axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_line(linetype = "solid"),
        text = element_text(family = "BentonSans", face = "bold")) + 
  xlab("Estimate") +
  scale_x_continuous(labels = percent, ) +
  labs(color = "Control Group\nDrawn From...",
       y = "Treatment Group",
       caption = "Note: 95% confidence intervals shown.
Mean effect for treatment groups shown in dashed lines.") +
  theme(plot.caption = element_text(hjust = 0),
        text = element_text(size = 12)) +
  ggtitle("Estimated Treatment Effect (Treated × Year > 2014)") +
  geom_segment(aes(x = lls$mean_eff[1], y = 0.5, xend = lls$mean_eff[1], yend = 1.5), linetype = "dashed") +
  geom_segment(aes(x = lls$mean_eff[2], y = 1.5, xend = lls$mean_eff[2], yend = 2.5), linetype = "dashed") +
  geom_segment(aes(x = lls$mean_eff[3], y = 2.5, xend = lls$mean_eff[3], yend = 3.5), linetype = "dashed")

p
ggsave(plot = p, file = "temp/sens_regs.png")
