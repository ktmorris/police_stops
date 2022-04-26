
#####
hills_pre_match <- readRDS("temp/real_pre_match_hills_cam.rds") %>% 
  ungroup()

matches <- readRDS("temp/matches_hills_yem_cam.rds")

matches <- left_join(matches, select(hills_pre_match, voter_id, first_tr_year),
                     by = c("group" = "voter_id", "first_tr_year")) %>% 
  mutate(fd = first_tr_year,
         first_tr_year = ifelse(first_tr_year == 1, "2014-11-04",
                                ifelse(first_tr_year == 2, "2016-11-08",
                                       ifelse(first_tr_year == 3, "2018-11-06", "XX"))),
         first_tr_year = as.Date(first_tr_year))

hist <- readRDS("C:/Users/morrisk/Desktop/full_raw_coded_hills_w_bgs.rds") %>%
  select(voter_id, starts_with("v1"), v08) %>%
  pivot_longer(!starts_with("vo"), names_to = "year", values_to = "to")

elec_dates <- fread("raw_data/election_dates.csv")

hist <- left_join(hist, elec_dates) %>% 
  select(-year) %>% 
  rename(year = date) %>% 
  mutate(year = as.Date(year, "%m/%d/%Y"))


matches <- left_join(matches, hist %>% 
                       filter(year(year) %% 2 == 0), by = c("voter" = "voter_id"))

periods <- fread("raw_data/period_lu.csv") %>% 
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

##########################################################
ll <- bind_rows(mutate(matches, first_tr_year = as.character(first_tr_year)),
                mutate(matches, first_tr_year = "Overall")) %>%
  filter(period <= 0.5) %>% 
  group_by(treated, period, black_t, first_tr_year) %>%
  summarize(to = weighted.mean(to, weight)) %>% 
  mutate(treated = ifelse(treated, "Treated", "Control"),
         black = ifelse(black_t, "Black Voters", "Non-Black Voters"))

ll$treated <- factor(ll$treated, levels = c("Treated", "Control"))

ll <- ll %>% 
  mutate(first_tr_year = ifelse(first_tr_year == "Overall", first_tr_year,
                                paste("t = 0\nin", substring(first_tr_year, 1, 4))))

ll$first_tr_year <- factor(ll$first_tr_year, levels = unique(ll$first_tr_year))

p2 <- ggplot(data = ll) + 
  facet_grid(first_tr_year ~ black) +
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
       caption = "Treatment occurs inside of yellow band.", ) +
  coord_cartesian(ylim = c(0.05, 0.75))
p2
saveRDS(p2, "temp/stopped_any_time_y.rds")

#####################

m1 <- to ~ treated * post + as.factor(year)

m2 <- to ~ treated * post + as.factor(year) +
  white + black + latino + asian + male +
  dem + rep + age + reg_date + pre_stops + v1 + v2 + v3 +
  median_income + some_college + unem + civil + paid + tampa_pd

m3 <- to ~ treated * post * black + as.factor(year)

m4 <- to ~ treated * post * black + as.factor(year) +
  white + black + latino + asian + male +
  dem + rep + age + reg_date + pre_stops + v1 + v2 + v3 +
  median_income + some_college + unem + civil + paid + tampa_pd

matches$first_tr_year <- as.character(matches$first_tr_year)

for(gg in c("overall", "2014-11-04", "2016-11-08", "2018-11-06")){
  
  if(gg == "overall"){
    dat1 <- filter(matches, period <= 0.5)
    tit = "\\label{tab:dind-table} Overall Treatment Effect"
    ooo = c(1, 2, 4, 23, 24, 22, 25)
  }else{
    dat1 <- dplyr::filter(matches, period <= 0.5, first_tr_year == gg)
    tit = paste0("\\label{tab:dind-table-", substring(gg, 1, 4), "}Treatment Effect for Voters Stopped before ", substring(gg, 1, 4), " Election")
    ooo = c(1, 2, 7, 23, 24, 22, 25)
  }
 
  models1 <- lapply(c(m1, m2, m3, m4), function(f){
    m <- lm(f, data = dat1,
            weight = dat1$weight)
  })
  
  
  ses_cl <- list(
    summary(lm.cluster(formula = m1, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
    summary(lm.cluster(formula = m2, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
    summary(lm.cluster(formula = m3, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2],
    summary(lm.cluster(formula = m4, data = dat1, weights = dat1$weight, cluster = dat1$group))[ , 2]
  )
  
  
  stargazer(models1,
            type = "text",
            omit.stat = c("f", "ser"),
            se = ses_cl,
            omit = c("as.fac"),
            covariate.labels = c("Treated",
                                 "Post Treatment",
                                 "Black",
                                 "Treated $\\times$ Post Treatment",
                                 "Treated $\\times$ Black",
                                 "Post Treatment $\\times$ Black",
                                 "Treated $\\times$ Post Treatment $\\times$ Black"),
            table.layout = "-cm#-t-a-s-n",
            keep = c("treatedTRUE", "post", "blackTRUE", "Constant"),
            order = ooo,
            notes = "TO REPLACE",
            table.placement = "H",
            title = tit,
            out = paste0("temp/two_matches_reg_y_", gg, ".tex"),
            add.lines = list(c("Includes Matched Covariates", "", "X", "", "X"),
                             c("Includes Year Fixed Effects", "X", "X", "X", "X")))
  
  j <- fread(paste0("temp/two_matches_reg_y_", gg, ".tex"), header = F, sep = "+")
  
  note.latex <- "\\multicolumn{5}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$. \\\\Robust standard errors (clustered at level of match) in parentheses.}}}"
  
  j <- j %>% 
    mutate(n = row_number(),
           V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
           V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>% 
    filter(!grepl("Note:", V1))
  
  
  j <- j %>%
    arrange(n) %>%
    select(-n)
  
  
  write.table(j, paste0("./temp/dind_reg_y", gg, ".tex"), quote = F, col.names = F,
              row.names = F)
   
}



########################

dat1$year <- as.factor(dat1$year)
dat1$lnfee <- log(dat1$max_amount)

tt <- lm(to ~ treated * post * amount_paid + year, dat1, weights = weight)

h <- ggeffect(tt, terms = c("days_before[all]", "treated", "post"))

h <- filter(h, facet == "TRUE")

h <- h %>% 
  mutate(group = ifelse(group == "TRUE",
                        "Treated",
                        "Control"))

inte <- reconPlots::curve_intersect(
  filter(h, group == "Treated") %>% 
    select(x, y = predicted),
  filter(h, group == "Control") %>% 
    select(x, y = predicted)
)

inte$x <- -inte$x

dat1$days_before <- -dat1$days_before
h$x <- -h$x

ggplot() + 
  geom_histogram(aes(x = days_before, y = ..count../2500000), position="identity", linetype=1,
                 fill="gray60", data = dat1, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted, color = group), data = h) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha=0.25, data = h) +
  xlab("Days before Election") +
  ylab("Predicted Turnout") + scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = percent) +
  labs(caption = "Notes: Distribution of days of tickets shown at bottom.",
       color = "Treatment Group",
       fill = "Treatment Group") +
  theme(plot.caption = element_text(hjust = 0)) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10")) +
  geom_vline(xintercept = inte$x, linetype = "dashed")

#######################################
hills_voters <- readRDS("temp/full_raw_coded_hills_w_bgs.rds")
hills_pre_match <- readRDS("temp/hills_pre_match.rds")

hills_voters <- filter(hills_voters, !(voter_id %in% hills_pre_match$voter_id)) %>% 
  mutate(white = race == 5,
         black = race == 3,
         latino = race == 4,
         asian = race == 2,
         male = gender == "M",
         dem = party_affiliation == "DEM",
         rep = party_affiliation == "REP")

hills_voters <- left_join(hills_voters, readRDS("../regular_data/census_bgs_18.rds") %>% 
                            select(median_income, some_college, unem, GEOID)) %>% 
  summarize_at(vars(c("white", "black", "latino",
                      "asian", "male", "dem", "rep", "age", "median_income", "some_college", "unem")),
               mean, na.rm = T) %>% 
  mutate(treated = "Never Stopped")

low_demos <- bind_rows(matches %>% 
  group_by(treated = as.character(treated)) %>% 
  summarize_at(vars(c("paid", "civil", "tampa_pd", "white", "black", "latino",
                    "asian", "male", "dem", "rep", "age", "pre_stops", "v1",
                    "v2", "v3", "median_income", "some_college", "unem")),
                    ~ weighted.mean(., weight)),
  hills_voters) %>% 
  mutate_at(vars(paid, civil, tampa_pd, white, black, latino, asian, male, dem,
                 rep, v1, v2, v3, some_college, unem), percent, accuracy = 0.1) %>% 
  mutate_at(vars(age, pre_stops), ~format(round(., 1))) %>% 
  mutate_at(vars(median_income), dollar, accuracy = 1) %>% 
  pivot_longer(cols = c("paid", "civil", "tampa_pd", "white", "black", "latino",
                        "asian", "male", "dem", "rep", "age", "pre_stops", "v1",
                        "v2", "v3", "median_income", "some_college", "unem")) %>% 
  pivot_wider(id_cols = "name", names_from = "treated", values_from = "value") %>% 
  select(name, `Treated Voters` = `TRUE`, `Control Voters` = `FALSE`, `Never Stopped`)

ord <- fread("./raw_data/var_orders.csv")

low_demos <- left_join(low_demos, ord, by = c("name" = "variable")) %>% 
  select(-name) %>% 
  rename(name = name.y) %>% 
  arrange(order) %>% 
  select(-order) %>% 
  mutate(`Never Stopped` = ifelse(is.na(`Never Stopped`)| `Never Stopped` == " NA", "", `Never Stopped`)) %>% 
  select(Variable = name, everything()) %>%
  mutate_all(~ gsub("[%]", paste0("\\\\", "%"), .)) %>%
  mutate_all(~ gsub("[$]", paste0("\\\\", "$"), .))

saveRDS(low_demos, "./temp/balance_table_y.rds")
