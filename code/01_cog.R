d <- read_fwf("raw_data/2017_individual_unit_file/2017FinEstDAT_09102020modp_pu.txt",
              col_positions = fwf_widths(c(14, 3, 12, 4, 1),
                                         col_names = c("place_id",
                                                       "item_code",
                                                       "amount",
                                                       "year",
                                                       "imputation")),
              col_types = "ccccc")


police <- d %>% 
  filter(item_code %in% c("E62", "F62", "G62", "I62", "L62", "M62", "N62", "O62", "P62", "R62",
                          "E25", "I25", "F25", "G25", "K25")) %>% 
  filter(as.numeric(amount) > 0)


info <- read_fwf("raw_data/2017_individual_unit_file/Fin_GID_2017.txt",
                 col_positions = fwf_widths(c(14, 64, 35, 2, 3, 5, 9, 2, 7, 2, 2, 2, 4, 2)),
                 col_types = "cccccccccccccc")

d <- left_join(d, select(info,
                         X1,
                         population = X7,
                         fips = X6,
                         state = X4,
                         name = X2), by = c("place_id" = "X1")) %>% 
  rename(pop_cog = population)

############################# NOW DO 2012
d12 <- read_fwf("raw_data/2012_individual_unit_file/2012FinEstDAT_10162019modp_pu.txt",
              col_positions = fwf_widths(c(14, 3, 12, 4, 1),
                                         col_names = c("place_id",
                                                       "item_code",
                                                       "amount",
                                                       "year",
                                                       "imputation")),
              col_types = "ccccc")


police12 <- d12 %>% 
  filter(item_code %in% c("E62", "F62", "G62", "I62", "L62", "M62", "N62", "O62", "P62", "R62",
                          "E25", "I25", "F25", "G25", "K25")) %>% 
  filter(as.numeric(amount) > 0)


info12 <- read_fwf("raw_data/2012_individual_unit_file/Fin_GID_2012.txt",
                 col_positions = fwf_widths(c(14, 64, 35, 2, 3, 5, 9, 2, 7, 2, 2, 2, 4, 2)),
                 col_types = "cccccccccccccc")

d12 <- left_join(d12, select(info12,
                             X1,
                             population = X7,
                             fips = X6,
                             state = X4,
                             name = X2), by = c("place_id" = "X1")) %>% 
  filter(item_code == "U30",
       place_id %in% police12$place_id) %>% 
  mutate(dper12 = as.numeric(amount)*1000 / as.numeric(population),
         GEOID = paste0(state, fips)) %>% 
  select(GEOID, dper12)
rm(info12, police12)
##################################


# c <- get_basic_census_stats("place", 2018) %>%
#   select(-NAME)
# 
# ptime <- get_acs("place", variables = c("B08013_001", "B08006_001"), year = 2018, output = "wide") %>% 
#   mutate(travel_time = B08013_001E/ B08006_001E) %>% 
#   select(GEOID, travel_time)
# 
# c <- full_join(c, ptime)
# 
# sd <- rbindlist(lapply(unique(filter(fips_codes, as.integer(state_code) < 60)$state_code), function(s){
#   if(!file.exists(paste0("temp/census_sub_", s, ".rds"))){
#     j <- get_basic_census_stats("county subdivision", 2018, state = s)
#     t <- get_acs("county subdivision", variables = c("B08013_001", "B08006_001"),
#                  output = "wide", year = 2018, state = s) %>% 
#       mutate(travel_time = B08013_001E/ B08006_001E) %>% 
#       select(GEOID, travel_time)
#     
#     j <- full_join(j, t)
#     
#     saveRDS(j, paste0("temp/census_sub_", s, ".rds"))
#   }else{
#     j <- readRDS(paste0("temp/census_sub_", s, ".rds"))
#     
#   }
#   return(j)
# })) %>%
#   mutate(GEOID = paste0(substring(GEOID, 1, 2), substring(GEOID, 6))) %>%
#   filter(!GEOID %in% c$GEOID)
# 
# sd1 <- sd %>%
#   group_by(GEOID) %>%
#   summarize_at(vars(asian, hawaiian_pac_island, latino, latino_black,
#                     native_american, nh_black, nh_white, median_income,
#                     some_college, unem, median_age, share_non_citizen, travel_time,
#                     share_moved, share_no_car, share_over_64, share_under40),
#                ~ weighted.mean(., population, na.rm = T))
# 
# sd2 <- sd %>%
#   group_by(GEOID) %>%
#   summarize(population = sum(population, na.rm = T),
#             vap = sum(vap, na.rm = T))
# 
# sd <- inner_join(sd1, sd2)
# 
# c2 <- bind_rows(c, sd)
# saveRDS(c2, "temp/census_data.rds")


###########
pcvap <- fread("../regular_data/CVAP_2014-2018_ACS_csv_files/Place.csv") %>% 
  mutate(GEOID = substring(geoid, 8),
         lntitle = ifelse(lntitle == "Total", "total",
                          ifelse(lntitle == "Hispanic or Latino", "latino",
                                 ifelse(lntitle == "Black or African American Alone", "black",
                                        ifelse(lntitle == "White Alone", "white", "other")))),
         lntitle = ifelse(lntitle == "total", "cvap",
                          paste0(lntitle, "_cvap"))) %>% 
  group_by(GEOID, lntitle) %>% 
  summarize(cvap = sum(cvap_est)) %>% 
  pivot_wider(id_cols = GEOID, names_from = lntitle, values_from = cvap, values_fill = 0) %>% 
  mutate(nonwhite_cvap = cvap - white_cvap) %>% 
  select(-other_cvap)

#####################################
cscvap <- fread("../regular_data/CVAP_2014-2018_ACS_csv_files/MCD.csv") %>% 
  mutate(GEOID = substring(geoid, 8),
         lntitle = ifelse(lntitle == "Total", "total",
                          ifelse(lntitle == "Hispanic or Latino", "latino",
                                 ifelse(lntitle == "Black or African American Alone", "black",
                                        ifelse(lntitle == "White Alone", "white", "other")))),
         lntitle = ifelse(lntitle == "total", "cvap",
                          paste0(lntitle, "_cvap"))) %>% 
  group_by(GEOID, lntitle) %>% 
  summarize(cvap = sum(cvap_est)) %>% 
  pivot_wider(id_cols = GEOID, names_from = lntitle, values_from = cvap, values_fill = 0) %>% 
  mutate(nonwhite_cvap = cvap - white_cvap) %>% 
  select(-other_cvap) %>% 
  mutate(GEOID = paste0(substring(GEOID, 1, 2), substring(GEOID, 6))) %>% 
  group_by(GEOID) %>% 
  summarize_at(vars(cvap, white_cvap, nonwhite_cvap, black_cvap, latino_cvap), sum) %>% 
  filter(!GEOID %in% pcvap$GEOID)

cvap <- bind_rows(cscvap, pcvap)
##################################

c2 <- readRDS("temp/census_data.rds")

cities <- inner_join(mutate(d, GEOID = paste0(state, fips)), c2, by = "GEOID") %>% 
  filter(item_code == "U30",
         place_id %in% police$place_id,
         as.integer(pop_cog) > 2500) %>% 
  mutate(dper = as.numeric(amount)*1000 / as.numeric(population),
         lndper = log(dper + 1),
         lnbl = log(nh_black + 1))

cities <- left_join(cities, d12)

cities <- left_join(cities, cvap)

saveRDS(cities, "temp/cog_cities.rds")

ggplot(filter(cities, dper > 0),
       aes(x = nh_black, y = dper)) + geom_point() + theme_bc() + geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent, breaks = c(0.01, 0.05, 0.1, 0.25, 0.5, 1), trans = "log") +
  scale_y_log10(labels = scales::dollar) +
  labs(y = "Dollars in Fines per Resident",
       x = "Share Non-Hispanic Black")
#########################################################
