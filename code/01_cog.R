d <- read_fwf("raw_data/2017_individual_unit_file/2017FinEstDAT_09102020modp_pu.txt",
              col_positions = fwf_widths(c(14, 3, 12, 4, 1),
                                         col_names = c("place_id",
                                                       "item_code",
                                                       "amount",
                                                       "year",
                                                       "imputation")),
              col_types = "ccccc") %>% 
  mutate(amount = as.numeric(amount) * 1000)


police <- d %>% 
  filter(item_code %in% c("E62", "F62", "G62", "I62", "L62", "M62", "N62", "O62", "P62", "R62",
                          "E25", "I25", "F25", "G25", "K25")) %>% 
  filter(as.numeric(amount) > 0)


info <- read_fwf("raw_data/2017_individual_unit_file/Fin_GID_2017.txt",
                 col_positions = fwf_widths(c(14, 64, 35, 2, 3, 5, 9, 2, 7, 2, 2, 2, 4, 2)),
                 col_types = "cccccccccccccc")

rev <- d %>% 
  filter(substring(item_code, 1, 1) %in% c("T", "B", "C", "D", "A", "U")) %>% 
  mutate(state_fed = amount * substring(item_code, 1, 1) %in% c("B", "C"),
         taxes = amount * substring(item_code, 1, 1) %in% c("T")) %>% 
  group_by(place_id) %>% 
  summarize(total_revenue = sum(amount),
            rev_state_fed = sum(state_fed),
            taxes = sum(taxes)) %>% 
  mutate(share_state_fed = rev_state_fed / total_revenue,
         share_taxes = taxes / total_revenue) %>% 
  select(-rev_state_fed, -taxes) %>% 
  ungroup()

fines_rev <- d %>% 
  filter(item_code == "U30",
         place_id %in% police$place_id) %>% 
  select(-imputation, -item_code) %>% 
  mutate(amount = as.numeric(amount)) %>% 
  rename(fees_fines = amount)

police_no_fines <- data.table(place_id = unique(d$place_id),
                              fees_fines = rep(0, length(unique(d$place_id)))) %>% 
  filter(place_id %in% police$place_id,
         !(place_id %in% fines_rev$place_id)) %>% 
  mutate(year = "2017")

fines_rev <- bind_rows(fines_rev, police_no_fines)

fines_rev <- left_join(fines_rev, rev)


fines_rev <- left_join(fines_rev, select(info,
                                         X1,
                                         population = X7,
                                         fips = X6,
                                         state = X4,
                                         name = X2), by = c("place_id" = "X1")) %>% 
  rename(pop_cog = population) %>% 
  mutate(pop_cog = as.numeric(pop_cog),
         dper = fees_fines / pop_cog,
         lndper = log(dper + 1))
############################# NOW DO 2012
d12 <- read_fwf("raw_data/2012_individual_unit_file/2012FinEstDAT_10162019modp_pu.txt",
                col_positions = fwf_widths(c(14, 3, 12, 4, 1),
                                           col_names = c("place_id",
                                                         "item_code",
                                                         "amount",
                                                         "year",
                                                         "imputation")),
                col_types = "ccccc") %>% 
  mutate(amount = as.numeric(amount) * 1000)

info12 <- read_fwf("raw_data/2012_individual_unit_file/Fin_GID_2012.txt",
                   col_positions = fwf_widths(c(14, 64, 35, 2, 3, 5, 9, 2, 7, 2, 2, 2, 4, 2)),
                   col_types = "cccccccccccccc")

rev_12 <- d12 %>% 
  filter(substring(item_code, 1, 1) %in% c("T", "B", "C", "D", "A", "U")) %>% 
  mutate(amount = as.numeric(amount),
         state_fed = amount * substring(item_code, 1, 1) %in% c("B", "C"),
         taxes = amount * substring(item_code, 1, 1) %in% c("T")) %>% 
  group_by(place_id) %>% 
  summarize(total_revenue = sum(amount),
            rev_state_fed = sum(state_fed),
            taxes = sum(taxes)) %>% 
  mutate(share_state_fed = rev_state_fed / total_revenue,
         share_taxes = taxes / total_revenue) %>% 
  select(-rev_state_fed, -taxes) %>% 
  ungroup()

d12_good <- d12 %>% 
  filter(item_code == "U30") %>% 
  select(-imputation, -item_code) %>% 
  mutate(amount = as.numeric(amount)) %>% 
  rename(fees_fines = amount)

d12_no_fines <- data.table(place_id = unique(d12$place_id),
                           fees_fines = rep(0, length(unique(d12$place_id)))) %>% 
  filter(place_id %in% fines_rev$place_id,
         !(place_id %in% d12_good$place_id)) %>% 
  mutate(year = "2012")

cog_12 <- full_join(bind_rows(d12_good, d12_no_fines), rev_12)

cog_12 <- left_join(cog_12, select(info12,
                                      X1,
                                      population = X7,
                                      fips = X6,
                                      state = X4,
                                      name = X2), by = c("place_id" = "X1")) %>% 
  rename(pop_cog = population) %>% 
  mutate(pop_cog = as.numeric(pop_cog),
         dper = fees_fines / pop_cog,
         lndper = log(dper + 1)) %>% 
  filter(place_id %in% fines_rev$place_id)

saveRDS(cog_12, "temp/cog_12.rds")
cleanup(c("fines_rev", "cog_12"))
##################################
# 
# 
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
#     j <- full_join(j, t)
# 
#     saveRDS(j, paste0("temp/census_sub_", s, ".rds"))
#   }else{
#     j <- readRDS(paste0("temp/census_sub_", s, ".rds"))
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
#                     share_moved, share_no_car, share_over_64, share_under40, pop_dens),
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
# 
# pcvap <- fread("../regular_data/CVAP_2014-2018_ACS_csv_files/Place.csv") %>%
#   filter(lntitle != "Not Hispanic or Latino") %>% 
#   mutate(GEOID = substring(geoid, 8),
#          lntitle = ifelse(lntitle == "Total", "total",
#                           ifelse(lntitle == "Hispanic or Latino", "latino",
#                                  ifelse(lntitle == "Black or African American Alone", "black",
#                                         ifelse(lntitle == "White Alone", "white",
#                                                ifelse(lntitle == "Asian Alone", "asian", "other"))))),
#          lntitle = ifelse(lntitle == "total", "cvap",
#                           paste0(lntitle, "_cvap"))) %>%
#   group_by(GEOID, lntitle) %>%
#   summarize(cvap = sum(cvap_est)) %>%
#   pivot_wider(id_cols = GEOID, names_from = lntitle, values_from = cvap, values_fill = 0) %>%
#   mutate(nonwhite_cvap = cvap - white_cvap)
# #
# #####################################
# cscvap <- fread("../regular_data/CVAP_2014-2018_ACS_csv_files/MCD.csv") %>%
#   filter(lntitle != "Not Hispanic or Latino") %>% 
#   mutate(GEOID = substring(geoid, 8),
#          lntitle = ifelse(lntitle == "Total", "total",
#                           ifelse(lntitle == "Hispanic or Latino", "latino",
#                                  ifelse(lntitle == "Black or African American Alone", "black",
#                                         ifelse(lntitle == "White Alone", "white",
#                                                ifelse(lntitle == "Asian Alone", "asian", "other"))))),
#          lntitle = ifelse(lntitle == "total", "cvap",
#                           paste0(lntitle, "_cvap"))) %>%
#   group_by(GEOID, lntitle) %>%
#   summarize(cvap = sum(cvap_est)) %>%
#   pivot_wider(id_cols = GEOID, names_from = lntitle, values_from = cvap, values_fill = 0) %>%
#   mutate(nonwhite_cvap = cvap - white_cvap) %>%
#   mutate(GEOID = paste0(substring(GEOID, 1, 2), substring(GEOID, 6))) %>%
#   group_by(GEOID) %>%
#   summarize_at(vars(ends_with("cvap")), sum) %>%
#   filter(!GEOID %in% pcvap$GEOID)
# 
# cvap <- bind_rows(cscvap, pcvap)
# 
# c2 <- left_join(c2, cvap)
# saveRDS(c2, "temp/census_data.rds")

##################################

c2 <- readRDS("temp/census_data.rds")

cities <- inner_join(mutate(fines_rev, GEOID = paste0(state, fips)), c2, by = "GEOID") %>% 
  filter(pop_cog >= 2500)

cities <- left_join(cities, select(cog_12,
                                   place_id, lndper12 = lndper, dper_12 = dper))

saveRDS(cities, "temp/cog_cities.rds")

# ggplot(filter(cities, dper > 0),
#        aes(x = nh_black, y = dper)) + geom_point() + theme_bc() + geom_smooth(method = "lm") +
#   scale_x_continuous(labels = scales::percent, breaks = c(0.01, 0.05, 0.1, 0.25, 0.5, 1), trans = "log") +
#   scale_y_log10(labels = scales::dollar) +
#   labs(y = "Dollars in Fines per Resident",
#        x = "Share Non-Hispanic Black")
#########################################################
