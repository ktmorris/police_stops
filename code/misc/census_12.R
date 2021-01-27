c <- get_basic_census_stats("place", 2012) %>%
  select(-NAME)

ptime <- get_acs("place", variables = c("B08013_001", "B08006_001"), year = 2012, output = "wide") %>%
  mutate(travel_time = B08013_001E/ B08006_001E) %>%
  select(GEOID, travel_time)

c <- full_join(c, ptime)

sd <- rbindlist(lapply(unique(filter(fips_codes, as.integer(state_code) < 60)$state_code), function(s){
  if(!file.exists(paste0("temp/census_sub_", s, "_12.rds"))){
    j <- get_basic_census_stats("county subdivision", 2012, state = s)
    t <- get_acs("county subdivision", variables = c("B08013_001", "B08006_001"),
                 output = "wide", year = 2012, state = s) %>%
      mutate(travel_time = B08013_001E/ B08006_001E) %>%
      select(GEOID, travel_time)

    j <- full_join(j, t)

    saveRDS(j, paste0("temp/census_sub_", s, "_12.rds"))
  }else{
    j <- readRDS(paste0("temp/census_sub_", s, "_12.rds"))
  }
  return(j)
})) %>%
  mutate(GEOID = paste0(substring(GEOID, 1, 2), substring(GEOID, 6))) %>%
  filter(!GEOID %in% c$GEOID)

sd1 <- sd %>%
  group_by(GEOID) %>%
  summarize_at(vars(asian, hawaiian_pac_island, latino, latino_black,
                    native_american, nh_black, nh_white, median_income,
                    some_college, unem, median_age, share_non_citizen, travel_time,
                    share_moved, share_no_car, share_over_64, share_under40, pop_dens),
               ~ weighted.mean(., population, na.rm = T))

sd2 <- sd %>%
  group_by(GEOID) %>%
  summarize(population = sum(population, na.rm = T),
            vap = sum(vap, na.rm = T))

sd <- inner_join(sd1, sd2)

c2 <- bind_rows(c, sd)

pcvap <- fread("../regular_data/CVAP_2008-2012_ACS_csv_files/Place.csv") %>%
  mutate(GEOID = substring(GEOID, 8),
         LNTITLE = ifelse(LNTITLE == "Total", "total",
                          ifelse(LNTITLE == "Hispanic or Latino", "latino",
                                 ifelse(LNTITLE == "Black or African American Alone", "black",
                                        ifelse(LNTITLE == "White Alone", "white", "other")))),
         LNTITLE = ifelse(LNTITLE == "total", "cvap",
                          paste0(LNTITLE, "_cvap"))) %>%
  group_by(GEOID, LNTITLE) %>%
  summarize(cvap = sum(CVAP_EST)) %>%
  pivot_wider(id_cols = GEOID, names_from = LNTITLE, values_from = cvap, values_fill = 0) %>%
  mutate(nonwhite_cvap = cvap - white_cvap) %>%
  select(-other_cvap)

#####################################
cscvap <- fread("../regular_data/CVAP_2008-2012_ACS_csv_files/MCD.csv") %>%
  mutate(GEOID = substring(GEOID, 8),
         LNTITLE = ifelse(LNTITLE == "Total", "total",
                          ifelse(LNTITLE == "Hispanic or Latino", "latino",
                                 ifelse(LNTITLE == "Black or African American Alone", "black",
                                        ifelse(LNTITLE == "White Alone", "white", "other")))),
         LNTITLE = ifelse(LNTITLE == "total", "cvap",
                          paste0(LNTITLE, "_cvap"))) %>%
  group_by(GEOID, LNTITLE) %>%
  summarize(cvap = sum(CVAP_EST)) %>%
  pivot_wider(id_cols = GEOID, names_from = LNTITLE, values_from = cvap, values_fill = 0) %>%
  mutate(nonwhite_cvap = cvap - white_cvap) %>%
  select(-other_cvap) %>%
  mutate(GEOID = paste0(substring(GEOID, 1, 2), substring(GEOID, 6))) %>%
  group_by(GEOID) %>%
  summarize_at(vars(cvap, white_cvap, nonwhite_cvap, black_cvap, latino_cvap), sum) %>%
  filter(!GEOID %in% pcvap$GEOID)

cvap <- bind_rows(cscvap, pcvap)

c2 <- left_join(c2, cvap)

saveRDS(c2, "temp/census_12.rds")
