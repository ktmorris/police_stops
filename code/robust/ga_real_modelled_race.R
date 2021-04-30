##################################

db_raw <- dbConnect(SQLite(), "D:/rolls.db")
ga_race <- dbGetQuery(db_raw, "select REGISTRATION_NUMBER, RACE from ga_roll_0319")


l2_db <- dbConnect(SQLite(), "D:/national_file.db")
ga_lookup <- dbGetQuery(l2_db, "select LALVOTERID, Voters_StateVoterID, EthnicGroups_EthnicGroup1Desc from ga")

ga_coded <- readRDS("temp/subs_places_13.rds")

ga_coded <- left_join(ga_coded, ga_lookup)

ga_coded <- left_join(ga_coded, ga_race, by = c("Voters_StateVoterID" = "REGISTRATION_NUMBER"))

ga_places <- ga_coded %>%
  filter(!is.na(RACE)) %>% 
  mutate(black = RACE == "BH") %>% 
  group_by(black, plasub = place) %>%
  tally()

ga_subs <- ga_coded %>%
  filter(!is.na(RACE)) %>% 
  mutate(black = RACE == "BH",
         plasub = paste0(substring(county_s, 1, 2), substring(county_s, 6))) %>% 
  group_by(black, plasub) %>%
  tally() %>% 
  filter(!(plasub %in% ga_places$plasub))

ga_real <- bind_rows(ga_subs, ga_places) %>% 
  group_by(plasub) %>% 
  mutate(share_black_vf = n / sum(n)) %>% 
  filter(black) %>% 
  select(plasub, share_black_vf)

############################

cog <- readRDS("temp/cog_cities.rds") %>% 
  mutate(state = as.factor(state),
         fines_rev = dper / total_revenue)

place_to <- readRDS("temp/place_to.rds") %>% 
  rename(plasub = place)

county_s_to <- readRDS("temp/county_s_to.rds") %>% 
  rename(plasub = county_s)

county_s_to <- county_s_to[!(county_s_to$plasub %in% place_to$plasub),]

city_to <- bind_rows(place_to, county_s_to)

city_to <- city_to[city_to$plasub %in% cog$GEOID, ] %>% 
  mutate(black = EthnicGroups_EthnicGroup1Desc == "Likely African-American") %>% 
  group_by(plasub, black) %>% 
  summarize(count = sum(count)) %>% 
  group_by(plasub) %>% 
  mutate(share_black_l2 = count / sum(count)) %>% 
  filter(black) %>% 
  select(plasub, share_black_l2)


ga_real_model <- inner_join(ga_real, city_to)

mae(ga_real_model$share_black_vf, ga_real_model$share_black_l2)

saveRDS(ga_real_model, "temp/ga_real_model_city.rds")
