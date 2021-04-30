##################################

db_raw <- dbConnect(SQLite(), "D:/rolls.db")
fl_race <- dbGetQuery(db_raw, "select Race, Voter_ID from fl_roll_201902")


l2_db <- dbConnect(SQLite(), "D:/national_file.db")
fl_lookup <- dbGetQuery(l2_db, "select LALVOTERID, Voters_StateVoterID, EthnicGroups_EthnicGroup1Desc from fl")

fl_coded <- readRDS("temp/subs_places_12.rds")

fl_coded <- left_join(fl_coded, fl_lookup)

fl_coded <- left_join(fl_coded, fl_race, by = c("Voters_StateVoterID" = "Voter_ID"))

fl_places <- fl_coded %>%
  filter(!is.na(Race)) %>% 
  mutate(black = Race == 3) %>% 
  group_by(black, plasub = place) %>%
  tally()

fl_subs <- fl_coded %>%
  filter(!is.na(Race)) %>% 
  mutate(black = Race == 3,
         plasub = paste0(substring(county_s, 1, 2), substring(county_s, 6))) %>% 
  group_by(black, plasub) %>%
  tally() %>% 
  filter(!(plasub %in% fl_places$plasub))
  
fl_real <- bind_rows(fl_subs, fl_places) %>% 
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


fl_real_model <- inner_join(fl_real, city_to)

saveRDS(fl_real_model, "temp/fl_real_model_city.rds")
