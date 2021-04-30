##################################

db_raw <- dbConnect(SQLite(), "D:/rolls.db")
nc_race <- dbGetQuery(db_raw, "select ncid, race_code from nc_roll_0219")


l2_db <- dbConnect(SQLite(), "D:/national_file.db")
nc_lookup <- dbGetQuery(l2_db, "select LALVOTERID, Voters_StateVoterID, EthnicGroups_EthnicGroup1Desc from nc")

nc_coded <- readRDS("temp/subs_places_37.rds")

nc_coded <- left_join(nc_coded, nc_lookup)

nc_coded <- left_join(nc_coded, nc_race, by = c("Voters_StateVoterID" = "ncid"))

nc_places <- nc_coded %>%
  filter(!is.na(race_code)) %>% 
  mutate(black = race_code == "B") %>% 
  group_by(black, plasub = place) %>%
  tally()

nc_subs <- nc_coded %>%
  filter(!is.na(race_code)) %>% 
  mutate(black = race_code == "B",
         plasub = paste0(substring(county_s, 1, 2), substring(county_s, 6))) %>% 
  group_by(black, plasub) %>%
  tally() %>% 
  filter(!(plasub %in% nc_places$plasub))

nc_real <- bind_rows(nc_subs, nc_places) %>% 
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


nc_real_model <- inner_join(nc_real, city_to)

saveRDS(nc_real_model, "temp/nc_real_model_city.rds")
