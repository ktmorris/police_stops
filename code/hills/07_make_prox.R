
stopped <- readRDS("temp/hills_pre_match.rds")

voters <- readRDS("temp/full_raw_coded_hills_w_bgs.rds") %>% 
  filter(count == 1) %>% 
  ungroup() %>% 
  mutate_at(vars(street, city), tolower) %>% 
  group_by(city, street) %>% 
  mutate(count = n()) %>% 
  filter(count <= 5) %>% 
  ungroup() %>% 
  select(-count)


stopped <- left_join(stopped, select(voters, voter_id, street, city))


ads <- stopped %>% 
  mutate_at(vars(street, city), tolower) %>% 
  group_by(first_tr_year, street, city) %>% 
  tally() %>% 
  select(-n)


voters2 <- left_join(voters,
                    ads)

prox14 <- filter(voters2, first_tr_year == "2014-11-04",
                 !(voter_id %in% filter(stopped, first_tr_year == "2014-11-04")$voter_id))

prox16 <- filter(voters2, first_tr_year == "2016-11-08",
                 !(voter_id %in% filter(stopped, first_tr_year == "2016-11-08")$voter_id))

prox18 <- filter(voters2, first_tr_year == "2018-11-06",
                 !(voter_id %in% filter(stopped, first_tr_year == "2018-11-06")$voter_id))

prox20 <- filter(voters2, first_tr_year == "2020-11-03",
                 !(voter_id %in% filter(stopped, first_tr_year == "2020-11-03")$voter_id))

proximals <- bind_rows(prox14, prox16, prox18, prox20) %>% 
  mutate(white = race == 5,
         black = race == 3,
         latino = race == 4,
         asian = race == 2,
         male = gender == "M",
         dem = party_affiliation == "DEM",
         rep = party_affiliation == "REP",
         reg_date = as.Date(registration_date, "%m/%d/%Y")) %>% 
  select(voter_id, GEOID,
         white, black, latino, asian, male, dem, rep, age, 
         reg_date, name_first, name_last, birth_date,
         v08, v10, v12, v14, v16, v18, latitude, longitude, first_tr_year)

#######################

census <- readRDS("../regular_data/census_bgs_18.rds")

proximals <- left_join(proximals, census %>% 
                      select(median_income, some_college, unem, GEOID)) %>% 
  select(-name_first, -name_last, -birth_date)

match_data <- proximals %>% 
  mutate(reg_date = reg_date - as.Date("2000-01-01"))


match_data <- match_data[complete.cases(match_data),]

saveRDS(select(match_data, -v18) %>% ungroup(), "temp/hills_pre_match_proximal.rds")
