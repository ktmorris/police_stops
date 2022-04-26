hills_stops <- rbindlist(lapply(c(2003:2021), function(l){
  print(l)
  if(l != "J"){
    if(!(file.exists(paste0("raw_data/hills_stops/civil_stops_", l, ".csv")))){
      download.file(paste0("https://publicrec.hillsclerk.com/Traffic/Civil_Traffic_Name_Index_files/Civil_Traffic_Name_Index_", l, ".csv"),
                    paste0("raw_data/hills_stops/civil_stops_", l, ".csv"))
    }
    
    ja <- fread(paste0("raw_data/hills_stops/civil_stops_", l, ".csv"))
    
    colnames(ja) <- clean_names(ja)
    
    ja <- ja %>%
      filter(grepl("cam", tolower(statute_description))) %>%
      select(last_name, first_name, middle_name,
             date_of_birth,
             street = address_line_1,
             city,
             state,
             zip = zip_code,
             offense_date,
             amount_paid,
             law_enf_agency_name) %>%
      mutate(civil = 1,
             tampa_pd = grepl("tampa police", tolower(law_enf_agency_name)),
             tampa_pd = ifelse(tampa_pd, T, law_enf_agency_name == "TPD")) %>%
      select(-law_enf_agency_name)
    
    return(ja)
  }
}))

hills_stops <- hills_stops %>%
  mutate(amount_paid = ifelse(is.na(amount_paid), 0, amount_paid)) %>%
  mutate(offense_date = as.Date(offense_date, "%m/%d/%Y"),
         date_of_birth = as.Date(date_of_birth, "%m/%d/%Y")) %>%
  mutate_at(vars(first_name,
                 last_name), ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, toupper(.))))

saveRDS(hills_stops, "temp/hills_stops_cam.rds")

hills_stops <- readRDS("temp/hills_stops_cam.rds")

#########

pre12 <- filter(hills_stops, offense_date <= "2012-11-06")[,
                                                           .(stop_count = .N),
                                                           .(first_name, last_name, date_of_birth, offense_date)]  # using data.table syntax for speed

pre12 <-  pre12[,
                .(stop_count = .N),
                .(first_name, last_name, date_of_birth)]  # using data.table syntax for speed

pre14 <- filter(hills_stops, offense_date <= "2014-11-04")[,
                                                           .(stop_count = .N),
                                                           .(first_name, last_name, date_of_birth, offense_date)]  # using data.table syntax for speed

pre14 <-  pre14[,
                .(stop_count = .N),
                .(first_name, last_name, date_of_birth)]  # using data.table syntax for speed

pre16 <- filter(hills_stops, offense_date <= "2016-11-08")[,
                                                           .(stop_count = .N),
                                                           .(first_name, last_name, date_of_birth, offense_date)]  # using data.table syntax for speed

pre16 <-  pre16[,
                .(stop_count = .N),
                .(first_name, last_name, date_of_birth)]  # using data.table syntax for speed

############################


s12_14 <- filter(hills_stops, offense_date > "2012-11-06",
                 offense_date <= "2014-11-04")[,
                                               .(amount_paid = sum(amount_paid),
                                                 civil = min(civil),
                                                 tampa_pd = max(tampa_pd)),
                                               .(first_name, last_name, date_of_birth, offense_date)]
s12_14 <- s12_14[,
                 .(stop_count = .N,
                   last_date = max(offense_date),
                   amount_paid = sum(amount_paid),
                   civil = min(civil),
                   tampa_pd = max(tampa_pd)),
                 .(first_name, last_name, date_of_birth)] %>%
  mutate(first_tr_year = as.Date("2014-11-04"))

s12_14 <- left_join(s12_14, rename(pre12, pre_stops = stop_count))
##########################

s14_16 <- filter(hills_stops, offense_date > "2014-11-04",
                 offense_date <= "2016-11-08")[,
                                               .(amount_paid = sum(amount_paid),
                                                 civil = min(civil),
                                                 tampa_pd = max(tampa_pd)),
                                               .(first_name, last_name, date_of_birth, offense_date)]
s14_16 <- s14_16[,
                 .(stop_count = .N,
                   last_date = max(offense_date),
                   amount_paid = sum(amount_paid),
                   civil = min(civil),
                   tampa_pd = max(tampa_pd)),
                 .(first_name, last_name, date_of_birth)] %>%
  mutate(first_tr_year = as.Date("2016-11-08"))


s14_16 <- left_join(s14_16, rename(pre14, pre_stops = stop_count))
s14_16 <- left_join(s14_16, rename(pre12, pre_stops_c = stop_count))
##########################

s16_18 <- filter(hills_stops, offense_date > "2016-11-08",
                 offense_date <= "2018-11-06")[,
                                               .(amount_paid = sum(amount_paid),
                                                 civil = min(civil),
                                                 tampa_pd = max(tampa_pd)),
                                               .(first_name, last_name, date_of_birth, offense_date)]
s16_18 <- s16_18[,
                 .(stop_count = .N,
                   last_date = max(offense_date),
                   amount_paid = sum(amount_paid),
                   civil = min(civil),
                   tampa_pd = max(tampa_pd)),
                 .(first_name, last_name, date_of_birth)] %>%
  mutate(first_tr_year = as.Date("2018-11-06"))

s16_18 <- left_join(s16_18, rename(pre16, pre_stops = stop_count))
s16_18 <- left_join(s16_18, rename(pre14, pre_stops_c = stop_count))
##########################

s18_20 <- filter(hills_stops, offense_date > "2018-11-06",
                 offense_date <= "2020-11-03")[,
                                               .(amount_paid = sum(amount_paid),
                                                 civil = min(civil),
                                                 tampa_pd = max(tampa_pd)),
                                               .(first_name, last_name, date_of_birth, offense_date)]
s18_20 <- s18_20[,
                 .(stop_count = .N,
                   last_date = max(offense_date),
                   amount_paid = sum(amount_paid),
                   civil = min(civil),
                   tampa_pd = max(tampa_pd)),
                 .(first_name, last_name, date_of_birth)] %>%
  mutate(first_tr_year = as.Date("2020-11-03"))

s18_20 <- left_join(s18_20, rename(pre16, pre_stops_c = stop_count))

###########################
hills_stops_ll <- bind_rows(
  s12_14,
  s14_16,
  s16_18,
  s18_20,
) %>%
  mutate(pre_stops = ifelse(is.na(pre_stops), 0, pre_stops),
         pre_stops_c = ifelse(is.na(pre_stops_c), 0, pre_stops_c))

saveRDS(hills_stops_ll, "temp/hills_stops_ll_multi_cam.rds")

########################
hills_stops_ll <- readRDS("temp/hills_stops_ll_multi_cam.rds")

hills_voters <- readRDS("temp/full_raw_coded_hills_w_bgs.rds") %>% 
  filter(count == 1) %>% 
  ungroup()

joined <- left_join(hills_voters, hills_stops_ll,
                    by = c("name_first" = "first_name",
                           "name_last" = "last_name",
                           "birth_date" = "date_of_birth")) %>% 
  mutate(age = 2020 - year(birth_date)) %>% 
  filter(!is.na(pre_stops))


####################### test permuting birth dates to check for false positives

stops_names_dobs <- select(hills_stops_ll, first_name,
                           last_name, date_of_birth) %>% 
  distinct()

j1 <- inner_join(hills_voters,
                 stops_names_dobs,
                 by = c("name_first" = "first_name",
                        "name_last" = "last_name",
                        "birth_date" = "date_of_birth"))

joined2 <- inner_join(hills_voters,
                      stops_names_dobs %>%
                        mutate(date_of_birth = date_of_birth + 35),
                      by = c("name_first" = "first_name",
                             "name_last" = "last_name",
                             "birth_date" = "date_of_birth"))

joined3 <- inner_join(hills_voters,
                      stops_names_dobs %>%
                        mutate(date_of_birth = date_of_birth - 35),
                      by = c("name_first" = "first_name",
                             "name_last" = "last_name",
                             "birth_date" = "date_of_birth"))

test <- data.table(group = c("Actual Birthdate", "Birthdate + 35 Days", "Birthdate - 35 Days"),
                   values = c(nrow(j1),
                              nrow(joined2),
                              nrow(joined3))) %>% 
  mutate(values = comma(values))

saveRDS(test, "temp/plus_minus_35_cam.rds")
######################################

joined <- joined %>% 
  mutate(stop_count = ifelse(is.na(stop_count), 0, stop_count),
         pre_stops = ifelse(is.na(pre_stops), 0, pre_stops),
         pre_stops_c = ifelse(is.na(pre_stops_c), 0, pre_stops_c),
         amount_paid = ifelse(is.na(amount_paid), 0, amount_paid),
         white = race == 5,
         black = race == 3,
         latino = race == 4,
         asian = race == 2,
         male = gender == "M",
         dem = party_affiliation == "DEM",
         rep = party_affiliation == "REP",
         reg_date = as.Date(registration_date, "%m/%d/%Y")) %>% 
  select(voter_id, GEOID, last_date,
         white, black, latino, asian, male, dem, rep, age, pre_stops_c, civil,
         reg_date, pre_stops, amount_paid, name_first, name_last, birth_date, tampa_pd,
         v08, v10, v12, v14, v16, v18, latitude, longitude, first_tr_year) %>% 
  mutate(paid = amount_paid > 0)

### remove people who were stopped, not fined
# joined <- left_join(joined, readRDS("temp/stopped_no_fine.rds"),
#                     by = c("name_first" = "first_name",
#                            "name_last" = "last_name",
#                            "birth_date" = "date_of_birth")) %>% 
# filter(is.na(exclude)) %>%
# select(-exclude)


census <- readRDS("../regular_data/census_bgs_18.rds")

joined <- left_join(joined, census %>% 
                      select(median_income, some_college, unem, GEOID)) %>% 
  select(-name_first, -name_last, -birth_date)

match_data <- joined %>% 
  mutate(reg_date = reg_date - as.Date("2000-01-01"))


match_data <- match_data[complete.cases(match_data),]

saveRDS(select(match_data, -v18) %>% ungroup(), "temp/hills_pre_match_cam.rds")
