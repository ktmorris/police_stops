# setwd("E:\\R WORKING DIRECTORY E")
# 
# hills_stops <- rbindlist(lapply(c(2003:2021), function(l){
#   print(l)
#   if(l != "J"){
#     if(!(file.exists(paste0("raw_data/hills_stops/civil_stops_", l, ".csv")))){
#       download.file(paste0("https://publicrec.hillsclerk.com/Traffic/Civil_Traffic_Name_Index_files/Civil_Traffic_Name_Index_", l, ".csv"),
#                     paste0("raw_data/hills_stops/civil_stops_", l, ".csv"))
#     }
#     
#     ja <- fread(paste0("raw_data/hills_stops/civil_stops_", l, ".csv"))
#     
#     colnames(ja) <- clean_names(ja)
#     
#     ja <- ja %>%
#       filter(!grepl("cam", tolower(statute_description))) %>%
#       select(last_name, first_name, middle_name,
#              date_of_birth,
#              street = address_line_1,
#              city,
#              state,
#              zip = zip_code,
#              offense_date,
#              amount_paid) %>%
#       mutate(civil = 1)
#     
#     return(ja)
#   }
# }))
# 
# hills_stops_cr <- rbindlist(lapply(c(2003:2021), function(l){
#   print(l)
#   if(l != "J"){
#     if(!(file.exists(paste0("raw_data/hills_stops/crim_stops_", l, ".csv")))){
#       download.file(paste0("https://publicrec.hillsclerk.com/Traffic/Criminal_Traffic_Name_Index_files/Criminal_Traffic_Name_Index_", l, ".csv"),
#                     paste0("raw_data/hills_stops/crim_stops_", l, ".csv"))
#     }
#     
#     ja <- fread(paste0("raw_data/hills_stops/crim_stops_", l, ".csv"))
#     
#     colnames(ja) <- clean_names(ja)
#     
#     ja <- ja %>%
#       select(last_name, first_name, middle_name,
#              date_of_birth,
#              offense_date, amount_paid) %>%
#       mutate(civil = 0)
#     
#     return(ja)
#   }
# }))
# hills_stops <- bind_rows(hills_stops, hills_stops_cr) %>%
#   mutate(amount_paid = ifelse(is.na(amount_paid), 0, amount_paid))
# 
# saveRDS(hills_stops, "temp/hills_stops.rds")
# #
hills_stops <- readRDS("temp/hills_stops.rds") %>%
  mutate(offense_date = as.Date(offense_date, "%m/%d/%Y"),
         date_of_birth = as.Date(date_of_birth, "%m/%d/%Y")) %>%
  mutate_at(vars(first_name,
                 last_name), ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, toupper(.))))
# 
# zeros_only <- hills_stops %>%
#   filter(offense_date > "2012-11-06",
#          offense_date <= "2018-11-06")
# 
# zeros_only <- zeros_only[,
#                          .(max_amount = max(amount_paid)),
#                          .(first_name, last_name, date_of_birth)] %>%
#   filter(max_amount == 0) %>%
#   select(first_name, last_name, date_of_birth) %>%
#   mutate(exclude = 1)
# 
# saveRDS(zeros_only, "temp/stopped_no_fine.rds")

# THE GOAL HERE IS TO COME UP WITH FOUR BIG SETS OF VOTERS:
# 1. PEOPLE STOPPED BETWEEN 2013 AND 2015 (TREATEMENT GROUP 1)
# 2. PEOPLE STOPPPED BETWEEN 2015 AND 2017 (CONTROL GROUP 1)
# 3. PEOPLE STOPPED BETWEEN 2017 AND 2019 (TREATMENT GROUP 2)
# 4. PEOPLE STOPPED BETWEEN 2019 AND 2021 (CONTROL GROUP 2)
# 
# FOR THE FIRST 2 SETS, WE WANT THE NUMBER OF STOPS PRIOR TO 2013; FOR THE SECOND 2, PRIOR TO 2019
# THIS IS A LITTLE SIMPLER THAN THE FEDERAL SETUP BECAUSE PEOPLE ARE ONLY EVER
# TREATED *OR* CONTROL; IN THE OTHER SETUP, PEOPLE ARE *BOTH*. HERE, PEOPLE STOPPED IN 2016 ARE NEVER
# CONSIDERED TREATED

# WE'LL START BY PULLING ALL THE PRE-TREATEMENT PERIOD STOPS FOR BOTH GROUPS TO MERGE BACK IN LATER
# pre13 <- filter(hills_stops, offense_date <= "2013-03-03")[,
#                                                            .(stop_count = .N),
#                                                            .(first_name, last_name, date_of_birth, offense_date)]  # using data.table syntax for speed
# pre13 <-  pre13[,
#                 .(stop_count = .N),
#                 .(first_name, last_name, date_of_birth)]  # using data.table syntax for speed
# 
# pre17 <- filter(hills_stops, offense_date <= "2017-03-05")[,
#                                                            .(stop_count = .N),
#                                                            .(first_name, last_name, date_of_birth, offense_date)]  # using data.table syntax for speed
# 
# pre17 <-  pre17[,
#                 .(stop_count = .N),
#                 .(first_name, last_name, date_of_birth)]  # using data.table syntax for speed
# 
# 
# ##############################
# 
# # FROM HERE IT'S RELATIVELY STRAIGHTFORWARD: WE NEED TO MAKE THOSE 4 GROUPS
# # TREATNEBT GROUP 1
# s13_15 <- filter(hills_stops, offense_date > "2013-03-03",
#                  offense_date <= "2015-03-03")[,
#                                                .(amount_paid = sum(amount_paid),
#                                                  civil = min(civil),
#                                                  tampa_pd = max(tampa_pd)),
#                                                .(first_name, last_name, date_of_birth, offense_date)]
# s13_15 <- s13_15[,
#                  .(stop_count = .N,
#                    last_date = max(offense_date),
#                    amount_paid = sum(amount_paid),
#                    civil = min(civil),
#                    tampa_pd = max(tampa_pd)),
#                  .(first_name, last_name, date_of_birth)] %>%
#   mutate(first_tr_year = as.Date("2015-03-03"))
# 
# s13_15 <- left_join(s13_15, pre13) %>% 
#   rename(pre_stops = stop_count)
# 
# # CONTROL GROUP 1
# s15_17 <- filter(hills_stops, offense_date > "2015-03-03",
#                  offense_date <= "2017-03-03")[,
#                                                .(amount_paid = sum(amount_paid),
#                                                  civil = min(civil),
#                                                  tampa_pd = max(tampa_pd)),
#                                                .(first_name, last_name, date_of_birth, offense_date)]
# s15_17 <- s15_17[,
#                  .(stop_count = .N,
#                    last_date = max(offense_date),
#                    amount_paid = sum(amount_paid),
#                    civil = min(civil),
#                    tampa_pd = max(tampa_pd)),
#                  .(first_name, last_name, date_of_birth)] %>%
#   mutate(first_tr_year = as.Date("2015-03-03"))
# 
# # POTENTIAL CONTROLS SHOULDNT HAVE BEEN STOPPED 2013-2015 SO WE NEED TO REMOVE THEM
# 
# s15_17 <- filter(s15_17,
#                  !(paste0(first_name, last_name, date_of_birth) %in%
#                      with(s13_15, paste0(first_name, last_name, date_of_birth))))
# 
# s15_17 <- left_join(s15_17, pre13) %>% 
#   rename(pre_stops = stop_count)
# ##########################
# 
# s17_19 <- filter(hills_stops, offense_date > "2017-03-05",
#                  offense_date <= "2019-03-05")[,
#                                                .(amount_paid = sum(amount_paid),
#                                                  civil = min(civil),
#                                                  tampa_pd = max(tampa_pd)),
#                                                .(first_name, last_name, date_of_birth, offense_date)]
# s17_19 <- s17_19[,
#                  .(stop_count = .N,
#                    last_date = max(offense_date),
#                    amount_paid = sum(amount_paid),
#                    civil = min(civil),
#                    tampa_pd = max(tampa_pd)),
#                  .(first_name, last_name, date_of_birth)] %>%
#   mutate(first_tr_year = as.Date("2019-03-05"))
# 
# s17_19 <- left_join(s17_19, pre17) %>% 
#   rename(pre_stops = stop_count)
# 
# # CONTROL GROUP 2
# s19_21 <- filter(hills_stops, offense_date > "2019-03-05",
#                  offense_date <= "2021-03-05")[,
#                                                .(amount_paid = sum(amount_paid),
#                                                  civil = min(civil),
#                                                  tampa_pd = max(tampa_pd)),
#                                                .(first_name, last_name, date_of_birth, offense_date)]
# s19_21 <- s19_21[,
#                  .(stop_count = .N,
#                    last_date = max(offense_date),
#                    amount_paid = sum(amount_paid),
#                    civil = min(civil),
#                    tampa_pd = max(tampa_pd)),
#                  .(first_name, last_name, date_of_birth)] %>%
#   mutate(first_tr_year = as.Date("2019-03-05"))
# 
# # POTENTIAL CONTROLS SHOULDNT HAVE BEEN STOPPED 2017-2019 SO WE NEED TO REMOVE THEM
# 
# s19_21 <- filter(s19_21,
#                  !(paste0(first_name, last_name, date_of_birth) %in%
#                      with(s17_19, paste0(first_name, last_name, date_of_birth))))
# 
# s19_21 <- left_join(s19_21, pre17) %>% 
#   rename(pre_stops = stop_count)
# ###########################
# 
# hills_stops_ll <- bind_rows(
#   s13_15,
#   s15_17,
#   s17_19,
#   s19_21,
# ) %>%
#   mutate(pre_stops = ifelse(is.na(pre_stops), 0, pre_stops))
# 
# saveRDS(hills_stops_ll, "temp/hills_stops_ll_multi_mayor.rds")

########################
hills_stops_ll <- readRDS("temp/hills_stops_ll_multi_mayor.rds")

hills_voters <- readRDS("temp/full_raw_coded_hills_w_bgs.rds") %>% 
  filter(tolower(city) == "tampa") %>% 
  group_by(name_first, name_last, birth_date) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

joined <- left_join(hills_voters, hills_stops_ll,
                    by = c("name_first" = "first_name",
                           "name_last" = "last_name",
                           "birth_date" = "date_of_birth")) %>% 
  mutate(age = 2021 - year(birth_date)) %>% 
  filter(!is.na(pre_stops))


######################## test permuting birth dates to check for false positives
# joined2 <- inner_join(hills_voters,
#                     select(hills_stops_ll, first_name,
#                            last_name, date_of_birth, count) %>%
#                       mutate(date_of_birth = date_of_birth + 35),
#                     by = c("name_first" = "first_name",
#                            "name_last" = "last_name",
#                            "birth_date" = "date_of_birth",
#                            "count"))
# 
# joined3 <- inner_join(hills_voters,
#                     select(hills_stops_ll, first_name,
#                            last_name, date_of_birth, count) %>%
#                       mutate(date_of_birth = date_of_birth - 35),
#                     by = c("name_first" = "first_name",
#                            "name_last" = "last_name",
#                            "birth_date" = "date_of_birth",
#                            "count"))
# 
# test <- data.table(dates = c("Good", "Plus", "Minus"),
#                    values = c(sum(!is.na(joined$fd)),
#                               nrow(joined2),
#                               nrow(joined3)))
# 
# saveRDS(test, "temp/plus_minus_35.rds")
######################################

joined <- joined %>% 
  mutate(pre_stops = ifelse(is.na(pre_stops), 0, pre_stops),
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
         white, black, latino, asian, male, dem, rep, age, civil,
         reg_date, pre_stops, amount_paid, name_first, name_last, birth_date, tampa_pd,
         v07, v11, v15, v19, latitude, longitude, first_tr_year) %>% 
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

saveRDS(match_data, "temp/hills_pre_match_mayor.rds")
