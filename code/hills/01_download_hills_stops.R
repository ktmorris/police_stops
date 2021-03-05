# hills_stops <- rbindlist(lapply(c(2003:2021), function(l){
#   print(l)
#   if(l != "J"){
#   if(!(file.exists(paste0("raw_data/hills_stops/civil_stops_", l, ".csv")))){
#     download.file(paste0("https://publicrec.hillsclerk.com/Traffic/Civil_Traffic_Name_Index_files/Civil_Traffic_Name_Index_", l, ".csv"),
#                   paste0("raw_data/hills_stops/civil_stops_", l, ".csv"))
#   }
# 
#   ja <- fread(paste0("raw_data/hills_stops/civil_stops_", l, ".csv"))
# 
#   colnames(ja) <- clean_names(ja)
# 
#   ja <- ja %>%
#     select(last_name, first_name, middle_name,
#            date_of_birth,
#            street = address_line_1,
#            city,
#            state,
#            zip = zip_code,
#            offense_date,
#            amount_paid) %>%
#     mutate(type = "civil")
# 
#   return(ja)
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
#       mutate(type = "crim")
# 
#     return(ja)
#   }
# }))
# hills_stops <- bind_rows(hills_stops, hills_stops_cr) %>% 
#   mutate(amount_paid = ifelse(is.na(amount_paid), 0, amount_paid))
# 
# saveRDS(hills_stops, "temp/hills_stops.rds")
# 
# hills_stops <- readRDS("temp/hills_stops.rds") %>%
#   mutate(offense_date = as.Date(offense_date, "%m/%d/%Y"),
#          date_of_birth = as.Date(date_of_birth, "%m/%d/%Y"))
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
# 
# hills_stops <- filter(hills_stops, amount_paid > 0)[,
#                            .(max_amount = max(amount_paid)),
#                            .(first_name, last_name, date_of_birth, offense_date)] %>%  # using data.table syntax for speed
#   mutate(hold = if_else(offense_date <= as.Date("2012-11-06"),
#                  as.Date("2099-12-31"), offense_date))
# 
# hills_stops_ll <- hills_stops[,
#                        .(stop_count = .N,
#                          pre_12 = sum(offense_date <= "2012-11-06"),
#                          fd = min(hold)),
#                        .(first_name, last_name, date_of_birth)]  # using data.table syntax for speed
# 
# hills_stops_ll <- left_join(hills_stops_ll, select(hills_stops, -hold),
#                             by = c("first_name", "last_name", "date_of_birth", "fd" = "offense_date"))
# 
# saveRDS(hills_stops_ll, "temp/hills_stops_ll.rds")
# 
hills_stops_ll <- readRDS("temp/hills_stops_ll.rds") %>% 
  mutate_at(vars(first_name,
                 last_name), ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, toupper(.)))) %>% 
  group_by(first_name, last_name, date_of_birth) %>% 
  mutate(count = row_number())

########################

hills_voters <- readRDS("temp/full_raw_coded_hills_w_bgs.rds")

joined <- left_join(hills_voters, hills_stops_ll,
                    by = c("name_first" = "first_name",
                           "name_last" = "last_name",
                           "birth_date" = "date_of_birth",
                           "count")) %>% 
  mutate(age = 2020 - year(birth_date))


######################## test permuting birth dates to check for false positives
joined2 <- inner_join(hills_voters,
                    select(hills_stops_ll, first_name,
                           last_name, date_of_birth, count) %>%
                      mutate(date_of_birth = date_of_birth + 35),
                    by = c("name_first" = "first_name",
                           "name_last" = "last_name",
                           "birth_date" = "date_of_birth",
                           "count"))

joined3 <- inner_join(hills_voters,
                    select(hills_stops_ll, first_name,
                           last_name, date_of_birth, count) %>%
                      mutate(date_of_birth = date_of_birth - 35),
                    by = c("name_first" = "first_name",
                           "name_last" = "last_name",
                           "birth_date" = "date_of_birth",
                           "count"))

test <- data.table(dates = c("Good", "Plus", "Minus"),
                   values = c(sum(!is.na(joined$fd)),
                              nrow(joined2),
                              nrow(joined3)))

saveRDS(test, "temp/plus_minus_35.rds")
######################################

joined <- joined %>% 
  mutate(fd = if_else(is.na(fd), as.Date("2099-12-31"), fd),
         stop_count = ifelse(is.na(stop_count), 0, stop_count),
         pre_12 = ifelse(is.na(pre_12), 0, pre_12),
         max_amount = ifelse(is.na(max_amount), 0, max_amount),
         white = race == 5,
         black = race == 3,
         latino = race == 4,
         asian = race == 2,
         male = gender == "M",
         dem = party_affiliation == "DEM",
         rep = party_affiliation == "REP",
         reg_date = as.Date(registration_date, "%m/%d/%Y")) %>% 
  select(voter_id, GEOID, fd,
         white, black, latino, asian, male, dem, rep, age,
         reg_date, pre_12, max_amount, name_first, name_last, birth_date,
         v10, v12, v14, v16, v18)

### remove people who were stopped, not fined
joined <- left_join(joined, readRDS("temp/stopped_no_fine.rds"),
                    by = c("name_first" = "first_name",
                           "name_last" = "last_name",
                           "birth_date" = "date_of_birth")) %>% 
  filter(is.na(exclude)) %>% 
  select(-name_first, -name_last, -birth_date, -exclude)


census <- readRDS("../regular_data/census_bgs_18.rds")

joined <- left_join(joined, census %>% 
                      select(median_income, some_college, unem, GEOID))

match_data <- joined %>% 
  mutate(reg_date = reg_date - as.Date("2000-01-01"))


match_data <- match_data[complete.cases(match_data),]

match_data$pre_12 <- ifelse(match_data$pre_12 > 10, 10,
                            match_data$pre_12)

saveRDS(select(match_data, -v18) %>% ungroup(), "temp/hills_pre_match.rds")
