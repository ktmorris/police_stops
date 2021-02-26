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
#          date_of_birth = as.Date(date_of_birth, "%m/%d/%Y")) %>% 
#   filter(offense_date <= as.Date("2018-11-06"))
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
# hills_stops_ll <- readRDS("temp/hills_stops_ll.rds")

########################

hills_voters <- readRDS("raw_data/fl_l2_hills/fl_l2_data_hills.rds")


hills_voters <- hills_voters %>% 
  mutate_at(vars(Voters_FirstName,
                 Voters_LastName), toupper) %>% 
  group_by(Voters_FirstName, Voters_LastName, Voters_BirthDate) %>% 
  mutate(count = row_number(),
         Voters_BirthDate = as.Date(Voters_BirthDate, "%m/%d/%Y")) %>% 
  ungroup()


joined <- left_join(hills_voters, hills_stops_ll,
                    by = c("Voters_FirstName" = "first_name",
                           "Voters_LastName" = "last_name",
                           "Voters_BirthDate" = "date_of_birth"))


######################## test permuting birth dates to check for false positives
# joined2 <- inner_join(hills_voters,
#                     select(hills_stops_ll, first_name,
#                            last_name, date_of_birth) %>%
#                       mutate(date_of_birth = date_of_birth + 35),
#                     by = c("Voters_FirstName" = "first_name",
#                            "Voters_LastName" = "last_name",
#                            "Voters_BirthDate" = "date_of_birth"))
# 
# joined3 <- inner_join(hills_voters,
#                     select(hills_stops_ll, first_name,
#                            last_name, date_of_birth) %>%
#                       mutate(date_of_birth = date_of_birth - 35),
#                     by = c("Voters_FirstName" = "first_name",
#                            "Voters_LastName" = "last_name",
#                            "Voters_BirthDate" = "date_of_birth"))
# 
# test <- data.table(dates = c("Good", "Plus", "Minus"),
#                    values = c(sum(!is.na(joined$fd)),
#                               nrow(joined2),
#                               nrow(joined3)))
######################################

joined <- joined %>% 
  mutate(fd = if_else(is.na(fd), as.Date("2099-12-31"), fd),
         stop_count = ifelse(is.na(stop_count), 0, stop_count),
         pre_12 = ifelse(is.na(pre_12), 0, pre_12),
         max_amount = ifelse(is.na(max_amount), 0, max_amount),
         GEOID = paste0("12",
                        str_pad(Voters_FIPS, pad = "0", width = 3, side = "left"),
                        str_pad(Residence_Addresses_CensusTract, width = 6, pad = "0", side = "left"),
                        Residence_Addresses_CensusBlockGroup),
         white = EthnicGroups_EthnicGroup1Desc == "European",
         black = EthnicGroups_EthnicGroup1Desc == "Likely African-American",
         latino = EthnicGroups_EthnicGroup1Desc == "Hispanic and Portuguese",
         asian = EthnicGroups_EthnicGroup1Desc == "East and South Asian",
         male = Voters_Gender == "M",
         dem = Parties_Description == "Democratic",
         rep = Parties_Description == "Republican",
         reg_date = as.Date(Voters_OfficialRegDate, "%m/%d/%Y")) %>% 
  select(LALVOTERID, GEOID, fd,
         white, black, latino, asian, male, dem, rep,
         age = Voters_Age,
         reg_date, pre_12, max_amount, Voters_FirstName, Voters_LastName, Voters_BirthDate)

### remove people who were stopped, not fined
joined <- left_join(joined, readRDS("temp/stopped_no_fine.rds"),
                    by = c("Voters_FirstName" = "first_name",
                           "Voters_LastName" = "last_name",
                           "Voters_BirthDate" = "date_of_birth")) %>% 
  filter(is.na(exclude)) %>% 
  select(-Voters_FirstName, -Voters_LastName, -Voters_BirthDate, -exclude)

### drop if stopped in period but no fee

census <- readRDS("../regular_data/census_bgs_18.rds")

joined <- left_join(joined, census %>% 
                      select(median_income, some_college, unem, GEOID))

match_data <- joined %>% 
  mutate(reg_date = reg_date - as.Date("2000-01-01"))

############# read in pre-history

hist <- readRDS("raw_data/fl_l2_hills/fl_l2_history_hills.rds") %>% 
  select(LALVOTERID, v10 = General_2010_11_02,
         v12 = General_2012_11_06,
         v14 = General_2014_11_04,
         v16 = General_2016_11_08)

match_data <- left_join(match_data, hist) %>% 
  mutate_at(vars(v10, v12, v14, v16), ~ ifelse(is.na(.) | . == "", 0, 1))


match_data <- match_data[complete.cases(match_data),]

match_data$pre_12 <- ifelse(match_data$pre_12 > 10, 10,
                            match_data$pre_12)

saveRDS(match_data, "temp/hills_pre_match.rds")
