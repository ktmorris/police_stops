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
#            offense_date) %>% 
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
#              offense_date) %>% 
#       mutate(type = "crim")
#     
#     return(ja)
#   }
# }))
# hills_stops <- bind_rows(hills_stops, hills_stops_cr)
# 
# saveRDS(hills_stops, "temp/hills_stops.rds")
# 
# hills_stops_ll <- hills_stops %>%
#   group_by(first_name, last_name, date_of_birth) %>%
#   summarize(fd = min(as.Date(offense_date, "%m/%d/%Y"), na.rm = T),
#             stop_count = n())
# 
# saveRDS(hills_stops_ll, "temp/hills_stops_ll.rds")

hills_stops_ll <- readRDS("temp/hills_stops_ll.rds") %>% 
  mutate(date_of_birth = as.Date(date_of_birth, "%m/%d/%Y"))
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
joined <- left_join(joined,
                    select(hills_stops_ll, first_name,
                           last_name, date_of_birth) %>% 
                      mutate(date_of_birth = date_of_birth + 35,
                             plus = 1),
                    by = c("Voters_FirstName" = "first_name",
                           "Voters_LastName" = "last_name",
                           "Voters_BirthDate" = "date_of_birth"))

joined <- left_join(joined,
                    select(hills_stops_ll, first_name,
                           last_name, date_of_birth) %>% 
                      mutate(date_of_birth = date_of_birth - 35,
                             minus = 1),
                    by = c("Voters_FirstName" = "first_name",
                           "Voters_LastName" = "last_name",
                           "Voters_BirthDate" = "date_of_birth"))

test <- data.table(dates = c("Good", "Plus", "Minus"),
                   values = c(sum(!is.na(joined$fd)),
                              sum(joined$plus, na.rm = T),
                              sum(joined$minus, na.rm = T)))
######################################

joined <- filter(joined,
                 fd >= "2010-11-03" | is.na(fd)) %>% 
  mutate(treated = !is.na(fd),
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
  select(LALVOTERID, GEOID, treated, fd,
         white, black, latino, asian, male, dem, rep,
         age = Voters_Age,
         reg_date, stop_count)


census <- readRDS("../regular_data/census_bgs_18.rds")

joined <- left_join(joined, census %>% 
                      select(median_income, some_college, unem, GEOID))

match_data <- joined %>% 
  mutate(reg_date = reg_date - as.Date("2000-01-01"))


match_data <- match_data[complete.cases(select(match_data, -fd, -stop_count)),]

saveRDS(match_data, "temp/hills_pre_match.rds")
########################################

ll <- match_data %>% 
  group_by(treated) %>% 
  summarize_at(vars(white, black, latino, asian,
                    male, dem, rep, age,
                    median_income, some_college, unem),
               mean)
