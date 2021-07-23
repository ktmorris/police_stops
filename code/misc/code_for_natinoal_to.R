library(RSQLite)
library(rgdal)
library(data.table)
library(tidyverse)

db_rolls <- dbConnect(SQLite(), "./data/national_file.db")

fips_codes <- fread("./data/fips_codes.csv")

dbListTables(db_rolls)

national_file <- rbindlist(lapply(dbListTables(db_rolls), function(s){
  scode <- substring(s, 1, 2)
  code_good <- str_pad(as.character(unique(filter(fips_codes, state == scode)$state_code)),
                       pad = "0", side = "left", width = 2)
  d <- dbGetQuery(db_rolls, paste0("select LALVOTERID,
                                           Voters_FIPS,
                                           Residence_Addresses_CensusTract,
                                           Residence_Addresses_CensusBlockGroup,
                                           Voters_Age,
                                           Parties_Description,
                                           EthnicGroups_EthnicGroup1Desc,
                                           Voters_Gender,
                                           State_Senate_District,
                                           State_House_District,
                                           State_Legislative_District,
                                           Residence_Addresses_Latitude,
                                           Residence_Addresses_Longitude,
                                           Voters_OfficialRegDate,
                                           state from [", s, "]")) %>% 
    mutate(State_Senate_District = ifelse(State_Senate_District == "",
                                          State_Legislative_District,
                                          State_Senate_District),
           GEOID = paste0(code_good, str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                          str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                          Residence_Addresses_CensusBlockGroup)) %>% 
    select(-State_Legislative_District, -Voters_FIPS, -Residence_Addresses_CensusTract,
           -Residence_Addresses_CensusBlockGroup)
}))

db_history <- dbConnect(SQLite(), "./data/national_file_history.db")
national_history <- rbindlist(lapply(dbListTables(db_history), function(s){             
  d <- dbGetQuery(db_history, paste0("select LALVOTERID,
                                   General_2018_11_06,
                                   General_2016_11_08,
                                   General_2014_11_04,
                                   General_2012_11_06,
                                   General_2010_11_02
                                   from ", s)) %>% 
    mutate_at(vars(General_2018_11_06,
                   General_2016_11_08,
                   General_2014_11_04,
                   General_2012_11_06,
                   General_2010_11_02),  ~ ifelse(. == "Y", 1, 0)) %>% 
    mutate(state = substring(s, 1, 2))
}))

national_file <- inner_join(national_file, national_history, by = c("LALVOTERID", "state"))
dbDisconnect(db_history, db_rolls)
rm(db_history, db_rolls, national_history)

# bg_census <- rbindlist(lapply(unique(filter(fips_codes, as.integer(state_code) <= 56)$state_code), function(s){
#   income <- get_acs(geography = "block group",
#                     variables = c(medincome = "B19013_001"),
#                     year = 2018, state = s) %>%
#     dplyr::select(-variable, -moe, -NAME) %>%
#     dplyr::rename(median_income = estimate)
#   
#   education <- get_acs(geography = "block group",
#                        variables = c("B15002_012",
#                                      "B15002_013",
#                                      "B15002_014",
#                                      "B15002_015",
#                                      "B15002_016",
#                                      "B15002_017",
#                                      "B15002_028",
#                                      "B15002_029",
#                                      "B15002_030",
#                                      "B15002_031",
#                                      "B15002_032",
#                                      "B15002_033",
#                                      "B15002_034",
#                                      "B15002_035"),
#                        summary_var = "B15002_001",
#                        state = s, year = 2018) %>%
#     dplyr::group_by(GEOID, NAME) %>%
#     dplyr::summarize(some_college = sum(estimate / summary_est)) %>% 
#     ungroup() %>% 
#     select(-NAME)
#   
#   ll <- full_join(education, income)
#   return(ll)
# }))
# 
# saveRDS(bg_census, "./temp/bg_data.rds")


national_file <- left_join(national_file, readRDS("./temp/bg_data.rds"), by = "GEOID")

saveRDS(national_file, "./temp/national_merged.rds")

saveRDS(national_file %>% 
          group_by(state, State_Senate_District) %>% 
          sample_frac(0.01) %>% 
          ungroup(),
        "./temp/1percent.rds")

low_level <- national_file %>% 
  group_by(state, State_Senate_District) %>% 
  summarize(voted_18 = mean(General_2018_11_06),
            voted_16 = mean(General_2016_11_08),
            ballots_18 = sum(General_2018_11_06),
            ballots_16 = sum(General_2016_11_08))

saveRDS(low_level, "./temp/state_senate_turnout.rds")


fc <- fread("raw_data/fips_codes.csv") %>%
  mutate(state_code = str_pad(as.character(state_code), side = "left", width = 2, pad = "0"))


nat <- readRDS("./temp/national_merged.rds")
nat <- left_join(nat, fc) %>%
  select(-state) %>%
  rename(state = state_code)

nat <- rename(nat, lat = Residence_Addresses_Latitude, long = Residence_Addresses_Longitude) %>%
  mutate(lat = as.numeric(lat),
         long = as.numeric(long)) %>%
  ungroup()
# 
# print(nrow(nat))
# print(colnames(nat))
# 
# 
# for(s in unique(nat$state)){
#   print(s)
#   if(s != "00"){
#     if(!file.exists(paste0("temp/subs_places_", s, ".rds"))){
#       if(file.exists(paste0("raw_data/tl_2018_", s, "_cousub/tl_2018_", s, "_cousub.shp"))){
#         s <- str_pad(as.character(s), width = 2, side = "left", pad = "0")
#         r <- filter(nat, state == s, !is.na(lat), !is.na(long))
#         
#         places <- readOGR(paste0("raw_data/tl_2018_", s, "_place"),
#                           paste0("tl_2018_", s, "_place"))
#         
#         pings  <- SpatialPoints(select(r, long, lat), proj4string = places@proj4string)
#         
#         r$place<- over(pings, places)$GEOID
#         
#         
#         subs <- readOGR(paste0("raw_data/tl_2018_", s, "_cousub"),
#                         paste0("tl_2018_", s, "_cousub"))
#         
#         pings  <- SpatialPoints(select(r, long, lat), proj4string = subs@proj4string)
#         
#         r$county_s<- over(pings, subs)$GEOID
#         
#         saveRDS(select(r, LALVOTERID, place, county_s), paste0("temp/subs_places_", s, ".rds"))
#       }
#     }
#   }
# }
# 
# ###########################
locs <- rbindlist(lapply(list.files("temp", full.names = T, pattern = "subs_places"), readRDS))

nat <- left_join(nat, locs)

nat <- nat %>%
  mutate_at(vars(place, county_s), as.character) %>%
  mutate(county_s = paste0(substring(county_s, 1, 2), substring(county_s, 6)))

ll <- nat %>%
  group_by(EthnicGroups_EthnicGroup1Desc, state) %>%
  summarize(count = n(),
            to_18 = mean(General_2018_11_06),
            to_16 = mean(General_2016_11_08),
            to_14 = mean(General_2014_11_04),
            to_12 = mean(General_2012_11_06),
            to_10 = mean(General_2010_11_02))

saveRDS(ll, "temp/state_to.rds")


ll <- nat %>%
  group_by(EthnicGroups_EthnicGroup1Desc, county_s, state) %>%
  summarize(count = n(),
            to_18 = mean(General_2018_11_06),
            to_16 = mean(General_2016_11_08),
            to_14 = mean(General_2014_11_04),
            to_12 = mean(General_2012_11_06),
            to_10 = mean(General_2010_11_02))


saveRDS(ll, "temp/county_s_to.rds")

ll <- nat %>%
  group_by(EthnicGroups_EthnicGroup1Desc, place, state) %>%
  summarize(count = n(),
            to_18 = mean(General_2018_11_06),
            to_16 = mean(General_2016_11_08),
            to_14 = mean(General_2014_11_04),
            to_12 = mean(General_2012_11_06),
            to_10 = mean(General_2010_11_02))


saveRDS(ll, "temp/place_to.rds")
# 
# ll <- filter(nat,
#              as.Date(Voters_OfficialRegDate, "%m/%d/%Y") <= "2010-11-02") %>% 
#   group_by(EthnicGroups_EthnicGroup1Desc, plasub, state) %>% 
#   summarize(count = n(),
#             to_18 = mean(General_2018_11_06),
#             to_16 = mean(General_2016_11_08),
#             to_14 = mean(General_2014_11_04),
#             to_12 = mean(General_2012_11_06),
#             to_10 = mean(General_2010_11_02))
# 
# 
# saveRDS(ll, "temp/city_to_early_reg.rds")
# 
# ll <- filter(nat,
#              as.Date(Voters_OfficialRegDate, "%m/%d/%Y") <= "2014-11-04") %>% 
#   group_by(EthnicGroups_EthnicGroup1Desc, plasub, state) %>% 
#   summarize(count = n(),
#             to_18 = mean(General_2018_11_06),
#             to_16 = mean(General_2016_11_08),
#             to_14 = mean(General_2014_11_04),
#             to_12 = mean(General_2012_11_06),
#             to_10 = mean(General_2010_11_02))
# 
# 
# saveRDS(ll, "temp/city_to_14_reg.rds")
# 
