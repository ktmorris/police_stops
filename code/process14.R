files <- list.files("D:/national/post_2014", full.names = T, pattern = "*.zip")


for(f in files){
  f2 <- substring(f, 1, nchar(f) - 4)
  state <- substring(f, 37, 38)
  print(state)
  if(!dir.exists(f2)){
    unzip(f, exdir = f2)
  }else{
    print("Already Unzipped")
  }
  if(!file.exists(paste0("temp/", state, "_to_14_block.rds"))){
    l <- list.files(f2, full.names = T, pattern = "*.tab")
    k <- read.csv.sql(l, sep = "\t",
                      sql = "select LALVOTERID, Voters_FIPS,
                             Residence_Addresses_AddressLine, 
                             Residence_Addresses_ExtraAddressLine, 
                             Residence_Addresses_City, 
                             Residence_Addresses_State,
                             Residence_Addresses_CensusTract,
                             Residence_Addresses_CensusBlockGroup,
                             Residence_Addresses_CensusBlock,
                             EthnicGroups_EthnicGroup1Desc,
                             [General_2014.11.04] from file") %>%
      mutate(state = state)
    
    saveRDS(filter(k,
                   Residence_Addresses_CensusTract == "" |
                     is.na(Residence_Addresses_CensusTract) | 
                     Residence_Addresses_CensusTract == 0), paste0("temp/", state, "_to_14_no_block.rds"))
    
    saveRDS(filter(k,
                   Residence_Addresses_CensusTract != "" &
                     !is.na(Residence_Addresses_CensusTract) & 
                     Residence_Addresses_CensusTract != 0), paste0("temp/", state, "_to_14_block.rds"))
    print(state)
    print(mean(k$Residence_Addresses_CensusTract == "" | is.na(k$Residence_Addresses_CensusTract) | k$Residence_Addresses_CensusTract == 0))
  }
}
##############################
files <- list.files("temp", pattern = "*to_14_no_block.rds", full.names = T)

db18 <- dbConnect(SQLite(), "D:/national_file.db")

for(f in files){
  s <- substring(f, 6, 7)
  if(file.exists(paste0("temp/cleaned_", s, "_2014.rds"))){
    print(paste0("Missing Blocks already merged with 2018 Addresses in ", s))
  }else{
    print(paste0("Merging missing blocks with 2018 Addresses in ", s))
    
    dat <- readRDS(f) %>% 
      select(-Residence_Addresses_CensusTract,
             -Residence_Addresses_CensusBlockGroup,
             -Residence_Addresses_CensusBlock) %>% 
      mutate(Residence_Addresses_AddressLine = tolower(str_replace_all(Residence_Addresses_AddressLine,"[^[:graph:]]", " ")))
    
    lu <- dbGetQuery(db18, paste0("select Voters_FIPS,
                             Residence_Addresses_AddressLine, 
                             Residence_Addresses_City, 
                             Residence_Addresses_State,
                             Residence_Addresses_CensusTract,
                             Residence_Addresses_CensusBlockGroup,
                             Residence_Addresses_CensusBlock from [", s, "]")) %>%
      
      mutate(Residence_Addresses_AddressLine = tolower(str_replace_all(Residence_Addresses_AddressLine,"[^[:graph:]]", " "))) %>% 
      distinct()
    
    dat <- left_join(dat, lu)
    print(nrow(filter(dat, !is.na(Residence_Addresses_CensusTract), Residence_Addresses_CensusTract != 0)) / nrow(dat))
    saveRDS(dat, paste0("temp/cleaned_", s, "_2014.rds"))
  }
}

##############################
places <- rbindlist(lapply(list.files("raw_data/2014_lookups", pattern = "block_place*", full.names = T), fread)) %>% 
  mutate(block = paste0(county, tract, block),
         block = gsub("[.]", "", block),
         place = paste0(state, placefp14)) %>% 
  select(block, place)

county_subs <- rbindlist(lapply(list.files("raw_data/2014_lookups", pattern = "block_county*", full.names = T), fread)) %>% 
  mutate(block = paste0(county, tract, block),
         block = gsub("[.]", "", block),
         cousub = paste0(substring(county, 1, 2), cousubfp14)) %>% 
  select(block, cousub) %>% 
  distinct()


files <- list.files("temp/", pattern = "*_to_14_block.rds", full.names = T)

states <- gsub("temp/|_to_14_block.rds", "", files)

for(s in states){
  print(s)
  scode <- filter(fips_codes, state == s)[1, "state_code"]
  if(!file.exists(paste0("temp/", s, "_county_subs_14.rds"))){
    j <- bind_rows(
      readRDS(paste0("temp/", s, "_to_14_block.rds")) %>% 
        mutate_at(vars(Residence_Addresses_CensusTract,
                       Residence_Addresses_CensusBlockGroup,
                       Residence_Addresses_CensusBlock), as.integer),
      readRDS(paste0("temp/cleaned_", s, "_2014.rds")) %>% 
        mutate_at(vars(Residence_Addresses_CensusTract,
                       Residence_Addresses_CensusBlockGroup,
                       Residence_Addresses_CensusBlock), as.integer)
    )%>% 
      mutate(block = paste0(scode,
                            str_pad(Voters_FIPS, side = "left", width = 3, pad = "0"),
                            str_pad(Residence_Addresses_CensusTract, side = "left", width = 6, pad = "0"),
                            str_pad(Residence_Addresses_CensusBlock, side = "left", width = 4, pad = "0")))
    
    j <- left_join(j, places)
    j <- left_join(j, county_subs)
    
    cousubs_j <- j %>% 
      group_by(cousub, EthnicGroups_EthnicGroup1Desc) %>% 
      summarize(ballots_14 = sum(!is.na(General_2014.11.04) & General_2014.11.04 != "" & General_2014.11.04 != "N"),
                voters_14 = n())
    
    places_j <- j %>% 
      group_by(place, EthnicGroups_EthnicGroup1Desc) %>% 
      summarize(ballots_14 = sum(!is.na(General_2014.11.04) & General_2014.11.04 != "" & General_2014.11.04 != "N"),
                voters_14 = n())
    
    saveRDS(cousubs_j, paste0("temp/", s, "_county_subs_14.rds"))
    saveRDS(places_j, paste0("temp/", s, "_places_14.rds")) 
  }
}

full_places <- rbindlist(lapply(list.files("temp/", pattern = "*_places_14", full.names = T), readRDS))
saveRDS(full_places, "temp/places_14_national.rds")

full_cs <- rbindlist(lapply(list.files("temp/", pattern = "*_county_subs_14.rds", full.names = T), readRDS))
saveRDS(full_cs, "temp/countysubs_14_national.rds")

################################################

files <- list.files("temp/", pattern = "*_to_14_block.rds", full.names = T)

states <- gsub("temp/|_to_14_block.rds", "", files)

share_okay <- rbindlist(lapply(states, function(s){
  j <- bind_rows(
    readRDS(paste0("temp/", s, "_to_14_block.rds")) %>% 
      mutate_at(vars(Residence_Addresses_CensusTract,
                     Residence_Addresses_CensusBlockGroup,
                     Residence_Addresses_CensusBlock), as.integer),
    readRDS(paste0("temp/cleaned_", s, "_2014.rds")) %>% 
      mutate_at(vars(Residence_Addresses_CensusTract,
                     Residence_Addresses_CensusBlockGroup,
                     Residence_Addresses_CensusBlock), as.integer)
  ) %>% 
    summarize(share_good = mean(nchar(Residence_Addresses_CensusTract) > 1 & !is.na(Residence_Addresses_CensusTract)),
              count = n()) %>% 
    mutate(state = s)
}))

weighted.mean(share_okay$share_good, share_okay$count)
##################################################

db18 <- dbConnect(SQLite(), "D:/national_file.db")

share_okay_18 <- rbindlist(lapply(dbListTables(db18), function(s){
  dbGetQuery(db18, paste0("select Residence_Addresses_Latitude, state from [", s, "]")) %>% 
    group_by(state) %>% 
    summarize(share_good = mean(Residence_Addresses_Latitude != 0 & !is.na(Residence_Addresses_Latitude)),
              count = n())
}))

weighted.mean(share_okay_18$share_good, share_okay_18$count)
