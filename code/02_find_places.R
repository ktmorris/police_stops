library(data.table)
library(tidyverse)
library(rgdal)

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


print(nrow(nat))

for(s in unique(nat$state)){
  print(s)
  if(s != "00"){
    if(!file.exists(paste0("temp/subs_places_", s, ".rds"))){
      if(file.exists(paste0("raw_data/tl_2018_", s, "_cousub/tl_2018_", s, "_cousub.shp"))){
        s <- str_pad(as.character(s), width = 2, side = "left", pad = "0")
        r <- filter(nat, state == s, !is.na(lat), !is.na(long))
        
        places <- readOGR(paste0("raw_data/tl_2018_", s, "_place"),
                          paste0("tl_2018_", s, "_place"))
        
        pings  <- SpatialPoints(select(r, long, lat), proj4string = places@proj4string)
        
        r$place<- over(pings, places)$GEOID
        
        
        subs <- readOGR(paste0("raw_data/tl_2018_", s, "_cousub"),
                        paste0("tl_2018_", s, "_cousub"))
        
        pings  <- SpatialPoints(select(r, long, lat), proj4string = subs@proj4string)
        
        r$county_s<- over(pings, subs)$GEOID
        
        saveRDS(select(r, LALVOTERID, place, county_s), paste0("temp/subs_places_", s, ".rds"))
      }
    }
  }
}

###########################
locs <- rbindlist(lapply(list.files("temp", full.names = T, pattern = "subs_places"), readRDS))

nat <- left_join(nat, locs)

saveRDS(nat, "temp/nat_with_places.rds")

# nat <- readRDS("temp/nat_with_places.rds")

cities <- readRDS("temp/cog_cities.rds")

nat <- nat %>% 
  mutate_at(vars(place, county_s), as.character) %>% 
  mutate(county_s = paste0(substring(county_s, 1, 2), substring(county_s, 6)),
         plasub = ifelse(place %in% cities$GEOID, place,
                         ifelse(county_s %in% cities$GEOID, county_s, NA)))

ll <- nat %>% 
  group_by(EthnicGroups_EthnicGroup1Desc, plasub, state) %>% 
  summarize(count = n(),
            to = mean(General_2018_11_06),
            to_old = mean(General_2016_11_08))


saveRDS(ll, "temp/city_to.rds")