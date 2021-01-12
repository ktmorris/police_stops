library(data.table)
library(tidyverse)
library(kevostools)
library(rgdal)

nat <- readRDS("./temp/national_merged.rds")

locs <- rbindlist(lapply(list.files("temp", full.names = T, pattern = "subs_places"), readRDS))

nat <- left_join(nat, locs)

saveRDS(nat, "temp/nat_with_places.rds")

cities <- readRDS("temp/cog_cities.rds")

nat <- nat %>% 
  mutate_at(vars(place, county_s), as.character) %>% 
  mutate(plasub = ifelse(place %in% cities$GEOID, place,
                         ifelse(county_s %in% cities$GEOID, county_s, NA)))


ll <- nat %>% 
  group_by(EthnicGroups_EthnicGroup1Desc, plasub, state) %>% 
  summarize(count = n(),
            to = mean(General_2018_11_06),
            to_old = mean(General_2016_11_08))

ll2 <- nat %>% 
  group_by(plasub, state) %>% 
  summarize(count = n(),
            to = mean(General_2018_11_06),
            to_old = mean(General_2016_11_08)) %>% 
  mutate(EthnicGroups_EthnicGroup1Desc = "overall")

ll3 <- nat %>% 
  filter(EthnicGroups_EthnicGroup1Desc != "European") %>% 
  group_by(plasub, state) %>% 
  summarize(count = n(),
            to = mean(General_2018_11_06),
            to_old = mean(General_2016_11_08)) %>% 
  mutate(EthnicGroups_EthnicGroup1Desc = "nonwhite")

ll <- bind_rows(ll, ll2, ll3)

ll <- pivot_wider(ll, id_cols = plasub, names_from = "EthnicGroups_EthnicGroup1Desc",
                  values_from = c("count", "to", "to_old"))

colnames(ll) <- clean_names(ll)

saveRDS(ll, "temp/city_to.rds")