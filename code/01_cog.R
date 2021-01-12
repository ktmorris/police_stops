d <- read_fwf("raw_data/2017_individual_unit_file/2017FinEstDAT_09102020modp_pu.txt",
              col_positions = fwf_widths(c(14, 3, 12, 4, 1),
                                         col_names = c("place_id",
                                                       "item_code",
                                                       "amount",
                                                       "year",
                                                       "imputation")),
              col_types = "ccccc")


police <- d %>% 
  filter(item_code %in% c("E62", "F62", "G62", "I62", "L62", "M62", "N62", "O62", "P62", "R62",
                          "E25", "I25", "F25", "G25", "K25")) %>% 
  filter(as.numeric(amount) > 0)


info <- read_fwf("raw_data/2017_individual_unit_file/Fin_GID_2017.txt",
                 col_positions = fwf_widths(c(14, 64, 35, 2, 3, 5, 9, 2, 7, 2, 2, 2, 4, 2)),
                 col_types = "cccccccccccccc")

d <- left_join(d, select(info,
                         X1,
                         population = X7,
                         fips = X6,
                         state = X4,
                         name = X2), by = c("place_id" = "X1")) %>% 
  rename(pop_cog = population)

# 
# c <- get_basic_census_stats("place", 2018)
# 
# sd <- rbindlist(lapply(unique(filter(fips_codes, as.integer(state_code) < 60)$state_code), function(s){
#   if(!file.exists(paste0("temp/census_sub_", s, ".rds"))){
#     j <- get_basic_census_stats("county subdivision", 2018, state = s)
#     saveRDS(j, paste0("temp/census_sub_", s, ".rds"))
#   }else{
#     j <- readRDS(paste0("temp/census_sub_", s, ".rds"))
#   }
#   return(j)
# })) %>% 
#   mutate(GEOID = paste0(substring(GEOID, 1, 2), substring(GEOID, 6)))
# 
# c2 <- rbind(c, sd)
# 
# saveRDS(c2, "temp/census_data.rds")

c2 <- readRDS("temp/census_data.rds")

cities <- inner_join(mutate(d, GEOID = paste0(state, fips)), c2, by = "GEOID") %>% 
  filter(item_code == "U30",
         place_id %in% police$place_id,
         as.integer(pop_cog) > 2500) %>% 
  mutate(dper = as.numeric(amount)*1000 / as.numeric(population),
         lndper = log(dper + 1),
         lnbl = log(nh_black + 1))

saveRDS(cities, "temp/cog_cities.rds")

ggplot(filter(cities, dper > 0),
       aes(x = nh_black, y = dper)) + geom_point() + theme_bc() + geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent, breaks = c(0.01, 0.05, 0.1, 0.25, 0.5, 1), trans = "log") +
  scale_y_log10(labels = scales::dollar) +
  labs(y = "Dollars in Fines per Resident",
       x = "Share Non-Hispanic Black")
#########################################################
