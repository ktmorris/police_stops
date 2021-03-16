vf_names <- make.names(fread("E:/rolls/florida/colnames.csv", header = F, sep = ",")$V1)
vf_names <- gsub("[.]", "_", tolower(vf_names))

vh_names <- make.names(fread("E:/rolls/florida/colnames_history.csv", header = F, sep = ",")$V1)
vh_names <- gsub("[.]", "_", tolower(vh_names))

#########################
v2012 <- fread("E:/rolls/florida/FL-2013-1/VoterExtract/HIL_20130203-ReSave.txt")
colnames(v2012) <- vf_names[1:37]
v2012 <- select(v2012,
                voter_id,
                name_last,
                name_first,
                residence_address_line_1,
                residence_address_line_2,
                city = residence_city,
                state = residence_state,
                zip = residence_zipcode,
                gender,
                race,
                birth_date,
                party_affiliation,
                registration_date)

roll <- v2012
#############################
v2014 <- fread("E:/rolls/florida/2014_12/VoterExtract/VoterExtract/VoterExtract/HIL_20141208.txt")
colnames(v2014) <- vf_names
v2014 <- select(v2014,
                voter_id,
                name_last,
                name_first,
                residence_address_line_1,
                residence_address_line_2,
                city = residence_city,
                state = residence_state,
                zip = residence_zipcode,
                gender,
                race,
                birth_date,
                party_affiliation,
                registration_date)

roll <- bind_rows(roll, filter(v2014, !(voter_id %in% roll$voter_id)))
#############################
v2016 <- fread("E:/rolls/florida/2016_12/20161206_VoterDetail/HIL_20161206.txt")
colnames(v2016) <- vf_names
v2016 <- select(v2016,
                voter_id,
                name_last,
                name_first,
                residence_address_line_1,
                residence_address_line_2,
                city = residence_city,
                state = residence_state,
                zip = residence_zipcode,
                gender,
                race,
                birth_date,
                party_affiliation,
                registration_date)
roll <- bind_rows(roll, filter(v2016, !(voter_id %in% roll$voter_id)))
#############################
v2018 <- fread("E:/rolls/florida/2018_12/Voter_Registration_20181211/20181211_VoterDetail/HIL_20181211.txt")
colnames(v2018) <- vf_names
v2018 <- select(v2018,
                voter_id,
                name_last,
                name_first,
                residence_address_line_1,
                residence_address_line_2,
                city = residence_city,
                state = residence_state,
                zip = residence_zipcode,
                gender,
                race,
                birth_date,
                party_affiliation,
                registration_date)
roll <- bind_rows(roll, filter(v2018, !(voter_id %in% roll$voter_id)))
#############################
v2019 <- fread("E:/rolls/florida/2019_08/Voter_Registration_20190813/20190813_VoterDetail/HIL_20190813.txt")
colnames(v2019) <- vf_names
v2019 <- select(v2019,
                voter_id,
                name_last,
                name_first,
                residence_address_line_1,
                residence_address_line_2,
                city = residence_city,
                state = residence_state,
                zip = residence_zipcode,
                gender,
                race,
                birth_date,
                party_affiliation,
                registration_date) %>% 
  mutate(latest_mayor = T)
roll <- bind_rows(roll, filter(v2019, !(voter_id %in% roll$voter_id)))
rm(v2012, v2014, v2016, v2018, v2019)
##################################################
hists <- rbindlist(lapply(c("E:/rolls/florida/FL-2013-1/VoterHistory_20130203/HIL_H_20130203-ReSave.txt",
                            "E:/rolls/florida/2014_12/VoterHistory/VoterHistory/VoterHistory/HIL_H_20141208.txt",
                            "E:/rolls/florida/2016_12/20161206_VoterHistory/HIL_H_20161206.txt",
                            "E:/rolls/florida/2018_12/Voter_History_20181211/20181211_VoterHistory/HIL_H_20181211.txt",
                            "E:/rolls/florida/2019_08/Voter_History_20190813/20190813_VoterHistory/HIL_H_20190813.txt"),
                          fread))



roll$v10 <- roll$voter_id %in% filter(hists, V3 =="11/02/2010")$V2
roll$v12 <- roll$voter_id %in% filter(hists, V3 =="11/06/2012")$V2
roll$v14 <- roll$voter_id %in% filter(hists, V3 =="11/04/2014")$V2
roll$v16 <- roll$voter_id %in% filter(hists, V3 =="11/08/2016")$V2
roll$v18 <- roll$voter_id %in% filter(hists, V3 =="11/06/2018")$V2
roll$v19 <- roll$voter_id %in% filter(hists, V3 =="03/05/2019")$V2
roll$v15 <- roll$voter_id %in% filter(hists, V3 =="03/03/2015")$V2
roll$v11 <- roll$voter_id %in% filter(hists, V3 =="03/01/2011")$V2
roll$v07 <- roll$voter_id %in% filter(hists, V3 =="03/06/2007")$V2

roll <- clean_streets(roll, vars = c("residence_address_line_1",
                                     "residence_address_line_2"))

roll <- geocode(roll)

saveRDS(roll, "temp/full_raw_coded_hills.rds")
#################################

roll <- readRDS("temp/full_raw_coded_hills.rds") %>%
  filter(match %in% c("Zip8", "Zip9")) %>%
  select(-match)

bgs <- readOGR("../regular_data/tl_2018_12_bg",
               "tl_2018_12_bg")

pings  <- SpatialPoints(roll[,c('longitude','latitude')], proj4string = bgs@proj4string)
roll$GEOID <- over(pings, bgs)$GEOID

roll <- roll %>% 
  mutate_at(vars(name_first,
                 name_last), ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, toupper(.)))) %>% 
  group_by(name_first, name_last, birth_date) %>% 
  mutate(count = row_number(),
         birth_date = as.Date(birth_date, "%m/%d/%Y"),
         age = 2020 - year(birth_date)) %>% 
  ungroup()

roll$pre <- roll$voter_id %in% v2012$voter_id
saveRDS(roll, "temp/full_raw_coded_hills_w_bgs.rds")

#######################################################################
