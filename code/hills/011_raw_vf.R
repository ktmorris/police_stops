vf_names <- make.names(fread("E:/rolls/florida/colnames.csv", header = F, sep = ",")$V1)
vf_names <- gsub("[.]", "_", tolower(vf_names))

vh_names <- make.names(fread("E:/rolls/florida/colnames_history.csv", header = F, sep = ",")$V1)
vh_names <- gsub("[.]", "_", tolower(vh_names))
#############################
hist19 <- fread("E:/rolls/florida/2019_08/Voter_History_20190813/20190813_VoterHistory/HIL_H_20190813.txt")
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

v2019$v10 <- v2019$voter_id %in% filter(hist19, V3 =="11/02/2010")$V2
v2019$v12 <- v2019$voter_id %in% filter(hist19, V3 =="11/06/2012")$V2
v2019$v14 <- v2019$voter_id %in% filter(hist19, V3 =="11/04/2014")$V2
v2019$v16 <- v2019$voter_id %in% filter(hist19, V3 =="11/08/2016")$V2
v2019$v18 <- v2019$voter_id %in% filter(hist19, V3 =="11/06/2018")$V2

v2019$v19 <- v2019$voter_id %in% filter(hist19, V3 =="03/05/2019")$V2
v2019$v15 <- v2019$voter_id %in% filter(hist19, V3 =="03/03/2015")$V2
v2019$v11 <- v2019$voter_id %in% filter(hist19, V3 =="03/01/2011")$V2
v2019$v07 <- v2019$voter_id %in% filter(hist19, V3 =="03/06/2007")$V2
rm(hist19)
#############################
hist18 <- fread("E:/rolls/florida/2018_12/Voter_History_20181211/20181211_VoterHistory/HIL_H_20181211.txt")
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

v2018$v10 <- v2018$voter_id %in% filter(hist18, V3 =="11/02/2010")$V2
v2018$v12 <- v2018$voter_id %in% filter(hist18, V3 =="11/06/2012")$V2
v2018$v14 <- v2018$voter_id %in% filter(hist18, V3 =="11/04/2014")$V2
v2018$v16 <- v2018$voter_id %in% filter(hist18, V3 =="11/08/2016")$V2
v2018$v18 <- v2018$voter_id %in% filter(hist18, V3 =="11/06/2018")$V2

v2018$v19 <- 0
v2018$v15 <- v2018$voter_id %in% filter(hist18, V3 =="03/03/2015")$V2
v2018$v11 <- v2018$voter_id %in% filter(hist18, V3 =="03/01/2011")$V2
v2018$v07 <- v2018$voter_id %in% filter(hist18, V3 =="03/06/2007")$V2

v2018 <- filter(v2018, !(voter_id %in% v2019$voter_id))
rm(hist18)
#############################
hist16 <- fread("E:/rolls/florida/2016_12/20161206_VoterHistory/HIL_H_20161206.txt")
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
v2016$v10 <- v2016$voter_id %in% filter(hist16, V3 =="11/02/2010")$V2
v2016$v12 <- v2016$voter_id %in% filter(hist16, V3 =="11/06/2012")$V2
v2016$v14 <- v2016$voter_id %in% filter(hist16, V3 =="11/04/2014")$V2
v2016$v16 <- v2016$voter_id %in% filter(hist16, V3 =="11/08/2016")$V2
v2016$v18 <- 0

v2016$v19 <- 0
v2016$v15 <- v2016$voter_id %in% filter(hist16, V3 =="03/03/2015")$V2
v2016$v11 <- v2016$voter_id %in% filter(hist16, V3 =="03/01/2011")$V2
v2016$v07 <- v2016$voter_id %in% filter(hist16, V3 =="03/06/2007")$V2

v2016 <- filter(v2016, !(voter_id %in% c(v2019$voter_id, v2018$voter_id)))
rm(hist16)
#############################
hist14 <- fread("E:/rolls/florida/2014_12/VoterHistory/VoterHistory/VoterHistory/HIL_H_20141208.txt")
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

v2014$v10 <- v2014$voter_id %in% filter(hist14, V3 =="11/02/2010")$V2
v2014$v12 <- v2014$voter_id %in% filter(hist14, V3 =="11/06/2012")$V2
v2014$v14 <- v2014$voter_id %in% filter(hist14, V3 =="11/04/2014")$V2
v2014$v16 <- 0
v2014$v18 <- 0

v2014$v19 <- 0
v2014$v15 <- 0
v2014$v11 <- v2014$voter_id %in% filter(hist14, V3 =="03/01/2011")$V2
v2014$v07 <- v2014$voter_id %in% filter(hist14, V3 =="03/06/2007")$V2

v2014 <- filter(v2014, !(voter_id %in% c(v2019$voter_id, v2018$voter_id, v2016$voter_id)))
rm(hist14)
#############################
hist12 <- fread("E:/rolls/florida/FL-2013-1/VoterHistory_20130203/HIL_H_20130203-ReSave.txt")
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
v2012$v10 <- v2012$voter_id %in% filter(hist12, V3 =="11/02/2010")$V2
v2012$v12 <- v2012$voter_id %in% filter(hist12, V3 =="11/06/2012")$V2
v2012$v14 <- 0
v2012$v16 <- 0
v2012$v18 <- 0

v2012$v19 <- 0
v2012$v15 <- 0
v2012$v11 <- v2012$voter_id %in% filter(hist12, V3 =="03/01/2011")$V2
v2012$v07 <- v2012$voter_id %in% filter(hist12, V3 =="03/06/2007")$V2

v2012 <- filter(v2012, !(voter_id %in% c(v2019$voter_id, v2018$voter_id, v2016$voter_id, v2014$voter_id)))
rm(hist12)
# #########################
# 
# full_file <- bind_rows(v2012, v2014, v2016, v2018, v2019)
# 
# full_file <- clean_streets(full_file, vars = c("residence_address_line_1",
#                                                "residence_address_line_2"))
# 
# 
# full_file <- geocode(full_file)
# 
# saveRDS(full_file, "temp/full_raw_coded_hills.rds")
# 
full_file <- readRDS("temp/full_raw_coded_hills.rds") %>%
  filter(match %in% c("Zip8", "Zip9")) %>%
  select(-match)

bgs <- readOGR("../regular_data/tl_2018_12_bg",
               "tl_2018_12_bg")

pings  <- SpatialPoints(full_file[,c('longitude','latitude')], proj4string = bgs@proj4string)
full_file$GEOID <- over(pings, bgs)$GEOID

full_file <- full_file %>% 
  mutate_at(vars(name_first,
                 name_last), ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, toupper(.)))) %>% 
  group_by(name_first, name_last, birth_date) %>% 
  mutate(count = row_number(),
         birth_date = as.Date(birth_date, "%m/%d/%Y"),
         age = 2020 - year(birth_date)) %>% 
  ungroup()

saveRDS(full_file, "temp/full_raw_coded_hills_w_bgs.rds")

#######################################################################
