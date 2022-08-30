

lapply(c(1:23), function(i){
  if(!file.exists(paste0("temp/matches_rob_", i, ".rds"))){
    pre <- readRDS("temp/real_pre_match_hills.rds") %>% 
      filter(((last_date >= (ymd(as.Date("2014-11-04")) %m+% months(-i))) &
                (last_date <= ymd(as.Date("2014-11-04")) %m+% months(i)) & (first_tr_year == 1)) |
               ((last_date >= (ymd(as.Date("2016-11-08")) %m+% months(-i))) &
                  (last_date <= ymd(as.Date("2016-11-08")) %m+% months(i)) & (first_tr_year == 2)) |
               ((last_date >= (ymd(as.Date("2018-11-06")) %m+% months(-i))) &
                  (last_date <= ymd(as.Date("2018-11-06")) %m+% months(i)) & (first_tr_year == 3)))
    
    
    ids <- pre %>%
      mutate(id = row_number()) %>%
      select(id, voter_id, first_tr_year)
    
    X <- pre %>%
      select(-voter_id, -treated, -GEOID, -amount_paid, -last_date,
             -v08, -v16, -v10, -reg_date) %>% 
      mutate_at(vars(white, black, latino, asian, male, dem, rep, v1, v2, v3, paid), ~ ifelse(. == T, 1, 0)) %>% 
      select(first_tr_year, paid, civil, tampa_pd, v1, v2, v3, everything())
    
    genout <- readRDS("temp/genout_hills.rds")
    
    
    mout <- Matchby(Tr = pre$treated, X = X,
                    by = c(X$first_tr_year,
                           X$white,
                           X$black,
                           X$latino,
                           X$asian,
                           X$male,
                           X$dem,
                           X$rep), estimand = "ATT", Weight.matrix = genout, M = 1,
                    exact = c(rep(T, 7), rep(F, 14)), ties = T)
    
    matches <- data.table(voter = c(mout$index.control,
                                    mout$index.treated),
                          group = rep(mout$index.treated, 2),
                          weight = rep(mout$weights, 2)) %>%
      group_by(voter, group) %>%
      summarize(weight = sum(weight)) %>%
      ungroup()
    
    
    matches <- left_join(matches, ids, by = c("voter" = "id")) %>%
      select(-voter, -first_tr_year) %>%
      rename(voter = voter_id)
    
    matches <- left_join(matches, ids, by = c("group" = "id")) %>%
      select(-group) %>%
      rename(group = voter_id)
    
    saveRDS(matches, paste0("temp/matches_rob_", i, ".rds")) 
  }
})

full <- rbindlist(lapply(c(1:23), function(i){
  readRDS(paste0("temp/matches_rob_", i, ".rds")) %>% 
    mutate(month = i)
}))

saveRDS(full, "temp/month_matches.rds")
