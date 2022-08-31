
nop <- readRDS("temp/full_reg_data_no_prior.rds") %>% 
  filter(period <= 0.5)

nom <- readRDS("temp/full_reg_data_no_matching.rds") %>% 
  filter(period <= 0.5) %>% 
  mutate(treated = treated == 1)

m1 <- to ~ treated * post + as.factor(year)

m2 <- to ~ treated * post + as.factor(year) +
  white + black + latino + asian + male +
  dem + rep + age + reg_date + pre_stops + v1 + v2 + v3 +
  median_income + some_college + unem + civil + paid + tampa_pd

m3 <- to ~ treated * post * black + as.factor(year)

m4 <- to ~ treated * post * black + as.factor(year) +
  white + black + latino + asian + male +
  dem + rep + age + reg_date + pre_stops + v1 + v2 + v3 +
  median_income + some_college + unem + civil + paid + tampa_pd


for(gg in c("overall")){
  
  tit = "\\label{tab:dind-table-alt} Overall Treatment Effect"
  ooo = c(1, 2, 4, 23, 24, 22, 25)
  
  
  models1 <- lapply(c(m2, m4), function(f){
    m <- feols(f, data = nop,
               weight = ~ weight, cluster = "group")
  })
  
  models2 <- lapply(c(m2, m4), function(f){
    m <- feols(f, data = nom,
               weight = ~ weight, cluster = "voter_id")
  })
  models <- c(models2, models1)
  rows <- tribble(~term, ~m1,  ~m2, ~m3, ~m4,
                  "Year Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
  
  attr(rows, 'position') <- 53
  modelsummary(models,
               statistic = "std.error",
               stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
               coef_map = c("treatedTRUE:postTRUE" = "Treated $\\times$ Post Treatment",
                            "treatedTRUE:postTRUE:blackTRUE" = "Treated $\\times$ Post Treatment $\\times$ Black",
                            "treatedTRUE" = "Treated",
                            "postTRUE" = "Post Treatment",
                            "blackTRUE" = "Black",
                            "whiteTRUE" = "White",
                            "latinoTRUE" = "Latino",
                            "asianTRUE" = "Asian",
                            "maleTRUE" = "Male",
                            "demTRUE" = "Democrat",
                            "repTRUE" = "Republican",
                            "age" = "Age",
                            "reg_date" = "Registration Date",
                            "pre_stops" = "Traffic Stops before Period",
                            "v1TRUE" = "Turnout (t = -3)",
                            "v2TRUE" = "Turnout (t = -2)",
                            "v3TRUE" = "Turnout (t = -1)",
                            "median_income" = "Nhood Median Income",
                            "some_college" = "Nhood w/ Some College",
                            "unem" = "Nhood Unemployment Rate",
                            "civil" = "Civil Infraction",
                            "paidTRUE" = "Paid Money on Stop",
                            "tampa_pd" = "Stopped by Tampa Police Department",
                            "treatedTRUE:blackTRUE" = "Treated $\\times$ Black",
                            "postTRUE:blackTRUE" = "Post Treatment $\\times$ Black",
                            "(Intercept)" = "Intercept"),
               gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
               title = tit,
               latex_options = "scale_down",
               add_rows = rows,
               output = "latex",
               escape = FALSE,
               booktabs = T) %>% 
    add_header_above(c(" " = 1, "No Matching" = 2, "Pre-Treatment Turnout\nExcluded from Match" = 2)) %>% 
    column_spec(column = c(1), width = "8cm") %>% 
    column_spec(column = c(2:5), width = "3cm") %>%
    save_kable(paste0("temp/table_", gg, "_no_prior.tex"))
  
  j <- fread(paste0("temp/table_", gg, "_no_prior.tex"), header = F, sep = "+") %>% 
    mutate(n = row_number())
  
  insert1 <- "\\resizebox*{!}{0.95\\textheight}{%"
  insert2 <- "}"
  
  j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(4.1, nrow(j) - 0.01))) %>% 
    mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>% 
    arrange(n) %>% 
    select(-n)
  
  
  write.table(j, paste0("./temp/dind_reg_", gg, "_no_prior.tex"), quote = F, col.names = F,
              row.names = F)
  
}
