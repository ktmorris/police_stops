m1 <- to ~ treated * post * latino + as.factor(year)

m2 <- to ~ treated * post * latino + as.factor(year) +
  white + black + latino + asian + male +
  dem + rep + age + reg_date + pre_stops + v1 + v2 + v3 +
  median_income + some_college + unem + civil + paid + tampa_pd

m3 <- to ~ treated * post * asian + as.factor(year)

m4 <- to ~ treated * post * asian + as.factor(year) +
  white + black + latino + asian + male +
  dem + rep + age + reg_date + pre_stops + v1 + v2 + v3 +
  median_income + some_college + unem + civil + paid + tampa_pd

matches<- readRDS("temp/full_reg_data.rds")

dat1 <- filter(matches, period <= 0.5)
tit = "\\label{tab:dind-table} Overall Treatment Effect"
ooo = c(1, 2, 4, 23, 24, 22, 25)

models1 <- lapply(c(m1, m2, m3, m4), function(f){
  m <- feols(f, data = dat1,
             weight = ~ weight, cluster = "group")
})

rows <- tribble(~term, ~m1, ~m2, ~m3, ~m4,
                "Year Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")

attr(rows, 'position') <- 53
modelsummary(models1,
             statistic = "std.error",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = c("treatedTRUE:postTRUE" = "Treated $\\times$ Post Treatment",
                          "treatedTRUE:postTRUE:latinoTRUE" = "Treated $\\times$ Post Treatment $\\times$ Latino",
                          "treatedTRUE:postTRUE:asianTRUE" = "Treated $\\times$ Post Treatment $\\times$ Asian",
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
                          "treatedTRUE:latinoTRUE" = "Treated $\\times$ Latino",
                          "postTRUE:latinoTRUE" = "Post Treatment $\\times$ Latino",
                          "treatedTRUE:asianTRUE" = "Treated $\\times$ Asian",
                          "postTRUE:asianTRUE" = "Post Treatment $\\times$ Asian",
                          "(Intercept)" = "Intercept"),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             title = tit,
             latex_options = "scale_down",
             add_rows = rows,
             output = paste0("temp/big_reg_", gg, "_nonwhite.tex"),
             escape = FALSE)
