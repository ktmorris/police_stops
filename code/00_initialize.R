library(splitstackshape)
library(sqldf)
library(fixest)
library(Metrics)
library(RSQLite)
library(modelsummary)
library(tidycensus)
library(modelsummary)
library(multcomp)
library(miceadds)
library(Matching)
library(ggstance)
library(lubridate)
library(kableExtra)
library(ggeffects)
library(scales)
library(data.table)
library(tidyverse)

save <- c("db", "cleanup", "theme_bc", "save", "weighted.ttest.ci")
options("modelsummary_format_numeric_latex" = "plain")


cleanup <- function(...){
  save2 <- c(save, ...)
  rm(list=ls(envir = .GlobalEnv)[! ls(envir = .GlobalEnv) %in% save2], envir = .GlobalEnv)
  gc()
}
