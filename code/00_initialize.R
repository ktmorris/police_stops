library(splitstackshape)
library(PanelMatch)
library(miceadds)
library(Matching)
library(plm)
library(ggeffects)
library(scales)
library(extrafont)
library(data.table)
library(stargazer)
library(tidyverse)
library(kevostools)

save <- c("db", "cleanup", "theme_bc", "save", "weighted.ttest.ci")


cleanup <- function(...){
  save2 <- c(save, ...)
  rm(list=ls(envir = .GlobalEnv)[! ls(envir = .GlobalEnv) %in% save2], envir = .GlobalEnv)
}
