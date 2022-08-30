## packages used (there may be extra here)

library(splitstackshape)
library(sqldf)
library(fixest)
library(Metrics)
library(RSQLite)
library(modelsummary)
library(tidycensus)
library(modelsummary)
options("modelsummary_format_numeric_latex" = "plain")
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

## function for ggplot theme
theme_bc <- function(base_size = 11, base_family = "BentonSans",
                     legend.position = "right", face = "plain", ...) {
  library(extrafont)
  half_line <- base_size/2
  theme_bw(base_family = base_family) %+replace%
    theme(plot.caption = element_text(size = rel(0.8), hjust = 0,
                                      family = base_family,
                                      vjust = 1, margin = margin(t = half_line)),
          plot.title = element_text(size = rel(1.2), hjust = 0.5,
                                    vjust = 1, margin = margin(b = half_line),
                                    family = base_family),
          plot.subtitle = element_text(hjust = 0.5, vjust = 1, margin = margin(b = half_line)),
          legend.position = legend.position,
          text = element_text(family = base_family, face = face,
                              colour = "black", size = base_size, lineheight = 0.9,
                              hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                              debug = FALSE),
          plot.caption.position =  "plot",
          ...)
}

## quick functions for clearing the workspace
save <- c("db", "cleanup", "theme_bc", "save", "weighted.ttest.ci")


cleanup <- function(...){
  save2 <- c(save, ...)
  rm(list=ls(envir = .GlobalEnv)[! ls(envir = .GlobalEnv) %in% save2], envir = .GlobalEnv)
  gc()
}
