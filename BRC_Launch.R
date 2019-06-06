### Application Launch Script

library(shiny)
library(magrittr)
library(dplyr)

config <- read.csv("/home/dc/Documents/R/BRC/BRC_config.csv") %>%
  .[,3] %>% as.character()

db <- config[5]
