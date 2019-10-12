library(tidyverse)
source('CommonFunctions.R')


levels <- c(2,2,2)
choicesets <- rbind(c(0,0,0,1,1,1),
                    c(0,0,1,1,1,0),
                    c(1,0,1,0,1,0),
                    c(1,0,0,0,1,1))

assess_design_main_effect(choicesets, levels)
