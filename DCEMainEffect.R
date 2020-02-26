library(tidyverse)
source('CommonFunctions.R')


levels <- c(2,2,2)
choicesets <- rbind(c(0,0,0,1,1,1),
                    c(0,0,1,1,1,0),
                    c(1,0,1,0,1,0),
                    c(1,0,0,0,1,1))

assess_design_main_effect(choicesets, levels)

ints = matrix(c(1,3),ncol = 2, byrow = T)
construct_contrast_interactions(levels, ints)


levels <- c(3,3,3)
construct_contrast_interactions(levels, ints)

ints = matrix(c(1,3,2,3),ncol = 2, byrow = T)
construct_contrast_interactions(levels, ints)
construct_contrast_interactions(levels, 'all')


assess_design(level_vec = levels, choicesets = choicesets, generators = NULL, 
              treatments = NULL, interactions = ints, print_detail = T,
              contrasts = "OP", addition = "modulo")
