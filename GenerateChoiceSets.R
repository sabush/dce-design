library(tidyverse)
source('CommonFunctions.R')

levels <- c(2,2,2)

tmts <-  rbind(c(0,0,0),
               c(0,0,1),
               c(0,1,0),
               c(0,1,1))

# List of generators, each generator should be in a separate row
# e.g. for generators 111 and 011, use rbind(c(1, 1, 1), c(0, 1, 1))
gens <- rbind(c(1, 1, 1))

des_detail <- assess_design(levels, generators = gens, print_detail = F)

des_detail <- assess_design(levels, generators = gens)

gens <- rbind(c(1,1,0), c(0,0,1))
des_detail <- assess_design(levels, generators = gens)


des_detail <- assess_design(levels, generators = gens, treatments = tmts)

# !!! Something is wrong with the efficiency % !!! - the choise sets are not being constructed properly



