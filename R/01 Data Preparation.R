library(AER)
library(here)
library(tidyverse)
library(data.table)
library(ggplot2)

dfc <- fread(here("data", "cardata.csv"))

#--- Session 1: Modify the database ---#
dfc <- dfc %>%
  
  group_by(year) %>% 
  
  mutate(market_size = hh/4) %>% 
  
  mutate(share = quantity / market_size) %>% 
  
  mutate(outshare = 1 - sum(share)) %>% 
  
  mutate(lhs = log(share / outshare))  %>% 
  
  ungroup() 

# Establishing 8 instrument variables: 
dfc <- dfc %>% 
  
  group_by(firm, year) %>%  
  
  mutate(firm_fueleff = sum(fueleff),   
         firm_kw = sum(kw), 
         firm_cylinders = sum(cylinders), 
         firm_footprint = sum(footprint)) %>%
  
  ungroup() %>% 
  
  group_by(year) %>% 
  
  mutate(all_fueleff = sum(fueleff), 
         all_kw = sum(kw),  
         all_cylinders = sum(cylinders), 
         all_footprint = sum(footprint)) 

dfc$IV_fueleff_intra <- (dfc$firm_fueleff - dfc$fueleff) / 10000 
dfc$IV_kw_intra <- (dfc$firm_kw - dfc$kw) / 10000 
dfc$IV_cylinders_intra <- (dfc$firm_cylinders - dfc$cylinders) / 10000 
dfc$IV_footprint_intra <- (dfc$firm_footprint - dfc$footprint) / 10000 

dfc$IV_fueleff_rival <- (dfc$all_fueleff - dfc$firm_fueleff) / 10000 
dfc$IV_kw_rival <- (dfc$all_kw - dfc$firm_kw) / 10000 
dfc$IV_cylinders_rival <- (dfc$all_cylinders - dfc$firm_cylinders) / 10000 
dfc$IV_footprint_rival <- (dfc$all_footprint - dfc$firm_footprint) / 10000 