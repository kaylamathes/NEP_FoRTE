#################NPP Calculations (Canopy, subcanopy & Seedlings): FoRTE 2023#########################

#######First Coded by: 09/08/21:  Kerstin Niedermaier (niedermaikm@vcu.edu)
######Edited by: 02/02/23: Kayla Mathes 


# Libraries:
library(dplyr)
library(tidyr)
library(reshape2)
library(plotrix)
library(fortedata)


####################### Canopy NPP  ###################################################################################################################
# First run pipeline
source("Scripts/NPP_pipeline.R") 

#This chunk scales D growth to daily NPP (kgC/ha/day) and annual NPP (kgC/ha/yr)

#create a df that has the # of days (inc_days) between measurements to join with 
#summarized verion of df below
inc_days <- select(df, subplot_id, week, inc_days) %>% unique()

## Calculate NPP!

##2022
NPP_can_2022 <- df %>% 
  filter(month == "2021-11-01" | month == "2022-11-01") %>% 
  mutate(biomass_new = case_when(
    species == "AMEL" ~ (a*(DBH_cm_new*10)^b)/1000,
    species != "AMEL" ~ a*DBH_cm_new^b
  )) %>%
  group_by(subplot_id, date, week) %>% 
  summarise(subplot_biomass_kg = sum(biomass_new)) %>%
  group_by(subplot_id) %>% 
  mutate(biomass_diff = subplot_biomass_kg - lag(subplot_biomass_kg, 
                                                 default = first(subplot_biomass_kg))) %>% 
  filter(week == "2022-10-30" | week == "2022-11-06") %>% 
  mutate(biomass_per_ha = biomass_diff*10) %>% 
  mutate(NPP = biomass_per_ha*0.48) %>% # kgC/ha/yr
  mutate(replicate = substr(subplot_id, 1, 1)) %>% 
  mutate(severity = case_when(
    subplot_id == "A01E" ~ "85", subplot_id == "A01W" ~ "85", subplot_id == "A02E" ~ "45",
    subplot_id == "A02W" ~ "45", subplot_id == "A03E" ~ "65", subplot_id == "A03W" ~ "65",
    subplot_id == "A04E" ~ "0", subplot_id == "A04W" ~ "0", subplot_id == "B01E" ~ "0",
    subplot_id == "B01W" ~ "0", subplot_id == "B02E" ~ "45", subplot_id == "B02W" ~ "45",
    subplot_id == "B03E" ~ "85", subplot_id == "B03W" ~ "85", subplot_id == "B04E" ~ "65",
    subplot_id == "B04W" ~ "65", subplot_id == "C01E" ~ "0", subplot_id == "C01W" ~ "0",
    subplot_id == "C02E" ~ "65", subplot_id == "C02W" ~ "65", subplot_id == "C03E" ~ "85",
    subplot_id == "C03W" ~ "85", subplot_id == "C04E" ~ "45", subplot_id == "C04W" ~ "45", 
    subplot_id == "D01E" ~ "0", subplot_id == "D01W" ~ "0", subplot_id == "D02E" ~ "85",
    subplot_id == "D02W" ~ "85", subplot_id == "D03E" ~ "45", subplot_id == "D03W" ~ "45",
    subplot_id == "D04E" ~ "65", subplot_id == "D04W" ~ "65"
  )) %>% 
  mutate(treatment = case_when(
    subplot_id == "A01E" ~ "bottom", subplot_id == "A01W" ~ "top", subplot_id == "A02E" ~ "top",
    subplot_id == "A02W" ~ "bottom", subplot_id == "A03E" ~ "bottom", subplot_id == "A03W" ~ "top",
    subplot_id == "A04E" ~ "bottom", subplot_id == "A04W" ~ "top", subplot_id == "B01E" ~ "bottom",
    subplot_id == "B01W" ~ "top", subplot_id == "B02E" ~ "top", subplot_id == "B02W" ~ "bottom",
    subplot_id == "B03E" ~ "bottom", subplot_id == "B03W" ~ "top", subplot_id == "B04E" ~ "top",
    subplot_id == "B04W" ~ "bottom", subplot_id == "C01E" ~ "top", subplot_id == "C01W" ~ "bottom",
    subplot_id == "C02E" ~ "bottom", subplot_id == "C02W" ~ "top", subplot_id == "C03E" ~ "bottom",
    subplot_id == "C03W" ~ "top", subplot_id == "C04E" ~ "top", subplot_id == "C04W" ~ "bottom", 
    subplot_id == "D01E" ~ "bottom", subplot_id == "D01W" ~ "top", subplot_id == "D02E" ~ "bottom",
    subplot_id == "D02W" ~ "top", subplot_id == "D03E" ~ "bottom", subplot_id == "D03W" ~ "top",
    subplot_id == "D04E" ~ "top", subplot_id == "D04W" ~ "bottom"
  )) %>% 
  select(subplot_id, NPP, replicate, severity, treatment) %>% 
  mutate(year = "2022")

### Calculate Coarse Root NPP using Cairns et al. 1997 Equation 1 for relationship between above-ground biomass and below-ground biomass = Coarse Root biomass = exp(-1.0850 + 0.9256*log(above-ground biomass)))

NPP_coarse_root_2022 <- df %>% 
  filter(month == "2021-11-01" | month == "2022-11-01") %>% 
  mutate(biomass_new = case_when(
    species == "AMEL" ~ (a*(DBH_cm_new*10)^b)/1000,
    species != "AMEL" ~ a*DBH_cm_new^b
  )) %>%
  group_by(subplot_id, date, week) %>% 
  summarise(subplot_biomass_kg = sum(biomass_new)) %>%
  mutate(subplot_biomass_Mg = subplot_biomass_kg/1000)%>%
  mutate(coarse_root_subplot_biomass_Mg = exp(-1.0850 + 0.9256*log(subplot_biomass_Mg)))%>%
  group_by(subplot_id) %>% 
  mutate(coarse_root_biomass_diff = coarse_root_subplot_biomass_Mg - lag(coarse_root_subplot_biomass_Mg, 
                                                 default = first(coarse_root_subplot_biomass_Mg))) %>% 
  filter(week == "2022-10-30" | week == "2022-11-06") %>% 
  mutate(coarse_root_biomass_per_ha = coarse_root_biomass_diff*10) %>% 
  mutate(NPP = coarse_root_biomass_per_ha*0.49) %>% # MgC/ha/yr
  mutate(replicate = substr(subplot_id, 1, 1)) %>% 
  mutate(severity = case_when(
    subplot_id == "A01E" ~ "85", subplot_id == "A01W" ~ "85", subplot_id == "A02E" ~ "45",
    subplot_id == "A02W" ~ "45", subplot_id == "A03E" ~ "65", subplot_id == "A03W" ~ "65",
    subplot_id == "A04E" ~ "0", subplot_id == "A04W" ~ "0", subplot_id == "B01E" ~ "0",
    subplot_id == "B01W" ~ "0", subplot_id == "B02E" ~ "45", subplot_id == "B02W" ~ "45",
    subplot_id == "B03E" ~ "85", subplot_id == "B03W" ~ "85", subplot_id == "B04E" ~ "65",
    subplot_id == "B04W" ~ "65", subplot_id == "C01E" ~ "0", subplot_id == "C01W" ~ "0",
    subplot_id == "C02E" ~ "65", subplot_id == "C02W" ~ "65", subplot_id == "C03E" ~ "85",
    subplot_id == "C03W" ~ "85", subplot_id == "C04E" ~ "45", subplot_id == "C04W" ~ "45", 
    subplot_id == "D01E" ~ "0", subplot_id == "D01W" ~ "0", subplot_id == "D02E" ~ "85",
    subplot_id == "D02W" ~ "85", subplot_id == "D03E" ~ "45", subplot_id == "D03W" ~ "45",
    subplot_id == "D04E" ~ "65", subplot_id == "D04W" ~ "65"
  )) %>% 
  mutate(treatment = case_when(
    subplot_id == "A01E" ~ "bottom", subplot_id == "A01W" ~ "top", subplot_id == "A02E" ~ "top",
    subplot_id == "A02W" ~ "bottom", subplot_id == "A03E" ~ "bottom", subplot_id == "A03W" ~ "top",
    subplot_id == "A04E" ~ "bottom", subplot_id == "A04W" ~ "top", subplot_id == "B01E" ~ "bottom",
    subplot_id == "B01W" ~ "top", subplot_id == "B02E" ~ "top", subplot_id == "B02W" ~ "bottom",
    subplot_id == "B03E" ~ "bottom", subplot_id == "B03W" ~ "top", subplot_id == "B04E" ~ "top",
    subplot_id == "B04W" ~ "bottom", subplot_id == "C01E" ~ "top", subplot_id == "C01W" ~ "bottom",
    subplot_id == "C02E" ~ "bottom", subplot_id == "C02W" ~ "top", subplot_id == "C03E" ~ "bottom",
    subplot_id == "C03W" ~ "top", subplot_id == "C04E" ~ "top", subplot_id == "C04W" ~ "bottom", 
    subplot_id == "D01E" ~ "bottom", subplot_id == "D01W" ~ "top", subplot_id == "D02E" ~ "bottom",
    subplot_id == "D02W" ~ "top", subplot_id == "D03E" ~ "bottom", subplot_id == "D03W" ~ "top",
    subplot_id == "D04E" ~ "top", subplot_id == "D04W" ~ "bottom"
  )) %>% 
  select(subplot_id, NPP, replicate, severity, treatment) %>% 
  mutate(year = "2022")




##2021
NPP_can_2021 <- df %>% 
  filter(month == "2020-11-01" | month == "2021-11-01") %>% 
  mutate(biomass_new = case_when(
    species == "AMEL" ~ (a*(DBH_cm_new*10)^b)/1000,
    species != "AMEL" ~ a*DBH_cm_new^b
  )) %>%
  group_by(subplot_id, date, week) %>% 
  summarise(subplot_biomass_kg = sum(biomass_new)) %>%
  group_by(subplot_id) %>% 
  mutate(biomass_diff = subplot_biomass_kg - lag(subplot_biomass_kg, 
                                                 default = first(subplot_biomass_kg))) %>% 
  filter(week == "2021-11-14" | week == "2021-11-07") %>% 
  mutate(biomass_per_ha = biomass_diff*10) %>% 
  mutate(NPP = biomass_per_ha*0.48) %>% # kgC/ha/yr
  mutate(replicate = substr(subplot_id, 1, 1)) %>% 
  mutate(severity = case_when(
    subplot_id == "A01E" ~ "85", subplot_id == "A01W" ~ "85", subplot_id == "A02E" ~ "45",
    subplot_id == "A02W" ~ "45", subplot_id == "A03E" ~ "65", subplot_id == "A03W" ~ "65",
    subplot_id == "A04E" ~ "0", subplot_id == "A04W" ~ "0", subplot_id == "B01E" ~ "0",
    subplot_id == "B01W" ~ "0", subplot_id == "B02E" ~ "45", subplot_id == "B02W" ~ "45",
    subplot_id == "B03E" ~ "85", subplot_id == "B03W" ~ "85", subplot_id == "B04E" ~ "65",
    subplot_id == "B04W" ~ "65", subplot_id == "C01E" ~ "0", subplot_id == "C01W" ~ "0",
    subplot_id == "C02E" ~ "65", subplot_id == "C02W" ~ "65", subplot_id == "C03E" ~ "85",
    subplot_id == "C03W" ~ "85", subplot_id == "C04E" ~ "45", subplot_id == "C04W" ~ "45", 
    subplot_id == "D01E" ~ "0", subplot_id == "D01W" ~ "0", subplot_id == "D02E" ~ "85",
    subplot_id == "D02W" ~ "85", subplot_id == "D03E" ~ "45", subplot_id == "D03W" ~ "45",
    subplot_id == "D04E" ~ "65", subplot_id == "D04W" ~ "65"
  )) %>% 
  mutate(treatment = case_when(
    subplot_id == "A01E" ~ "bottom", subplot_id == "A01W" ~ "top", subplot_id == "A02E" ~ "top",
    subplot_id == "A02W" ~ "bottom", subplot_id == "A03E" ~ "bottom", subplot_id == "A03W" ~ "top",
    subplot_id == "A04E" ~ "bottom", subplot_id == "A04W" ~ "top", subplot_id == "B01E" ~ "bottom",
    subplot_id == "B01W" ~ "top", subplot_id == "B02E" ~ "top", subplot_id == "B02W" ~ "bottom",
    subplot_id == "B03E" ~ "bottom", subplot_id == "B03W" ~ "top", subplot_id == "B04E" ~ "top",
    subplot_id == "B04W" ~ "bottom", subplot_id == "C01E" ~ "top", subplot_id == "C01W" ~ "bottom",
    subplot_id == "C02E" ~ "bottom", subplot_id == "C02W" ~ "top", subplot_id == "C03E" ~ "bottom",
    subplot_id == "C03W" ~ "top", subplot_id == "C04E" ~ "top", subplot_id == "C04W" ~ "bottom", 
    subplot_id == "D01E" ~ "bottom", subplot_id == "D01W" ~ "top", subplot_id == "D02E" ~ "bottom",
    subplot_id == "D02W" ~ "top", subplot_id == "D03E" ~ "bottom", subplot_id == "D03W" ~ "top",
    subplot_id == "D04E" ~ "top", subplot_id == "D04W" ~ "bottom"
  )) %>% 
  select(subplot_id, NPP, replicate, severity, treatment) %>% 
  mutate(year = "2021")

## Calculate Coarse Root NPP using Cairns et al. 1997 Equation 1 for relationship between above-ground biomass and below-ground biomass = Coarse Root biomass = exp(-1.0850 + 0.9256*log(above-ground biomass)))
NPP_coarse_root_2021 <- df %>% 
  filter(month == "2020-11-01" | month == "2021-11-01") %>%  
  mutate(biomass_new = case_when(
    species == "AMEL" ~ (a*(DBH_cm_new*10)^b)/1000,
    species != "AMEL" ~ a*DBH_cm_new^b
  )) %>%
  group_by(subplot_id, date, week) %>% 
  summarise(subplot_biomass_kg = sum(biomass_new)) %>%
  mutate(subplot_biomass_Mg = subplot_biomass_kg/1000)%>%
  mutate(coarse_root_subplot_biomass_Mg = exp(-1.0850 + 0.9256*log(subplot_biomass_Mg)))%>%
  group_by(subplot_id) %>% 
  mutate(coarse_root_biomass_diff = coarse_root_subplot_biomass_Mg - lag(coarse_root_subplot_biomass_Mg, 
                                                                         default = first(coarse_root_subplot_biomass_Mg))) %>% 
  filter(week == "2021-11-14" | week == "2021-11-07") %>%  
  mutate(coarse_root_biomass_per_ha = coarse_root_biomass_diff*10) %>% 
  mutate(NPP = coarse_root_biomass_per_ha*0.49) %>% # MgC/ha/yr
  mutate(replicate = substr(subplot_id, 1, 1)) %>% 
  mutate(severity = case_when(
    subplot_id == "A01E" ~ "85", subplot_id == "A01W" ~ "85", subplot_id == "A02E" ~ "45",
    subplot_id == "A02W" ~ "45", subplot_id == "A03E" ~ "65", subplot_id == "A03W" ~ "65",
    subplot_id == "A04E" ~ "0", subplot_id == "A04W" ~ "0", subplot_id == "B01E" ~ "0",
    subplot_id == "B01W" ~ "0", subplot_id == "B02E" ~ "45", subplot_id == "B02W" ~ "45",
    subplot_id == "B03E" ~ "85", subplot_id == "B03W" ~ "85", subplot_id == "B04E" ~ "65",
    subplot_id == "B04W" ~ "65", subplot_id == "C01E" ~ "0", subplot_id == "C01W" ~ "0",
    subplot_id == "C02E" ~ "65", subplot_id == "C02W" ~ "65", subplot_id == "C03E" ~ "85",
    subplot_id == "C03W" ~ "85", subplot_id == "C04E" ~ "45", subplot_id == "C04W" ~ "45", 
    subplot_id == "D01E" ~ "0", subplot_id == "D01W" ~ "0", subplot_id == "D02E" ~ "85",
    subplot_id == "D02W" ~ "85", subplot_id == "D03E" ~ "45", subplot_id == "D03W" ~ "45",
    subplot_id == "D04E" ~ "65", subplot_id == "D04W" ~ "65"
  )) %>% 
  mutate(treatment = case_when(
    subplot_id == "A01E" ~ "bottom", subplot_id == "A01W" ~ "top", subplot_id == "A02E" ~ "top",
    subplot_id == "A02W" ~ "bottom", subplot_id == "A03E" ~ "bottom", subplot_id == "A03W" ~ "top",
    subplot_id == "A04E" ~ "bottom", subplot_id == "A04W" ~ "top", subplot_id == "B01E" ~ "bottom",
    subplot_id == "B01W" ~ "top", subplot_id == "B02E" ~ "top", subplot_id == "B02W" ~ "bottom",
    subplot_id == "B03E" ~ "bottom", subplot_id == "B03W" ~ "top", subplot_id == "B04E" ~ "top",
    subplot_id == "B04W" ~ "bottom", subplot_id == "C01E" ~ "top", subplot_id == "C01W" ~ "bottom",
    subplot_id == "C02E" ~ "bottom", subplot_id == "C02W" ~ "top", subplot_id == "C03E" ~ "bottom",
    subplot_id == "C03W" ~ "top", subplot_id == "C04E" ~ "top", subplot_id == "C04W" ~ "bottom", 
    subplot_id == "D01E" ~ "bottom", subplot_id == "D01W" ~ "top", subplot_id == "D02E" ~ "bottom",
    subplot_id == "D02W" ~ "top", subplot_id == "D03E" ~ "bottom", subplot_id == "D03W" ~ "top",
    subplot_id == "D04E" ~ "top", subplot_id == "D04W" ~ "bottom"
  )) %>% 
  select(subplot_id, NPP, replicate, severity, treatment) %>% 
  mutate(year = "2021")


# Now repeat for 2020
NPP_can_2020 <- df %>% 
  filter(week == "2019-11-10" | week == "2020-11-15") %>% 
  mutate(biomass_new = case_when(
    species == "AMEL" ~ (a*(DBH_cm_new*10)^b)/1000,
    species != "AMEL" ~ a*DBH_cm_new^b
  )) %>%
  group_by(subplot_id, date, week) %>% 
  summarise(subplot_biomass_kg = sum(biomass_new)) %>%
  group_by(subplot_id) %>% 
  mutate(biomass_diff = subplot_biomass_kg - lag(subplot_biomass_kg, 
                                                 default = first(subplot_biomass_kg))) %>% 
  filter(week == "2020-11-15") %>% 
  mutate(biomass_per_ha = biomass_diff*10) %>% 
  mutate(NPP = biomass_per_ha*0.48) %>% # kgC/ha/yr
  mutate(replicate = substr(subplot_id, 1, 1)) %>% 
  mutate(severity = case_when(
    subplot_id == "A01E" ~ "85", subplot_id == "A01W" ~ "85", subplot_id == "A02E" ~ "45",
    subplot_id == "A02W" ~ "45", subplot_id == "A03E" ~ "65", subplot_id == "A03W" ~ "65",
    subplot_id == "A04E" ~ "0", subplot_id == "A04W" ~ "0", subplot_id == "B01E" ~ "0",
    subplot_id == "B01W" ~ "0", subplot_id == "B02E" ~ "45", subplot_id == "B02W" ~ "45",
    subplot_id == "B03E" ~ "85", subplot_id == "B03W" ~ "85", subplot_id == "B04E" ~ "65",
    subplot_id == "B04W" ~ "65", subplot_id == "C01E" ~ "0", subplot_id == "C01W" ~ "0",
    subplot_id == "C02E" ~ "65", subplot_id == "C02W" ~ "65", subplot_id == "C03E" ~ "85",
    subplot_id == "C03W" ~ "85", subplot_id == "C04E" ~ "45", subplot_id == "C04W" ~ "45", 
    subplot_id == "D01E" ~ "0", subplot_id == "D01W" ~ "0", subplot_id == "D02E" ~ "85",
    subplot_id == "D02W" ~ "85", subplot_id == "D03E" ~ "45", subplot_id == "D03W" ~ "45",
    subplot_id == "D04E" ~ "65", subplot_id == "D04W" ~ "65"
  )) %>% 
  mutate(treatment = case_when(
    subplot_id == "A01E" ~ "bottom", subplot_id == "A01W" ~ "top", subplot_id == "A02E" ~ "top",
    subplot_id == "A02W" ~ "bottom", subplot_id == "A03E" ~ "bottom", subplot_id == "A03W" ~ "top",
    subplot_id == "A04E" ~ "bottom", subplot_id == "A04W" ~ "top", subplot_id == "B01E" ~ "bottom",
    subplot_id == "B01W" ~ "top", subplot_id == "B02E" ~ "top", subplot_id == "B02W" ~ "bottom",
    subplot_id == "B03E" ~ "bottom", subplot_id == "B03W" ~ "top", subplot_id == "B04E" ~ "top",
    subplot_id == "B04W" ~ "bottom", subplot_id == "C01E" ~ "top", subplot_id == "C01W" ~ "bottom",
    subplot_id == "C02E" ~ "bottom", subplot_id == "C02W" ~ "top", subplot_id == "C03E" ~ "bottom",
    subplot_id == "C03W" ~ "top", subplot_id == "C04E" ~ "top", subplot_id == "C04W" ~ "bottom", 
    subplot_id == "D01E" ~ "bottom", subplot_id == "D01W" ~ "top", subplot_id == "D02E" ~ "bottom",
    subplot_id == "D02W" ~ "top", subplot_id == "D03E" ~ "bottom", subplot_id == "D03W" ~ "top",
    subplot_id == "D04E" ~ "top", subplot_id == "D04W" ~ "bottom"
  )) %>% 
  select(subplot_id, NPP, replicate, severity, treatment) %>% 
  mutate(year = "2020")

## Calculate Coarse Root NPP using Cairns et al. 1997 Equation 1 for relationship between above-ground biomass and below-ground biomass = Coarse Root biomass = exp(-1.0850 + 0.9256*log(above-ground biomass)))
NPP_coarse_root_2020 <- df %>% 
  filter(week == "2019-11-10" | week == "2020-11-15") %>%  
  mutate(biomass_new = case_when(
    species == "AMEL" ~ (a*(DBH_cm_new*10)^b)/1000,
    species != "AMEL" ~ a*DBH_cm_new^b
  )) %>%
  group_by(subplot_id, date, week) %>% 
  summarise(subplot_biomass_kg = sum(biomass_new)) %>%
  mutate(subplot_biomass_Mg = subplot_biomass_kg/1000)%>%
  mutate(coarse_root_subplot_biomass_Mg = exp(-1.0850 + 0.9256*log(subplot_biomass_Mg)))%>%
  group_by(subplot_id) %>% 
  mutate(coarse_root_biomass_diff = coarse_root_subplot_biomass_Mg - lag(coarse_root_subplot_biomass_Mg, 
                                                                         default = first(coarse_root_subplot_biomass_Mg))) %>% 
  filter(week == "2020-11-15") %>%  
  mutate(coarse_root_biomass_per_ha = coarse_root_biomass_diff*10) %>% 
  mutate(NPP = coarse_root_biomass_per_ha*0.49) %>% # MgC/ha/yr
  mutate(replicate = substr(subplot_id, 1, 1)) %>% 
  mutate(severity = case_when(
    subplot_id == "A01E" ~ "85", subplot_id == "A01W" ~ "85", subplot_id == "A02E" ~ "45",
    subplot_id == "A02W" ~ "45", subplot_id == "A03E" ~ "65", subplot_id == "A03W" ~ "65",
    subplot_id == "A04E" ~ "0", subplot_id == "A04W" ~ "0", subplot_id == "B01E" ~ "0",
    subplot_id == "B01W" ~ "0", subplot_id == "B02E" ~ "45", subplot_id == "B02W" ~ "45",
    subplot_id == "B03E" ~ "85", subplot_id == "B03W" ~ "85", subplot_id == "B04E" ~ "65",
    subplot_id == "B04W" ~ "65", subplot_id == "C01E" ~ "0", subplot_id == "C01W" ~ "0",
    subplot_id == "C02E" ~ "65", subplot_id == "C02W" ~ "65", subplot_id == "C03E" ~ "85",
    subplot_id == "C03W" ~ "85", subplot_id == "C04E" ~ "45", subplot_id == "C04W" ~ "45", 
    subplot_id == "D01E" ~ "0", subplot_id == "D01W" ~ "0", subplot_id == "D02E" ~ "85",
    subplot_id == "D02W" ~ "85", subplot_id == "D03E" ~ "45", subplot_id == "D03W" ~ "45",
    subplot_id == "D04E" ~ "65", subplot_id == "D04W" ~ "65"
  )) %>% 
  mutate(treatment = case_when(
    subplot_id == "A01E" ~ "bottom", subplot_id == "A01W" ~ "top", subplot_id == "A02E" ~ "top",
    subplot_id == "A02W" ~ "bottom", subplot_id == "A03E" ~ "bottom", subplot_id == "A03W" ~ "top",
    subplot_id == "A04E" ~ "bottom", subplot_id == "A04W" ~ "top", subplot_id == "B01E" ~ "bottom",
    subplot_id == "B01W" ~ "top", subplot_id == "B02E" ~ "top", subplot_id == "B02W" ~ "bottom",
    subplot_id == "B03E" ~ "bottom", subplot_id == "B03W" ~ "top", subplot_id == "B04E" ~ "top",
    subplot_id == "B04W" ~ "bottom", subplot_id == "C01E" ~ "top", subplot_id == "C01W" ~ "bottom",
    subplot_id == "C02E" ~ "bottom", subplot_id == "C02W" ~ "top", subplot_id == "C03E" ~ "bottom",
    subplot_id == "C03W" ~ "top", subplot_id == "C04E" ~ "top", subplot_id == "C04W" ~ "bottom", 
    subplot_id == "D01E" ~ "bottom", subplot_id == "D01W" ~ "top", subplot_id == "D02E" ~ "bottom",
    subplot_id == "D02W" ~ "top", subplot_id == "D03E" ~ "bottom", subplot_id == "D03W" ~ "top",
    subplot_id == "D04E" ~ "top", subplot_id == "D04W" ~ "bottom"
  )) %>% 
  select(subplot_id, NPP, replicate, severity, treatment) %>% 
  mutate(year = "2020")


# Now repeat for 2019
NPP_can_2019 <- df %>% 
  filter(month == "2018-11-01" | month == "2019-11-01") %>% 
  mutate(biomass_new = case_when(
    species == "AMEL" ~ (a*(DBH_cm_new*10)^b)/1000,
    species != "AMEL" ~ a*DBH_cm_new^b
  )) %>%
  group_by(subplot_id, date, week) %>% 
  summarise(subplot_biomass_kg = sum(biomass_new)) %>%
  group_by(subplot_id) %>% 
  mutate(biomass_diff = subplot_biomass_kg - lag(subplot_biomass_kg, 
                                                 default = first(subplot_biomass_kg))) %>% 
  filter(week == "2019-11-10") %>% 
  mutate(biomass_per_ha = biomass_diff*10) %>% 
  mutate(NPP = biomass_per_ha*0.48) %>% # kgC/ha/yr
  mutate(replicate = substr(subplot_id, 1, 1)) %>% 
  mutate(severity = case_when(
    subplot_id == "A01E" ~ "85", subplot_id == "A01W" ~ "85", subplot_id == "A02E" ~ "45",
    subplot_id == "A02W" ~ "45", subplot_id == "A03E" ~ "65", subplot_id == "A03W" ~ "65",
    subplot_id == "A04E" ~ "0", subplot_id == "A04W" ~ "0", subplot_id == "B01E" ~ "0",
    subplot_id == "B01W" ~ "0", subplot_id == "B02E" ~ "45", subplot_id == "B02W" ~ "45",
    subplot_id == "B03E" ~ "85", subplot_id == "B03W" ~ "85", subplot_id == "B04E" ~ "65",
    subplot_id == "B04W" ~ "65", subplot_id == "C01E" ~ "0", subplot_id == "C01W" ~ "0",
    subplot_id == "C02E" ~ "65", subplot_id == "C02W" ~ "65", subplot_id == "C03E" ~ "85",
    subplot_id == "C03W" ~ "85", subplot_id == "C04E" ~ "45", subplot_id == "C04W" ~ "45", 
    subplot_id == "D01E" ~ "0", subplot_id == "D01W" ~ "0", subplot_id == "D02E" ~ "85",
    subplot_id == "D02W" ~ "85", subplot_id == "D03E" ~ "45", subplot_id == "D03W" ~ "45",
    subplot_id == "D04E" ~ "65", subplot_id == "D04W" ~ "65"
  )) %>% 
  mutate(treatment = case_when(
    subplot_id == "A01E" ~ "bottom", subplot_id == "A01W" ~ "top", subplot_id == "A02E" ~ "top",
    subplot_id == "A02W" ~ "bottom", subplot_id == "A03E" ~ "bottom", subplot_id == "A03W" ~ "top",
    subplot_id == "A04E" ~ "bottom", subplot_id == "A04W" ~ "top", subplot_id == "B01E" ~ "bottom",
    subplot_id == "B01W" ~ "top", subplot_id == "B02E" ~ "top", subplot_id == "B02W" ~ "bottom",
    subplot_id == "B03E" ~ "bottom", subplot_id == "B03W" ~ "top", subplot_id == "B04E" ~ "top",
    subplot_id == "B04W" ~ "bottom", subplot_id == "C01E" ~ "top", subplot_id == "C01W" ~ "bottom",
    subplot_id == "C02E" ~ "bottom", subplot_id == "C02W" ~ "top", subplot_id == "C03E" ~ "bottom",
    subplot_id == "C03W" ~ "top", subplot_id == "C04E" ~ "top", subplot_id == "C04W" ~ "bottom", 
    subplot_id == "D01E" ~ "bottom", subplot_id == "D01W" ~ "top", subplot_id == "D02E" ~ "bottom",
    subplot_id == "D02W" ~ "top", subplot_id == "D03E" ~ "bottom", subplot_id == "D03W" ~ "top",
    subplot_id == "D04E" ~ "top", subplot_id == "D04W" ~ "bottom"
  )) %>% 
  select(subplot_id, NPP, replicate, severity, treatment) %>% 
  mutate(year = "2019")

NPP_coarse_root_2019 <- df %>% 
  filter(month == "2018-11-01" | month == "2019-11-01") %>% 
  mutate(biomass_new = case_when(
    species == "AMEL" ~ (a*(DBH_cm_new*10)^b)/1000,
    species != "AMEL" ~ a*DBH_cm_new^b
  )) %>%
  group_by(subplot_id, date, week) %>% 
  summarise(subplot_biomass_kg = sum(biomass_new)) %>%
  mutate(subplot_biomass_Mg = subplot_biomass_kg/1000)%>%
  mutate(coarse_root_subplot_biomass_Mg = exp(-1.0850 + 0.9256*log(subplot_biomass_Mg)))%>%
  group_by(subplot_id) %>% 
  mutate(coarse_root_biomass_diff = coarse_root_subplot_biomass_Mg - lag(coarse_root_subplot_biomass_Mg, 
                                                                         default = first(coarse_root_subplot_biomass_Mg))) %>% 
  filter(week == "2019-11-10") %>%  
  mutate(coarse_root_biomass_per_ha = coarse_root_biomass_diff*10) %>% 
  mutate(NPP = coarse_root_biomass_per_ha*0.49) %>% # MgC/ha/yr
  mutate(replicate = substr(subplot_id, 1, 1)) %>% 
  mutate(severity = case_when(
    subplot_id == "A01E" ~ "85", subplot_id == "A01W" ~ "85", subplot_id == "A02E" ~ "45",
    subplot_id == "A02W" ~ "45", subplot_id == "A03E" ~ "65", subplot_id == "A03W" ~ "65",
    subplot_id == "A04E" ~ "0", subplot_id == "A04W" ~ "0", subplot_id == "B01E" ~ "0",
    subplot_id == "B01W" ~ "0", subplot_id == "B02E" ~ "45", subplot_id == "B02W" ~ "45",
    subplot_id == "B03E" ~ "85", subplot_id == "B03W" ~ "85", subplot_id == "B04E" ~ "65",
    subplot_id == "B04W" ~ "65", subplot_id == "C01E" ~ "0", subplot_id == "C01W" ~ "0",
    subplot_id == "C02E" ~ "65", subplot_id == "C02W" ~ "65", subplot_id == "C03E" ~ "85",
    subplot_id == "C03W" ~ "85", subplot_id == "C04E" ~ "45", subplot_id == "C04W" ~ "45", 
    subplot_id == "D01E" ~ "0", subplot_id == "D01W" ~ "0", subplot_id == "D02E" ~ "85",
    subplot_id == "D02W" ~ "85", subplot_id == "D03E" ~ "45", subplot_id == "D03W" ~ "45",
    subplot_id == "D04E" ~ "65", subplot_id == "D04W" ~ "65"
  )) %>% 
  mutate(treatment = case_when(
    subplot_id == "A01E" ~ "bottom", subplot_id == "A01W" ~ "top", subplot_id == "A02E" ~ "top",
    subplot_id == "A02W" ~ "bottom", subplot_id == "A03E" ~ "bottom", subplot_id == "A03W" ~ "top",
    subplot_id == "A04E" ~ "bottom", subplot_id == "A04W" ~ "top", subplot_id == "B01E" ~ "bottom",
    subplot_id == "B01W" ~ "top", subplot_id == "B02E" ~ "top", subplot_id == "B02W" ~ "bottom",
    subplot_id == "B03E" ~ "bottom", subplot_id == "B03W" ~ "top", subplot_id == "B04E" ~ "top",
    subplot_id == "B04W" ~ "bottom", subplot_id == "C01E" ~ "top", subplot_id == "C01W" ~ "bottom",
    subplot_id == "C02E" ~ "bottom", subplot_id == "C02W" ~ "top", subplot_id == "C03E" ~ "bottom",
    subplot_id == "C03W" ~ "top", subplot_id == "C04E" ~ "top", subplot_id == "C04W" ~ "bottom", 
    subplot_id == "D01E" ~ "bottom", subplot_id == "D01W" ~ "top", subplot_id == "D02E" ~ "bottom",
    subplot_id == "D02W" ~ "top", subplot_id == "D03E" ~ "bottom", subplot_id == "D03W" ~ "top",
    subplot_id == "D04E" ~ "top", subplot_id == "D04W" ~ "bottom"
  )) %>% 
  select(subplot_id, NPP, replicate, severity, treatment) %>% 
  mutate(year = "2019")



####################### Subcanopy NPP  ###############################################################################################################################################################################################

## 2022 increment 
# Load in subcanopy data
subcanopy_raw_2022 <- read.csv("Data_NPP/Raw_subcanopy_D_2022.csv")

# Allometric equations for subcanopy trees
biomass_a <- function(species, DBH){
  if (species == "ACRU"){
    biomass <- 0.03117 * (DBH) ^ 2.7780
  } else if (species == "ACPE"){
    biomass <-  0.2040 * DBH ^ 2.2524
  } else if (species == "ACSA"){
    biomass <- 0.1693 * DBH ^ 2.3436
  } else if (species == "AMEL"){
    biomass <- (0.1630 * (DBH * 10) ^ 2.4940)/1000
  } else if (species == "FAGR"){
    biomass <- 0.1892 * DBH ^ 2.3097
  } else if (species == "PIRE"){
    biomass <- 0.0526 * DBH ^ 2.5258
  } else if (species == "PIST"){
    biomass <- 0.0408 * DBH ^ 2.5735
  } else if (species == "POGR"){
    biomass <- 0.1387 * DBH ^ 2.3498
  } else if (species == "QURU"){
    biomass <- 0.0398 * DBH ^ 2.7734
  }
  return(biomass)
}

# vectorize my function because it will be used on a vector 
biomass_a <- Vectorize(biomass_a, vectorize.args = c("species", "DBH"))

# change dbh_mm from character to numeric (idk why it's a character) and date 
# into date format
subcanopy_raw_2022$DBH_cm <- as.numeric(subcanopy_raw_2022$DBH_cm)
subcanopy_raw_2022$date <- as.Date(subcanopy_raw_2022$date, "%Y-%m-%d")

# Next, select important columns from subcanopy data. 
# Then remove records where DBH_cm was NA. 
#  use my biomass_a function to estimate biomass(kg)
subcanopy_biomass_2022 <- subcanopy_raw_2022 %>% 
  select(subplot_id,  species, tag, DBH_cm, date) %>% 
  filter(!is.na(DBH_cm)) %>% 
  mutate(biomass_kg = biomass_a(species, DBH_cm))

# next step: pull in previous year's subcanopy data and look at the incremental
# growth to see change in biomass --> NPP

# Load in data from 2021 and filter just the fall data 
subcanopy_2021 <- read.csv("Data_NPP/fd_subcanopy_diameter_2021.csv") %>% 
  filter(date == "2021-11-12" |date == "2021-11-13" | date == "2021-11-15"  )

# Now repeat the same code from 2021 to get dbh_cm and then biomass

subcanopy_2021$dbh_mm <- as.numeric(subcanopy_2021$dbh_mm)

subcanopy_biomass_2021 <- subcanopy_2021 %>% 
  select(subplot_id,  species, tag, dbh_mm, date) %>% 
  filter(!is.na(dbh_mm)) %>% 
  mutate(dbh_cm = dbh_mm/10) %>%
  mutate(biomass_kg = biomass_a(species, dbh_cm)) 


# First gotta convert date to date again for 2020
subcanopy_biomass_2021$date <- as.Date(subcanopy_biomass_2021$date, "%Y-%m-%d")

# Join these two DFs into one
subcanopy_inc_2022 <- bind_rows(subcanopy_biomass_2021, subcanopy_biomass_2022) %>% 
  arrange(tag, date) %>% group_by(tag) %>% 
  mutate(biomass_inc = biomass_kg - lag(biomass_kg, default = first(biomass_kg))) %>% 
  filter(row_number() == n()) %>% 
  select(subplot_id, species, tag, biomass_kg, biomass_inc) %>% 
  arrange(subplot_id) 

# Get rid of negative numbers
subcanopy_inc_2022$biomass_inc[subcanopy_inc_2022$biomass_inc < 0] <- 0

# Turn this increment into NPP for the subcanopy!

# First, load in subcanopy tree counts for species
tree_counts <- read.csv("Data_NPP/fd_subcanopy_stem_count_2019.csv")

# Then I just copied this code from Max in 1-8 cm RScript; this is basically filling
# in the number of stem counts per subplot so that we can "count" how much the
# unmeasured stems grew by using the average growth of the subcanopy trees we did
# measure 
NPP_sc_2022 <- subcanopy_inc_2022 %>% 
  group_by(subplot_id) %>% 
  summarize(mean_subplot = mean(biomass_inc)) %>% 
  left_join(subcanopy_inc_2022) %>% 
  right_join(tree_counts) %>% arrange(subplot_id, species) %>% 
  group_by(subplot_id) %>% fill(mean_subplot, .direction = "updown") %>% 
  group_by(subplot_id, species) %>% 
  mutate(biomass_inc_obs_est = case_when(
    !is.na(biomass_inc) ~ biomass_inc,
    is.na(biomass_inc) ~ mean_subplot
  )) %>% 
  summarize(mean_biomass = mean(biomass_inc_obs_est)) %>% 
  right_join(tree_counts) %>% 
  mutate(sp_biomass_ha = mean_biomass * count * 40) %>% 
  # * species count * 40 b/c counts were in 0.025ha
  group_by(subplot_id) %>% 
  summarize(kgC_ha_yr = sum(sp_biomass_ha) * 0.48) %>% 
  rename(NPP = kgC_ha_yr) %>% 
  mutate(severity = case_when(
    subplot_id == "A01E" ~ "85", subplot_id == "A01W" ~ "85", subplot_id == "A02E" ~ "45",
    subplot_id == "A02W" ~ "45", subplot_id == "A03E" ~ "65", subplot_id == "A03W" ~ "65",
    subplot_id == "A04E" ~ "0", subplot_id == "A04W" ~ "0", subplot_id == "B01E" ~ "0",
    subplot_id == "B01W" ~ "0", subplot_id == "B02E" ~ "45", subplot_id == "B02W" ~ "45",
    subplot_id == "B03E" ~ "85", subplot_id == "B03W" ~ "85", subplot_id == "B04E" ~ "65",
    subplot_id == "B04W" ~ "65", subplot_id == "C01E" ~ "0", subplot_id == "C01W" ~ "0",
    subplot_id == "C02E" ~ "65", subplot_id == "C02W" ~ "65", subplot_id == "C03E" ~ "85",
    subplot_id == "C03W" ~ "85", subplot_id == "C04E" ~ "45", subplot_id == "C04W" ~ "45", 
    subplot_id == "D01E" ~ "0", subplot_id == "D01W" ~ "0", subplot_id == "D02E" ~ "85",
    subplot_id == "D02W" ~ "85", subplot_id == "D03E" ~ "45", subplot_id == "D03W" ~ "45",
    subplot_id == "D04E" ~ "65", subplot_id == "D04W" ~ "65"
  )) %>% 
  mutate(treatment = case_when(
    subplot_id == "A01E" ~ "bottom", subplot_id == "A01W" ~ "top", subplot_id == "A02E" ~ "top",
    subplot_id == "A02W" ~ "bottom", subplot_id == "A03E" ~ "bottom", subplot_id == "A03W" ~ "top",
    subplot_id == "A04E" ~ "bottom", subplot_id == "A04W" ~ "top", subplot_id == "B01E" ~ "bottom",
    subplot_id == "B01W" ~ "top", subplot_id == "B02E" ~ "top", subplot_id == "B02W" ~ "bottom",
    subplot_id == "B03E" ~ "bottom", subplot_id == "B03W" ~ "top", subplot_id == "B04E" ~ "top",
    subplot_id == "B04W" ~ "bottom", subplot_id == "C01E" ~ "top", subplot_id == "C01W" ~ "bottom",
    subplot_id == "C02E" ~ "bottom", subplot_id == "C02W" ~ "top", subplot_id == "C03E" ~ "bottom",
    subplot_id == "C03W" ~ "top", subplot_id == "C04E" ~ "top", subplot_id == "C04W" ~ "bottom", 
    subplot_id == "D01E" ~ "bottom", subplot_id == "D01W" ~ "top", subplot_id == "D02E" ~ "bottom",
    subplot_id == "D02W" ~ "top", subplot_id == "D03E" ~ "bottom", subplot_id == "D03W" ~ "top",
    subplot_id == "D04E" ~ "top", subplot_id == "D04W" ~ "bottom"
  )) %>% 
  mutate(year = "2022")




####################################################2021 Subcanopy#########################
# 2021 increment
# Load in subcanopy data
subcanopy_raw_2021 <- read.csv("Data_NPP/fd_subcanopy_diameter_2021.csv")


# change dbh_mm from character to numeric (idk why it's a character) and date 
# into date format
subcanopy_raw_2021$dbh_mm <- as.numeric(subcanopy_raw_2021$dbh_mm)
subcanopy_raw_2021$date <- as.Date(subcanopy_raw_2021$date, "%Y-%m-%d")

# Next, select important columns from subcanopy data. 
# Then remove records where DBH_mm was NA. 
# Next I convert DBH in mm to cm, and use my biomass_a function to estimate biomass(kg)
subcanopy_biomass_2021 <- subcanopy_raw_2021 %>% 
  select(subplot_id,  species, tag, dbh_mm, date) %>% 
  filter(!is.na(dbh_mm)) %>% 
  mutate(dbh_cm = dbh_mm/10) %>%
  mutate(biomass_kg = biomass_a(species, dbh_cm)) %>% 
  filter(date == "2021-11-12" | date == "2021-11-13" | date == "2021-11-15")

# next step: pull in previous year's subcanopy data and look at the incremental
# growth to see change in biomass --> NPP

# Load in data from 2020 and filter just the fall data 
subcanopy_2020 <- read.csv("Data_NPP/fd_subcanopy_diameter_2020.csv") %>% 
  filter(date == "2020-11-18" | date == "2020-11-19")
  
# Now repeat the same code from 2021 to get dbh_cm and then biomass
subcanopy_biomass_2020 <- subcanopy_2020 %>% 
  select(subplot_id,  species, tag, dbh_mm, date) %>% 
  filter(!is.na(dbh_mm)) %>% 
  mutate(dbh_cm = dbh_mm/10) %>%
  mutate(biomass_kg = biomass_a(species, dbh_cm)) 

# First gotta convert date to date again for 2020
subcanopy_biomass_2020$date <- as.Date(subcanopy_biomass_2020$date, "%Y-%m-%d")

# Join these two DFs into one
subcanopy_inc_2021 <- bind_rows(subcanopy_biomass_2020, subcanopy_biomass_2021) %>% 
  arrange(tag, date) %>% group_by(tag) %>% 
  mutate(biomass_inc = biomass_kg - lag(biomass_kg, default = first(biomass_kg))) %>% 
  filter(row_number() == n()) %>% 
  select(subplot_id, species, tag, biomass_kg, biomass_inc) %>% 
  arrange(subplot_id) 

# Get rid of negative numbers
subcanopy_inc_2021$biomass_inc[subcanopy_inc_2021$biomass_inc < 0] <- 0

# Turn this increment into NPP for the subcanopy!

# Then I just copied this code from Max in 1-8 cm RScript; this is basically filling
# in the number of stem counts per subplot so that we can "count" how much the
# unmeasured stems grew by using the average growth of the subcanopy trees we did
# measure 
NPP_sc_2021 <- subcanopy_inc_2021 %>% 
  group_by(subplot_id) %>% 
  summarize(mean_subplot = mean(biomass_inc)) %>% 
  left_join(subcanopy_inc_2021) %>% 
  right_join(tree_counts) %>% arrange(subplot_id, species) %>% 
  group_by(subplot_id) %>% fill(mean_subplot, .direction = "updown") %>% 
  group_by(subplot_id, species) %>% 
  mutate(biomass_inc_obs_est = case_when(
    !is.na(biomass_inc) ~ biomass_inc,
    is.na(biomass_inc) ~ mean_subplot
  )) %>% 
  summarize(mean_biomass = mean(biomass_inc_obs_est)) %>% 
  right_join(tree_counts) %>% 
  mutate(sp_biomass_ha = mean_biomass * count * 40) %>% 
  # * species count * 40 b/c counts were in 0.025ha
  group_by(subplot_id) %>% 
  summarize(kgC_ha_yr = sum(sp_biomass_ha) * 0.48) %>% 
  rename(NPP = kgC_ha_yr) %>% 
  mutate(severity = case_when(
    subplot_id == "A01E" ~ "85", subplot_id == "A01W" ~ "85", subplot_id == "A02E" ~ "45",
    subplot_id == "A02W" ~ "45", subplot_id == "A03E" ~ "65", subplot_id == "A03W" ~ "65",
    subplot_id == "A04E" ~ "0", subplot_id == "A04W" ~ "0", subplot_id == "B01E" ~ "0",
    subplot_id == "B01W" ~ "0", subplot_id == "B02E" ~ "45", subplot_id == "B02W" ~ "45",
    subplot_id == "B03E" ~ "85", subplot_id == "B03W" ~ "85", subplot_id == "B04E" ~ "65",
    subplot_id == "B04W" ~ "65", subplot_id == "C01E" ~ "0", subplot_id == "C01W" ~ "0",
    subplot_id == "C02E" ~ "65", subplot_id == "C02W" ~ "65", subplot_id == "C03E" ~ "85",
    subplot_id == "C03W" ~ "85", subplot_id == "C04E" ~ "45", subplot_id == "C04W" ~ "45", 
    subplot_id == "D01E" ~ "0", subplot_id == "D01W" ~ "0", subplot_id == "D02E" ~ "85",
    subplot_id == "D02W" ~ "85", subplot_id == "D03E" ~ "45", subplot_id == "D03W" ~ "45",
    subplot_id == "D04E" ~ "65", subplot_id == "D04W" ~ "65"
  )) %>% 
  mutate(treatment = case_when(
    subplot_id == "A01E" ~ "bottom", subplot_id == "A01W" ~ "top", subplot_id == "A02E" ~ "top",
    subplot_id == "A02W" ~ "bottom", subplot_id == "A03E" ~ "bottom", subplot_id == "A03W" ~ "top",
    subplot_id == "A04E" ~ "bottom", subplot_id == "A04W" ~ "top", subplot_id == "B01E" ~ "bottom",
    subplot_id == "B01W" ~ "top", subplot_id == "B02E" ~ "top", subplot_id == "B02W" ~ "bottom",
    subplot_id == "B03E" ~ "bottom", subplot_id == "B03W" ~ "top", subplot_id == "B04E" ~ "top",
    subplot_id == "B04W" ~ "bottom", subplot_id == "C01E" ~ "top", subplot_id == "C01W" ~ "bottom",
    subplot_id == "C02E" ~ "bottom", subplot_id == "C02W" ~ "top", subplot_id == "C03E" ~ "bottom",
    subplot_id == "C03W" ~ "top", subplot_id == "C04E" ~ "top", subplot_id == "C04W" ~ "bottom", 
    subplot_id == "D01E" ~ "bottom", subplot_id == "D01W" ~ "top", subplot_id == "D02E" ~ "bottom",
    subplot_id == "D02W" ~ "top", subplot_id == "D03E" ~ "bottom", subplot_id == "D03W" ~ "top",
    subplot_id == "D04E" ~ "top", subplot_id == "D04W" ~ "bottom"
  )) %>% 
  mutate(year = "2021")


# Here's a data frame of NPP subcanopy by replicate
NPP_sc_rep <- NPP_sc_2021 %>% 
  mutate(replicate = substr(subplot_id, 1, 1)) %>% 
  group_by(replicate) %>% 
  summarise(NPP_rep = mean(NPP)) # WAIT DOES THIS NEED TO BE THE SUM OR THE MEAN??? OR A COMBO OF BOTH???

write.csv(NPP_sc_2021, "Data_NPP/NPP_sc_2021.csv", row.names = FALSE)

# 2020 incremenet
# Load in subcanopy data
subcanopy_raw_19 <- read.csv("Data_NPP/fd_subcanopy_diameter_2019.csv")

# Next, select important columns from subcanopy data. 
# Then remove records where DBH_mm was NA. 
# Next I convert DBH in mm to cm, and use my biomass_a function to estimate biomass(kg)

subcanopy_biomass_2019 <- subcanopy_raw_19 %>% 
  select(subplot_id,  species, tag, dbh_mm, date) %>% 
  filter(!is.na(dbh_mm)) %>% 
  mutate(dbh_cm = dbh_mm/10) %>%
  mutate(biomass_kg = biomass_a(species, dbh_cm)) %>% 
  filter(date == "2019-08-03" | date == "2019-08-04")

subcanopy_biomass_2019$date <- as.Date(subcanopy_biomass_2019$date, "%Y-%m-%d")

# Join these two DFs into one
subcanopy_inc_20 <- bind_rows(subcanopy_biomass_2020, subcanopy_biomass_2019) %>% 
  subset(tag != 3166) %>% # deleted this row because I only had one data point
  arrange(tag, date) %>% group_by(tag) %>% 
  mutate(biomass_inc = biomass_kg - lag(biomass_kg, default = first(biomass_kg))) %>% 
  filter(row_number() == n()) %>% 
  select(subplot_id, species, tag, biomass_kg, biomass_inc) %>% 
  arrange(subplot_id) 

# Get rid of negative numbers
subcanopy_inc_20$biomass_inc[subcanopy_inc_20$biomass_inc < 0] <- 0

# Turn this increment into NPP for the subcanopy!
NPP_sc_2020 <- subcanopy_inc_20 %>% 
  group_by(subplot_id) %>% 
  summarize(mean_subplot = mean(biomass_inc)) %>% 
  left_join(subcanopy_inc_20) %>% 
  right_join(tree_counts) %>% arrange(subplot_id, species) %>% 
  group_by(subplot_id) %>% fill(mean_subplot, .direction = "updown") %>% 
  group_by(subplot_id, species) %>% 
  mutate(biomass_inc_obs_est = case_when(
    !is.na(biomass_inc) ~ biomass_inc,
    is.na(biomass_inc) ~ mean_subplot
  )) %>% 
  summarize(mean_biomass = mean(biomass_inc_obs_est)) %>% 
  right_join(tree_counts) %>% 
  mutate(sp_biomass_ha = mean_biomass * count * 40) %>% 
  # * species count * 40 b/c counts were in 0.025ha
  group_by(subplot_id) %>% 
  summarize(kgC_ha_yr = sum(sp_biomass_ha) * 0.48) %>% 
  rename(NPP = kgC_ha_yr) %>% 
  mutate(severity = case_when(
    subplot_id == "A01E" ~ "85", subplot_id == "A01W" ~ "85", subplot_id == "A02E" ~ "45",
    subplot_id == "A02W" ~ "45", subplot_id == "A03E" ~ "65", subplot_id == "A03W" ~ "65",
    subplot_id == "A04E" ~ "0", subplot_id == "A04W" ~ "0", subplot_id == "B01E" ~ "0",
    subplot_id == "B01W" ~ "0", subplot_id == "B02E" ~ "45", subplot_id == "B02W" ~ "45",
    subplot_id == "B03E" ~ "85", subplot_id == "B03W" ~ "85", subplot_id == "B04E" ~ "65",
    subplot_id == "B04W" ~ "65", subplot_id == "C01E" ~ "0", subplot_id == "C01W" ~ "0",
    subplot_id == "C02E" ~ "65", subplot_id == "C02W" ~ "65", subplot_id == "C03E" ~ "85",
    subplot_id == "C03W" ~ "85", subplot_id == "C04E" ~ "45", subplot_id == "C04W" ~ "45", 
    subplot_id == "D01E" ~ "0", subplot_id == "D01W" ~ "0", subplot_id == "D02E" ~ "85",
    subplot_id == "D02W" ~ "85", subplot_id == "D03E" ~ "45", subplot_id == "D03W" ~ "45",
    subplot_id == "D04E" ~ "65", subplot_id == "D04W" ~ "65"
  )) %>% 
  mutate(treatment = case_when(
    subplot_id == "A01E" ~ "bottom", subplot_id == "A01W" ~ "top", subplot_id == "A02E" ~ "top",
    subplot_id == "A02W" ~ "bottom", subplot_id == "A03E" ~ "bottom", subplot_id == "A03W" ~ "top",
    subplot_id == "A04E" ~ "bottom", subplot_id == "A04W" ~ "top", subplot_id == "B01E" ~ "bottom",
    subplot_id == "B01W" ~ "top", subplot_id == "B02E" ~ "top", subplot_id == "B02W" ~ "bottom",
    subplot_id == "B03E" ~ "bottom", subplot_id == "B03W" ~ "top", subplot_id == "B04E" ~ "top",
    subplot_id == "B04W" ~ "bottom", subplot_id == "C01E" ~ "top", subplot_id == "C01W" ~ "bottom",
    subplot_id == "C02E" ~ "bottom", subplot_id == "C02W" ~ "top", subplot_id == "C03E" ~ "bottom",
    subplot_id == "C03W" ~ "top", subplot_id == "C04E" ~ "top", subplot_id == "C04W" ~ "bottom", 
    subplot_id == "D01E" ~ "bottom", subplot_id == "D01W" ~ "top", subplot_id == "D02E" ~ "bottom",
    subplot_id == "D02W" ~ "top", subplot_id == "D03E" ~ "bottom", subplot_id == "D03W" ~ "top",
    subplot_id == "D04E" ~ "top", subplot_id == "D04W" ~ "bottom"
  )) %>% 
  mutate(year = "2020")


# For 2019 I "cheated" and just exported Max's df as a .csv into old_data
NPP_sc_2019 <- read.csv("Data_NPP/NPP_sc_2019.csv") %>% 
  rename(subplot_id = subplot) %>% 
  mutate(severity = case_when(
    subplot_id == "A01E" ~ "85", subplot_id == "A01W" ~ "85", subplot_id == "A02E" ~ "45",
    subplot_id == "A02W" ~ "45", subplot_id == "A03E" ~ "65", subplot_id == "A03W" ~ "65",
    subplot_id == "A04E" ~ "0", subplot_id == "A04W" ~ "0", subplot_id == "B01E" ~ "0",
    subplot_id == "B01W" ~ "0", subplot_id == "B02E" ~ "45", subplot_id == "B02W" ~ "45",
    subplot_id == "B03E" ~ "85", subplot_id == "B03W" ~ "85", subplot_id == "B04E" ~ "65",
    subplot_id == "B04W" ~ "65", subplot_id == "C01E" ~ "0", subplot_id == "C01W" ~ "0",
    subplot_id == "C02E" ~ "65", subplot_id == "C02W" ~ "65", subplot_id == "C03E" ~ "85",
    subplot_id == "C03W" ~ "85", subplot_id == "C04E" ~ "45", subplot_id == "C04W" ~ "45", 
    subplot_id == "D01E" ~ "0", subplot_id == "D01W" ~ "0", subplot_id == "D02E" ~ "85",
    subplot_id == "D02W" ~ "85", subplot_id == "D03E" ~ "45", subplot_id == "D03W" ~ "45",
    subplot_id == "D04E" ~ "65", subplot_id == "D04W" ~ "65"
  )) %>% 
  mutate(treatment = case_when(
    subplot_id == "A01E" ~ "bottom", subplot_id == "A01W" ~ "top", subplot_id == "A02E" ~ "top",
    subplot_id == "A02W" ~ "bottom", subplot_id == "A03E" ~ "bottom", subplot_id == "A03W" ~ "top",
    subplot_id == "A04E" ~ "bottom", subplot_id == "A04W" ~ "top", subplot_id == "B01E" ~ "bottom",
    subplot_id == "B01W" ~ "top", subplot_id == "B02E" ~ "top", subplot_id == "B02W" ~ "bottom",
    subplot_id == "B03E" ~ "bottom", subplot_id == "B03W" ~ "top", subplot_id == "B04E" ~ "top",
    subplot_id == "B04W" ~ "bottom", subplot_id == "C01E" ~ "top", subplot_id == "C01W" ~ "bottom",
    subplot_id == "C02E" ~ "bottom", subplot_id == "C02W" ~ "top", subplot_id == "C03E" ~ "bottom",
    subplot_id == "C03W" ~ "top", subplot_id == "C04E" ~ "top", subplot_id == "C04W" ~ "bottom", 
    subplot_id == "D01E" ~ "bottom", subplot_id == "D01W" ~ "top", subplot_id == "D02E" ~ "bottom",
    subplot_id == "D02W" ~ "top", subplot_id == "D03E" ~ "bottom", subplot_id == "D03W" ~ "top",
    subplot_id == "D04E" ~ "top", subplot_id == "D04W" ~ "bottom"
  )) %>% 
rename(NPP = NPP_subcan) %>% 
  mutate(year = "2019")


######################################### Seedling and Sapling NPP 2021 and 2020 ##############

# Load in data

seeds_saps_22 <- read.csv("Data_NPP/seedling_2022.csv")
seeds_saps_21 <- read.csv("Data_NPP/fd_seedling_sapling_2021.csv")
seeds_saps_20 <- read.csv("Data_NPP/fd_seedling_sapling_2020.csv")


# Convert date from character to date
seeds_saps_22$date <- as.Date(as.character(seeds_saps_22$date,"%Y-%m-%d"))
seeds_saps_21$date <- as.Date(as.character(seeds_saps_21$date,"%Y-%m-%d"))
seeds_saps_20$date <- as.Date(as.character(seeds_saps_20$date,"%Y-%m-%d"))

# Max didn't actually measure the basal diameter, so I have to adjust the df
# to use the bins he put basal diameter in. 

seeds_saps_20 <-  seeds_saps_20 %>% 
  select(subplot_id, nested_subplot, species, basal_diameter_cm, height_total_cm, date) %>% 
  filter(!is.na(height_total_cm)) %>% 
  filter(!is.na(species)) %>% 
  mutate(D_cm = case_when(
    basal_diameter_cm == "0-1" ~ 0.5,
    basal_diameter_cm == "2-Jan" ~ 1.5, # something happened in Max's original excel where it automatically converted the bin 1-2 into January 2nd
    basal_diameter_cm == "3-Feb" ~ 2.5 # same as above
  )) %>% 
  rename(height_2020 = height_total_cm) %>% 
  rename(veg_plot = nested_subplot) %>% 
  select(subplot_id, veg_plot, species, height_2020, date, D_cm)

seeds_saps_20$veg_plot <-as.character(seeds_saps_20$veg_plot) # makes this a character to match my data later




# And now because 2020 is in bins, I gotta put 2021 & 2022 in bins too to be comparable
bins <- c(0,1,2,3)
names <- c("0-1", "1-2", "2-3")

seeds_saps_21 <- seeds_saps_21 %>% 
  filter(!is.na(height_2021)) %>% 
  filter(!is.na(species)) %>% 
  mutate(D_cm = base_D/10) # turn basal diameter in mm into cm

seeds_saps_22 <- seeds_saps_22 %>% 
  filter(!is.na(species))%>%
  filter(species != "abba")%>%
  filter(!is.na(base_D))%>%
  filter(!is.na(height))%>%
  mutate(D_cm = base_D/10)%>% # turn basal diameter in mm into cm
  mutate(height_cm = height/10)# turn height from mm into cm



seeds_saps_21$D_cm <- cut(seeds_saps_21$D_cm, breaks = bins, labels = names) 
seeds_saps_22$D_cm <- cut(seeds_saps_22$D_cm, breaks = bins, labels = names) 

# this bins everything up properly for analysis
#2021
seeds_saps_21 <- seeds_saps_21 %>% 
  mutate(D_cm = case_when(
    D_cm == "0-1" ~ 0.5,
    D_cm == "1-2" ~ 1.5, 
    D_cm == "2-3" ~ 2.5 
  )) # and this just does the same thing that Max did to his bins

#2022
seeds_saps_22 <- seeds_saps_22 %>% 
  mutate(D_cm = case_when(
    D_cm == "0-1" ~ 0.5,
    D_cm == "1-2" ~ 1.5, 
    D_cm == "2-3" ~ 2.5 
  )) # and this just does the same thing that Max did to his bins

# Add allometric equations (2020)
seedling_biomass_2020 <- seeds_saps_20 %>% 
  mutate(volume_2020_cm3 = pi*((0.5*D_cm)^2)*(height_2020/3)) %>% 
  mutate(density_g_cm3 = case_when( 
    species == "PIRE" ~ 0.41,
    species == "ACPE" ~ 0.44,
    species == "ACRU" ~ 0.49,
    species == "ACSA" ~ 0.56,
    species == "FRPE" ~ 0.53,
    species == "POGR" ~ 0.36,
    species == "QURU" ~ 0.56,
    species == "AMEL" ~ 0.66,
    species == "FAGR" ~ 0.56,
    species == "FRAM" ~ 0.55, 
    species == "PIST" ~ 0.34
  )) %>% # not actually sure where these densities came from?
  mutate(biomass_kg = (volume_2020_cm3*density_g_cm3)/1000) %>% 
  mutate(kgC = biomass_kg * 0.48) %>%  # 0.48 is the ratio of biomass to C
  mutate(year = "2020") %>%  # add a year column for later
  group_by(subplot_id) %>% 
  mutate(total_kgC_subplot = sum(kgC, na.rm = TRUE)) %>% 
  mutate(kgC_m2 = total_kgC_subplot/4) # kgC per m^2 done by dividing the total by 4, 
# which is the number of 1x1 m plots Max measured in each subplot

# Do the same allometric equations (2021)
seedling_biomass_2021 <- seeds_saps_21 %>% 
  mutate(volume_2021_cm3 = pi*((0.5*D_cm)^2)*(height_2021/3)) %>% 
  mutate(density_g_cm3 = case_when( 
    species == "PIRE" ~ 0.41,
    species == "ACPE" ~ 0.44,
    species == "ACRU" ~ 0.49,
    species == "ACSA" ~ 0.56,
    species == "FRPE" ~ 0.53,
    species == "POGR" ~ 0.36,
    species == "QURU" ~ 0.56,
    species == "AMEL" ~ 0.66,
    species == "FAGR" ~ 0.56,
    species == "FRAM" ~ 0.55, 
    species == "PIST" ~ 0.34
  )) %>% 
  mutate(biomass_kg = (volume_2021_cm3*density_g_cm3)/1000) %>% 
  mutate(kgC = biomass_kg * 0.48) %>% 
  mutate(year = "2021") %>% 
  group_by(subplot_id) %>% 
  mutate(total_kgC_subplot = sum(kgC, na.rm = TRUE)) %>% 
  mutate(kgC_m2 = case_when(
    subplot_id == "A03E" ~ total_kgC_subplot/16,
    subplot_id != "A03E" ~ total_kgC_subplot/8
  )) # In A03E, we measured every single 1x1 m square, in the rest we only measured 8 per subplot

# Do the same allometric equations (2022)
seedling_biomass_2022 <- seeds_saps_22 %>% 
  mutate(volume_2021_cm3 = pi*((0.5*D_cm)^2)*(height_cm/3)) %>% 
  mutate(density_g_cm3 = case_when( 
    species == "acpe" ~ 0.44,
    species == "acru" ~ 0.49,
    species == "acsa" ~ 0.56,
    species == "pogr" ~ 0.36,
    species == "quru" ~ 0.56,
    species == "amel" ~ 0.66,
    species == "fagr" ~ 0.56,
    species == "fram" ~ 0.55, 
    species == "pist" ~ 0.34
  )) %>% 
  mutate(biomass_kg = (volume_2021_cm3*density_g_cm3)/1000) %>% 
  mutate(kgC = biomass_kg * 0.48) %>% 
  mutate(year = "2022") %>% 
  group_by(subplot_id) %>% 
  mutate(total_kgC_subplot = sum(kgC, na.rm = TRUE)) %>% 
  mutate(kgC_m2 =  total_kgC_subplot/8)

##2022 Seedling Data ########
# Find the change in biomass by subplot:bind the data frames together
# Make a new row finding the change in kgC to use for NPP calculations
seeds_saps_inc_22 <- bind_rows(seedling_biomass_2021, seedling_biomass_2022) %>% 
  select(subplot_id, kgC_m2, year) %>% 
  group_by(subplot_id, year) %>% 
  distinct(kgC_m2) %>%   # gets rid of repeat rows
  ungroup() %>% 
  group_by(subplot_id) %>% 
  arrange(subplot_id) %>% 
  mutate(change_in_kgC_m2 = kgC_m2 - lag(kgC_m2))

### OK so now we're finally able to calculate NPP using the biomass increment
NPP_seedlings_22 <- seeds_saps_inc_22 %>% 
  mutate(kgC_ha = change_in_kgC_m2*10000) %>%  # turns the change in kgC/m2 to change in kgC/ha
  mutate(severity = case_when(
    subplot_id == "A01E" ~ "85", subplot_id == "A01W" ~ "85", subplot_id == "A02E" ~ "45",
    subplot_id == "A02W" ~ "45", subplot_id == "A03E" ~ "65", subplot_id == "A03W" ~ "65",
    subplot_id == "A04E" ~ "0", subplot_id == "A04W" ~ "0", subplot_id == "B01E" ~ "0",
    subplot_id == "B01W" ~ "0", subplot_id == "B02E" ~ "45", subplot_id == "B02W" ~ "45",
    subplot_id == "B03E" ~ "85", subplot_id == "B03W" ~ "85", subplot_id == "B04E" ~ "65",
    subplot_id == "B04W" ~ "65", subplot_id == "C01E" ~ "0", subplot_id == "C01W" ~ "0",
    subplot_id == "C02E" ~ "65", subplot_id == "C02W" ~ "65", subplot_id == "C03E" ~ "85",
    subplot_id == "C03W" ~ "85", subplot_id == "C04E" ~ "45", subplot_id == "C04W" ~ "45", 
    subplot_id == "D01E" ~ "0", subplot_id == "D01W" ~ "0", subplot_id == "D02E" ~ "85",
    subplot_id == "D02W" ~ "85", subplot_id == "D03E" ~ "45", subplot_id == "D03W" ~ "45",
    subplot_id == "D04E" ~ "65", subplot_id == "D04W" ~ "65"
  )) %>% 
  mutate(treatment = case_when(
    subplot_id == "A01E" ~ "bottom", subplot_id == "A01W" ~ "top", subplot_id == "A02E" ~ "top",
    subplot_id == "A02W" ~ "bottom", subplot_id == "A03E" ~ "bottom", subplot_id == "A03W" ~ "top",
    subplot_id == "A04E" ~ "bottom", subplot_id == "A04W" ~ "top", subplot_id == "B01E" ~ "bottom",
    subplot_id == "B01W" ~ "top", subplot_id == "B02E" ~ "top", subplot_id == "B02W" ~ "bottom",
    subplot_id == "B03E" ~ "bottom", subplot_id == "B03W" ~ "top", subplot_id == "B04E" ~ "top",
    subplot_id == "B04W" ~ "bottom", subplot_id == "C01E" ~ "top", subplot_id == "C01W" ~ "bottom",
    subplot_id == "C02E" ~ "bottom", subplot_id == "C02W" ~ "top", subplot_id == "C03E" ~ "bottom",
    subplot_id == "C03W" ~ "top", subplot_id == "C04E" ~ "top", subplot_id == "C04W" ~ "bottom", 
    subplot_id == "D01E" ~ "bottom", subplot_id == "D01W" ~ "top", subplot_id == "D02E" ~ "bottom",
    subplot_id == "D02W" ~ "top", subplot_id == "D03E" ~ "bottom", subplot_id == "D03W" ~ "top",
    subplot_id == "D04E" ~ "top", subplot_id == "D04W" ~ "bottom"
  )) %>% 
  select(subplot_id, change_in_kgC_m2, kgC_ha, treatment, severity) %>% 
  filter(!is.na(kgC_ha))%>%
  mutate(year = "2022")

##2021
# Find the change in biomass by subplot:bind the data frames together
# Make a new row finding the change in kgC to use for NPP calculations
seeds_saps_inc_21 <- bind_rows(seedling_biomass_2020, seedling_biomass_2021) %>% 
  select(subplot_id, kgC_m2, year) %>% 
  group_by(subplot_id, year) %>% 
  distinct(kgC_m2) %>%   # gets rid of repeat rows
  ungroup() %>% 
  group_by(subplot_id) %>% 
  arrange(subplot_id) %>% 
  mutate(change_in_kgC_m2 = kgC_m2 - lag(kgC_m2))

### OK so now we're finally able to calculate NPP using the biomass increment
NPP_seedlings_21 <- seeds_saps_inc_21 %>% 
  mutate(kgC_ha = change_in_kgC_m2*10000) %>%  # turns the change in kgC/m2 to change in kgC/ha
  mutate(severity = case_when(
    subplot_id == "A01E" ~ "85", subplot_id == "A01W" ~ "85", subplot_id == "A02E" ~ "45",
    subplot_id == "A02W" ~ "45", subplot_id == "A03E" ~ "65", subplot_id == "A03W" ~ "65",
    subplot_id == "A04E" ~ "0", subplot_id == "A04W" ~ "0", subplot_id == "B01E" ~ "0",
    subplot_id == "B01W" ~ "0", subplot_id == "B02E" ~ "45", subplot_id == "B02W" ~ "45",
    subplot_id == "B03E" ~ "85", subplot_id == "B03W" ~ "85", subplot_id == "B04E" ~ "65",
    subplot_id == "B04W" ~ "65", subplot_id == "C01E" ~ "0", subplot_id == "C01W" ~ "0",
    subplot_id == "C02E" ~ "65", subplot_id == "C02W" ~ "65", subplot_id == "C03E" ~ "85",
    subplot_id == "C03W" ~ "85", subplot_id == "C04E" ~ "45", subplot_id == "C04W" ~ "45", 
    subplot_id == "D01E" ~ "0", subplot_id == "D01W" ~ "0", subplot_id == "D02E" ~ "85",
    subplot_id == "D02W" ~ "85", subplot_id == "D03E" ~ "45", subplot_id == "D03W" ~ "45",
    subplot_id == "D04E" ~ "65", subplot_id == "D04W" ~ "65"
  )) %>% 
  mutate(treatment = case_when(
    subplot_id == "A01E" ~ "bottom", subplot_id == "A01W" ~ "top", subplot_id == "A02E" ~ "top",
    subplot_id == "A02W" ~ "bottom", subplot_id == "A03E" ~ "bottom", subplot_id == "A03W" ~ "top",
    subplot_id == "A04E" ~ "bottom", subplot_id == "A04W" ~ "top", subplot_id == "B01E" ~ "bottom",
    subplot_id == "B01W" ~ "top", subplot_id == "B02E" ~ "top", subplot_id == "B02W" ~ "bottom",
    subplot_id == "B03E" ~ "bottom", subplot_id == "B03W" ~ "top", subplot_id == "B04E" ~ "top",
    subplot_id == "B04W" ~ "bottom", subplot_id == "C01E" ~ "top", subplot_id == "C01W" ~ "bottom",
    subplot_id == "C02E" ~ "bottom", subplot_id == "C02W" ~ "top", subplot_id == "C03E" ~ "bottom",
    subplot_id == "C03W" ~ "top", subplot_id == "C04E" ~ "top", subplot_id == "C04W" ~ "bottom", 
    subplot_id == "D01E" ~ "bottom", subplot_id == "D01W" ~ "top", subplot_id == "D02E" ~ "bottom",
    subplot_id == "D02W" ~ "top", subplot_id == "D03E" ~ "bottom", subplot_id == "D03W" ~ "top",
    subplot_id == "D04E" ~ "top", subplot_id == "D04W" ~ "bottom"
  )) %>% 
  select(subplot_id, change_in_kgC_m2, kgC_ha, treatment, severity) %>% 
  filter(!is.na(kgC_ha))%>%
  mutate(year = "2021")


####### Seedling and Sapling NPP 2020 ######
# Load in data
seeds_saps_19 <- read.csv("Data_NPP/fd_seedling_sapling_2019.csv")

# Convert date from character to date
seeds_saps_19$date <- as.Date(as.character(seeds_saps_19$date,"%Y-%m-%d"))

# making the data frames match
seeds_saps_19 <-  seeds_saps_19 %>% 
  select(subplot_id, nested_subplot, species, basal_diameter_cm, height_total_cm, date) %>% 
  filter(date == "2019-08-06" | date == "2019-08-05" | date == "2019-08-02") %>% 
  filter(!is.na(height_total_cm)) %>% 
  filter(!is.na(species)) %>% 
  mutate(D_cm = case_when(
    basal_diameter_cm == "0-1" ~ 0.5,
    basal_diameter_cm == "2-Jan" ~ 1.5, # something happened in Max's original excel where it automatically converted the bin 1-2 into January 2nd
    basal_diameter_cm == "3-Feb" ~ 2.5 # same as above
  )) %>% 
  rename(height_2019 = height_total_cm) %>% 
  rename(veg_plot = nested_subplot) %>% 
  select(subplot_id, veg_plot, species, height_2019, date, D_cm)

seeds_saps_19$veg_plot <-as.character(seeds_saps_19$veg_plot) # makes this a character to match my data later

# Add allometric equations (2020)
seedling_biomass_2019 <- seeds_saps_19 %>% 
  mutate(volume_2019_cm3 = pi*((0.5*D_cm)^2)*(height_2019/3)) %>% 
  mutate(density_g_cm3 = case_when( 
    species == "PIRE" ~ 0.41,
    species == "ACPE" ~ 0.44,
    species == "ACRU" ~ 0.49,
    species == "ACSA" ~ 0.56,
    species == "FRPE" ~ 0.53,
    species == "POGR" ~ 0.36,
    species == "QURU" ~ 0.56,
    species == "AMEL" ~ 0.66,
    species == "FAGR" ~ 0.56,
    species == "FRAM" ~ 0.55, 
    species == "PIST" ~ 0.34
  )) %>% # not actually sure where these densities came from?
  mutate(biomass_kg = (volume_2019_cm3*density_g_cm3)/1000) %>% 
  mutate(kgC = biomass_kg * 0.48) %>%  # 0.48 is the ratio of biomass to C
  mutate(year = "2019") %>%  # add a year column for later
  group_by(subplot_id) %>% 
  mutate(total_kgC_subplot = sum(kgC, na.rm = TRUE)) %>% 
  mutate(kgC_m2 = total_kgC_subplot/4) # kgC per m^2 done by dividing the total by 4, 
# which is the number of 1x1 m plots Max measured in each subplot


# Find the change in biomass by subplot:bind the data frames together
# Make a new row finding the change in kgC to use for NPP calculations
seeds_saps_inc_20 <- bind_rows(seedling_biomass_2019, seedling_biomass_2020) %>% 
  select(subplot_id, kgC_m2, year) %>% 
  group_by(subplot_id, year) %>% 
  distinct(kgC_m2) %>%   # gets rid of repeat rows
  ungroup() %>% 
  group_by(subplot_id) %>% 
  arrange(subplot_id) %>% 
  mutate(change_in_kgC_m2 = kgC_m2 - lag(kgC_m2))


### OK so now calculate NPP as an increment

NPP_seedlings_20 <- seeds_saps_inc_20 %>% 
  mutate(kgC_ha = change_in_kgC_m2*10000) %>%  # turns the change in C/m2 to change in C/ha
  mutate(severity = case_when(
    subplot_id == "A01E" ~ "85", subplot_id == "A01W" ~ "85", subplot_id == "A02E" ~ "45",
    subplot_id == "A02W" ~ "45", subplot_id == "A03E" ~ "65", subplot_id == "A03W" ~ "65",
    subplot_id == "A04E" ~ "0", subplot_id == "A04W" ~ "0", subplot_id == "B01E" ~ "0",
    subplot_id == "B01W" ~ "0", subplot_id == "B02E" ~ "45", subplot_id == "B02W" ~ "45",
    subplot_id == "B03E" ~ "85", subplot_id == "B03W" ~ "85", subplot_id == "B04E" ~ "65",
    subplot_id == "B04W" ~ "65", subplot_id == "C01E" ~ "0", subplot_id == "C01W" ~ "0",
    subplot_id == "C02E" ~ "65", subplot_id == "C02W" ~ "65", subplot_id == "C03E" ~ "85",
    subplot_id == "C03W" ~ "85", subplot_id == "C04E" ~ "45", subplot_id == "C04W" ~ "45", 
    subplot_id == "D01E" ~ "0", subplot_id == "D01W" ~ "0", subplot_id == "D02E" ~ "85",
    subplot_id == "D02W" ~ "85", subplot_id == "D03E" ~ "45", subplot_id == "D03W" ~ "45",
    subplot_id == "D04E" ~ "65", subplot_id == "D04W" ~ "65"
  )) %>% 
  mutate(treatment = case_when(
    subplot_id == "A01E" ~ "bottom", subplot_id == "A01W" ~ "top", subplot_id == "A02E" ~ "top",
    subplot_id == "A02W" ~ "bottom", subplot_id == "A03E" ~ "bottom", subplot_id == "A03W" ~ "top",
    subplot_id == "A04E" ~ "bottom", subplot_id == "A04W" ~ "top", subplot_id == "B01E" ~ "bottom",
    subplot_id == "B01W" ~ "top", subplot_id == "B02E" ~ "top", subplot_id == "B02W" ~ "bottom",
    subplot_id == "B03E" ~ "bottom", subplot_id == "B03W" ~ "top", subplot_id == "B04E" ~ "top",
    subplot_id == "B04W" ~ "bottom", subplot_id == "C01E" ~ "top", subplot_id == "C01W" ~ "bottom",
    subplot_id == "C02E" ~ "bottom", subplot_id == "C02W" ~ "top", subplot_id == "C03E" ~ "bottom",
    subplot_id == "C03W" ~ "top", subplot_id == "C04E" ~ "top", subplot_id == "C04W" ~ "bottom", 
    subplot_id == "D01E" ~ "bottom", subplot_id == "D01W" ~ "top", subplot_id == "D02E" ~ "bottom",
    subplot_id == "D02W" ~ "top", subplot_id == "D03E" ~ "bottom", subplot_id == "D03W" ~ "top",
    subplot_id == "D04E" ~ "top", subplot_id == "D04W" ~ "bottom"
  )) %>% 
  select(subplot_id, change_in_kgC_m2, kgC_ha, treatment, severity) %>% 
  filter(!is.na(kgC_ha))%>%
  mutate(year = "2020")


################################# Multi-YEAR VALUES ###########################################################################
four_yr_can_NPP <- bind_rows(NPP_can_2019, NPP_can_2020, NPP_can_2021, NPP_can_2022) %>% 
  arrange(subplot_id) %>% 
  mutate(stratum = "canopy")

four_yr_coarse_root_NPP <- bind_rows(NPP_coarse_root_2019, NPP_coarse_root_2020, NPP_coarse_root_2021, NPP_coarse_root_2022)%>%
  rename(NPP_MgC_ha_yr = NPP)

four_yr_sc_NPP <- bind_rows(NPP_sc_2019, NPP_sc_2020, NPP_sc_2021,NPP_sc_2022 ) %>% 
  arrange(subplot_id) %>% 
  mutate(stratum = "subcanopy") %>% 
  mutate(replicate = substr(subplot_id, 1, 1))




############## TOTAL NPP #################

##create dataframe for total NPP 
four_year_NPP_can_sub <- merge(four_yr_can_NPP, four_yr_sc_NPP, by = c("subplot_id", "year", "severity", "treatment", "replicate"))%>%
  select(subplot_id, replicate, severity, treatment, year, NPP.x, NPP.y)%>%
  rename(NPP_can = NPP.x, NPP_sc = NPP.y)%>%
  mutate(NPP_total = NPP_can + NPP_sc)%>%
  mutate(NPP_total_MgC_ha_yr = NPP_total/1000)

write.csv(four_year_NPP_can_sub , "Figures_Output/NPP_total.csv", row.names=FALSE)



########################Dataframes for Summary Tables########################
four_year_NPP_can_sub_summary_severity <- four_year_NPP_can_sub%>%
  group_by(severity, year)%>%
  summarize(NPP_ave_Mghayr = mean(NPP_total_MgC_ha_yr), NPP_se = std.error(NPP_total_MgC_ha_yr))

four_year_NPP_can_sub_summary_treatment <- four_year_NPP_can_sub%>%
  group_by(treatment, year)%>%
  summarize(NPP_ave_Mghayr = mean(NPP_total_MgC_ha_yr), NPP_se = std.error(NPP_total_MgC_ha_yr))

four_yr_coarse_root_NPP_summary_severity <- four_yr_coarse_root_NPP%>%
  group_by(severity, year)%>%
  summarize(NPP_ave_Mghayr = mean(NPP_MgC_ha_yr), NPP_se = std.error(NPP_MgC_ha_yr))

four_yr_coarse_root_NPP_summary_treatment <- four_yr_coarse_root_NPP%>%
  group_by(treatment, year)%>%
  summarize(NPP_ave_Mghayr = mean(NPP_MgC_ha_yr), NPP_se = std.error(NPP_MgC_ha_yr))


############################################################# FIGURES ###########################################################################


forte_pal <- forte_colors()

##canopy
ggplot(four_yr_can_NPP, aes(x = severity, y = NPP, fill = severity)) +
  geom_boxplot()+
  facet_grid(~year)+
  theme_classic() +
  scale_fill_manual(values =forte_pal) +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
ylab("Canopy ANPPw")

##coarse roots
ggplot(four_yr_coarse_root_NPP, aes(x = severity, y = NPP, fill = severity)) +
  geom_boxplot()+
  facet_grid(~year)+
  theme_classic() +
  scale_fill_manual(values =forte_pal) +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30))




ggsave(path = "Figures_Output", filename = "NPP_canopy.png", height = 10, width =20, units = "in")

##Subcanopy
ggplot(four_yr_sc_NPP,aes(x = severity, y = NPP, fill = severity)) +
  geom_boxplot()+
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal)+
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30))


ggsave(path = "Figures_Output", filename = "NPP_subcanopy.png", height = 10, width =20, units = "in")

##Total ANPP (Canopy + Subcanopy)
ggplot(four_year_NPP_can_sub, aes(x = severity, y = NPP_total_MgC_ha_yr, fill = severity)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
ylab("ANPPw (Mg C ha yr)")

ggplot(four_year_NPP_can_sub, aes(x = treatment, y = NPP_total_MgC_ha_yr, fill = treatment)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("ANPPw (Mg C ha yr)")

ggsave(path = "Figures_Output", filename = "NPP_Total.png", height = 10, width =20, units = "in")



######Total NPPw (Above + Belowground)
############Preparing a dataframe for figure script 
four_yr_total_NPPw <- merge(four_year_NPP_can_sub, four_yr_coarse_root_NPP, by = c("severity", "treatment", "subplot_id", "replicate", "year"))%>%
  rename(ANPP = NPP_total_MgC_ha_yr, BNPP = NPP_MgC_ha_yr)%>%
  mutate(NPPw = ANPP + BNPP)%>%
  select(!NPP_total)%>%
  mutate(NPP_can = NPP_can/1000)%>%
  mutate(NPP_sc = NPP_sc/1000)%>%
  mutate(type = case_when(treatment == "bottom"~ "Bottom", 
                          treatment == "top" ~ "Top"))%>%
  select(!treatment)

write.csv(four_yr_total_NPPw , "Figures_Output/NPP_total.csv", row.names=FALSE)

ggplot(four_yr_total_NPPw, aes(x = severity, y = NPPw, fill = severity)) +
geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("NPPw (Mg C ha yr)")

ggsave(path = "Figures_Output", filename = "NPP_Total.png", height = 10, width =20, units = "in")

#########Split plot Model for absolute data#####

##Transform variables into factors for model 
four_yr_total_NPPw$severity <- as.factor(four_yr_total_NPPw$severity)
four_yr_total_NPPw$treatment <- as.factor(four_yr_total_NPPw$treatment)
four_yr_total_NPPw$year <- as.factor(four_yr_total_NPPw$year)

####Testing Assumptions 
##Test for outliers test: no extreme outliers
four_yr_total_NPPw%>% 
  group_by(severity, treatment, year) %>%
  identify_outliers(NPPw)

##Equality of variance test for severity and treatment (Slightly unequal data using alpha = 0.05. Equal variance using alpha = 0.1)
leveneTest(NPPw ~ year*treatment*severity, data = four_yr_total_NPPw)

##Normality (Data are normal)
# Build the linear model
normality_test  <- lm(NPPw ~ severity*treatment*year,
                      data = four_yr_total_NPPw)

# Create a QQ plot of residuals
ggqqplot(residuals(normality_test))
# Shapiro test of normality 
shapiro_test(residuals(normality_test))



####WORKING SPLIT-SPLIT MODEL: Using aov(). Same results as the agricolae package. Ran an ANCOVA with VWC as a covariate.(However significance does not change with or without VWC). 

four_yr_total_NPPw_model <- aov(NPPw  ~ severity*treatment*year + replicate +Error(replicate:severity/treatment/year), data = four_yr_total_NPPw)

summary(four_yr_total_NPPw_model)




######Post hoc analysis: LSD test for Rs/VWC/temp models without covariates for all significant values in the model
library(agricolae)
#Rs model
out_year_severity_NPPw <- with(four_yr_total_NPPw, LSD.test(NPPw, severity:year,72, 0.763, console = TRUE))







