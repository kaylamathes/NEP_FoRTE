####CWD Calculations 


###libraries 
library(dplyr)
library(tidyr)
library(reshape2)
library(plotrix)
library(fortedata)
library(ggplot2)
library(googledrive)#upload data from googledrive 

##Run pipeline script
source("Scripts/NPP_pipeline.R") 

# Import dendroband data (here is where subsequent years' data should be added)
# You may be tempted to use the cleaned forte_data version of 2019 and 2020, but 
# lemme tell you, it's gonna fuck your shit up if you do, so don't even bother

dendro_2019 <- read.csv("Data_NPP/canopy_dendrobands_2019.csv", na.strings = c("", "NA")) %>% 
  rename(subplot_id = subplot, dbh_cm = DBH_cm, band_in = bands_in)

dendro_2020 <- read.csv("Data_NPP/canopy_dendrobands_2020.csv", na.strings = c("", "NA")) %>% 
  rename(subplot_id = subplot, dbh_cm = DBH_cm, band_in = bands_in)

dendro_2021 <- read.csv("Data_NPP/fd_canopy_dendroband_2021.csv", na.strings = c("", "NA")) 

dendro_2022 <-  read.csv("Data_NPP/canopy_dendrobands_2022.csv", na.strings = c("", "NA")) %>%
  rename(subplot_id = subplot, band_in = bottom_in_2022)

# Add a weeks column to 2021 data
dendro_2021 <-  dendro_2021 %>% 
  mutate(month = substr(date, 6, 7)) %>% 
  mutate(week = case_when(month == "06" ~ 20, month == "11" ~ 21 )) %>% 
  # the above code basically just looks at the month and assigns it the week number
  # that corresponds with the data collection number (ie in June 2021, it was the 
  # 20th time the dendrobands were measured, in Nov 2021, it was the 21st time)
  select(-month)

### Edit 2022 Data to match columns from previous years 

dendro_2022 <- dendro_2022%>%
  select(!top_in_2022)%>%
  select(!notes_2021)%>%
  rename(notes = notes_2022, date = date_2022)


###Add a week column using protocol from above 
dendro_2022 <-  dendro_2022 %>% 
  mutate(week = case_when(date == "2022-06-10" | date == "2022-06-13" |date == "2022-06-09" ~ "22", 
                          date == "2022-11-05" | date == "2022-11-06" ~ "23"))

dendro_2022$band_in <- as.numeric(dendro_2022$band_in)
dendro_2022$week <- as.numeric(dendro_2022$week)


# Bind the three years together and fill in the blanks missing from some of data sheets
dendro_data <- bind_rows(dendro_2019, dendro_2020, dendro_2021, dendro_2022)

# change date to proper Data format
dendro_data$date <- as.Date(dendro_data$date)

# arrange by tag and week for organization
dendro_data <- arrange(dendro_data, tag, week)


#selected only for columns that I will use and convert band_in to cm 
dendro_data <- select(dendro_data, week, subplot_id, tag, 
                      fate, species, dbh_cm, band_in, DOY, date, notes, status) %>%
  mutate(band_cm= band_in*2.54) 

#create uniqueID for dendrodata 
dendro_data$uniqueID <- paste(dendro_data$subplot_id, dendro_data$week,
                              dendro_data$species, dendro_data$fate, sep = "_")



##filter for trees that are standing dead or felled and were not measured for dendroband increment (Included in NPP calculation)
dendro_data_dead <- dendro_data%>%
  filter(status == "felled" | status == "stand_dead")%>%
  filter(is.na(band_in))%>%
  select(tag,week, date, subplot_id, species, status, dbh_cm)%>%
  rename(dbh_cm_18 = dbh_cm)


dendro_data_dead$year <- as.factor(format(dendro_data_dead$date,'%Y'))

##keeping only the one observation of each tag per year and only include the entry for the first time the tagged tree entered in the CWD pool 
dendro_data_dead_unique <- dendro_data_dead%>%
  group_by(tag)%>%
  filter(date == min(date))%>%
  filter(tag != "510") ## get rid of a tree that was only dead in 2020 lol 

  
# arrange by tag and week for organization
dendro_data_dead_unique <- arrange(dendro_data_dead_unique, tag, year)


##Importing the new DBH readings directly from the pipeline df increment dataframe from the NPP_pipline.R file

##Create a list of tree tags that are dead
dead_tags <- dendro_data_dead_unique$tag

##Filter out only the tags that are dead 
##Find the last DBH measurement that was taken and add to original dendro_data_dead_unique datafraome
df_dead <- df%>%
  filter(tag %in% dead_tags)%>%
  group_by(tag)%>%
  filter(month == max(month))%>%
  select(tag, DBH_cm_new)

dendro_data_dead_unique <- merge(dendro_data_dead_unique, df_dead, by = "tag")


###Add canopy height data from fortedata: using the median canopy height to calculate the median height of the new CWD material to convert to volume. We are doing this because we did not directly measure the heights of the new CWD material 
require(fortedata)
require(tidyverse)
# bring in lidar data
df_height <- fortedata::fd_canopy_structure()
# sort to what you need
df_height %>%
  group_by(subplot_id) %>%
  summarize(med_ht2 = median(mean_height_median, na.rm = TRUE),
            med_outer_ht = median(moch, na.rm = TRUE)) %>%
  data.frame() -> med.hts
med.hts <- med.hts%>%
  select(subplot_id, med_ht2)


dendro_data_dead_unique <- merge(dendro_data_dead_unique, med.hts, by = "subplot_id")


##Calculate the biomass of CWD from dendro data
dendro_data_dead_unique <- dendro_data_dead_unique%>%
  mutate(A_cm2 = pi*((DBH_cm_new/2)^2))%>%
  mutate(length_cm = med_ht2*100)%>%
  mutate(Volume_cm3 = length_cm*A_cm2)%>%##Then apply the Newton Equation to get to woody volume per tree (cm^3)
  mutate(C_fraction_g_cm3 = 0.50) ##Apply a carbon density value by decay class (C fraction) found in Gough et al. 2007 (All disturbance induced CWD was class 1, only apply the C fraction of decay class 1) 


##Convert from wood volume to carbon mass (Mg)
dendro_data_dead_unique <- dendro_data_dead_unique%>%
  mutate(C_mass_Mg = (Volume_cm3*C_fraction_g_cm3)/1000000)

##Add rep_id to dataframe 
dendro_data_dead_unique <- dendro_data_dead_unique%>%
  mutate(rep_id = case_when(subplot_id == "A01E" |subplot_id == "A02E" |subplot_id == "A03E" |subplot_id == "A04E" |subplot_id == "A01W" |subplot_id == "A02W" |subplot_id == "A03W"|subplot_id == "A04W" ~ "A",
                            subplot_id == "B01E" |subplot_id == "B02E" |subplot_id == "B03E" |subplot_id == "B04E" |subplot_id == "B01W" |subplot_id == "B02W" |subplot_id == "B03W" |subplot_id == "B04W" ~ "B", 
                            subplot_id == "C01E" |subplot_id == "C02E" |subplot_id == "C03E" |subplot_id == "C04E" |subplot_id == "C01W" |subplot_id == "C02W" |subplot_id == "C03W"|subplot_id == "C04W" ~ "C", 
                            subplot_id == "D01E" |subplot_id == "D02E" |subplot_id == "D03E" |subplot_id == "D04E" |subplot_id == "D01W" |subplot_id == "D02W" |subplot_id == "D03W"|subplot_id == "D04W" ~ "D"))


#####Figure out the percentage of dendroband trees to unbanded trees per subplot 

dendro_data_percent <- dendro_data%>%
  filter(date == min(date))%>%
  select(tag, subplot_id)

df_percent <- df%>%
  filter(date == min(date))%>%
  select(tag, subplot_id)
  

df_percent_count <- df_percent%>% 
  group_by(subplot_id) %>% summarise(count_total=n())

dendro_data_percent_count <- dendro_data_percent%>% 
  group_by(subplot_id) %>% summarise(count_dendro=n())



df_percent_count <- merge(dendro_data_percent_count,df_percent_count, by = "subplot_id")

df_percent_count$count_dendro <- as.numeric(df_percent_count$count_dendro)
df_percent_count$count_total <- as.numeric(df_percent_count$count_total)

df_percent_count <- df_percent_count%>%
  mutate(percent_dendro = count_dendro/count_total)


##Scale up to subplot . THIS IS WHERE I NEED TO SCALE TO UNBANDED TREES!!!! (Add an extra 75% of C mass to scale to unbanded trees)

dendro_data_CWD_subplot <- dendro_data_dead_unique%>%
  inner_join(df_percent_count, by = "subplot_id")%>%
  group_by(subplot_id, rep_id, year, percent_dendro)%>%
  summarize(C_mass_Mg_subplot = sum(C_mass_Mg))%>%##Scale to subplot
  mutate(decay_class = 1)%>%
  mutate(C_mass_Mg_subplot = C_mass_Mg_subplot/percent_dendro)




#########################################FoRTE CWD Survey 2019##############################

##Import Data From FoRTE google drive

CWD_2019 <- read.csv("Data_CWD/FoRTE_CWDsurvey2019.csv", na.strings = c("", "NA")) 

##Add replicate as a column

CWD_2019 <- CWD_2019%>%
  mutate(rep_id = case_when(subplot_id == "A01E" |subplot_id == "A02E" |subplot_id == "A03E" |subplot_id == "A04E" |subplot_id == "A01W" |subplot_id == "A02W" |subplot_id == "A03W"|subplot_id == "A04W" ~ "A",
                            subplot_id == "B01E" |subplot_id == "B02E" |subplot_id == "B03E" |subplot_id == "B04E" |subplot_id == "B01W" |subplot_id == "B02W" |subplot_id == "B03W" |subplot_id == "B04W" ~ "B", 
                            subplot_id == "C01E" |subplot_id == "C02E" |subplot_id == "C03E" |subplot_id == "C04E" |subplot_id == "C01W" |subplot_id == "C02W" |subplot_id == "C03W"|subplot_id == "C04W" ~ "C", 
                            subplot_id == "D01E" |subplot_id == "D02E" |subplot_id == "D03E" |subplot_id == "D04E" |subplot_id == "D01W" |subplot_id == "D02W" |subplot_id == "D03W"|subplot_id == "D04W" ~ "D"))

###Use the Harmon & Sexton 1996 "Newton Method to estimate volumn of a downed woody log 
## Newton Equation: V = L(Ab + 4Am + At)/6

##First Calculate area from the diameter measurements (which are classified as widths in the dataframe): Use equation for surface area (A =pi*r^2)
##Then apply the Newton Equation to get to woody volume per tree (cm^3)

CWD_2019 <- CWD_2019%>%
  mutate(A_b_cm2 = pi*((width_end1_cm/2)^2))%>%
  mutate(A_m_cm2 = pi*((width_mid_cm/2)^2))%>%
  mutate(A_t_cm2 = pi*((width_end2_cm/2)^2))%>%
  mutate(length_cm = length_m*100)%>%
  mutate(Volume_cm3 = length_cm*(A_b_cm2 + 4*A_m_cm2 + A_t_cm2)/6)%>%##Then apply the Newton Equation to get to woody volume per tree (cm^3)
  mutate(C_fraction_g_cm3 = case_when(decay_class == 1 ~ "0.50", 
                                       decay_class == 2 ~ "0.38", 
                                       decay_class == 3 ~ "0.31", 
                                       decay_class == 4 ~ "0.29", 
                                       decay_class == 5 ~ "0.23"))##Apply a carbon density value by decay class (C fraction) found in Gough et al. 2007 

##convert to numeric class
CWD_2019$C_fraction_g_cm3 <- as.numeric(CWD_2019$C_fraction_g_cm3)

##Convert from wood volume to carbon mass (Mg)
CWD_2019 <- CWD_2019%>%
  mutate(C_mass_Mg = (Volume_cm3*C_fraction_g_cm3)/1000000)

##Scale up to subplot
CWD_2019_subplot <- CWD_2019%>%
  filter(!is.na(subplot_id))%>%
  group_by(subplot_id, decay_class, rep_id)%>%
  summarize(C_mass_Mg_subplot = sum(C_mass_Mg))%>%
  mutate(year = "2019")


############################Calculate Mass loss approach (Site aggregate decay constants from Dai et al. 2021) ##########################################
#########################################################################################################################################################

####2019 Survey + additional dendroband trees 
##Add additions from 2019 to baseline CWD (Since the survey was done at the sametime as the dendroband reads in 2019)
dendro_data_CWD_subplot_2019 <- dendro_data_CWD_subplot%>%
  filter(year == 2019)

##merge the new 2019 additions with the survey data 
CWD_2019_subplot_agg <- merge(dendro_data_CWD_subplot_2019,CWD_2019_subplot, by = c("subplot_id","year", "decay_class", "rep_id"), all = TRUE)%>%
  rename(C_mass_Mg_subplot_new = C_mass_Mg_subplot.x, C_mass_Mg_subplot_survey = C_mass_Mg_subplot.y)

  
##Make the NAs = zero for addition 

CWD_2019_subplot_agg <- replace(CWD_2019_subplot_agg, is.na(CWD_2019_subplot_agg), 0)

##Make an aggregate column 

CWD_2019_subplot_agg <- CWD_2019_subplot_agg%>%
  mutate(C_mass_Mg_subplot_total = C_mass_Mg_subplot_new + C_mass_Mg_subplot_survey)


####Apply the Simulated CWD mass loss equations from climate sensitivity models from the Dai et al 2021 and site specific K constants for downed and standing coarse woody debris 
##Equation: ln (Mt) = ln (M0) + k5 * ln (t +1) - k4 * t

##Mt = Mass remaining at time (t = years)
##M0 = Mass initial
##k4 = 0.12  k5 = 0.099 site-specific decay constants that were calculated from the same lat/long as UMBS (Dai et al. 2021, appendix, Table S3)
CWD_2019_subplot_agg <- CWD_2019_subplot_agg%>%
  mutate(ln_Mass_remaining_Mg_subplot = log(C_mass_Mg_subplot_total) + 0.099 * log(1+1) - 0.12 * 1)%>%
  mutate(Mass_remaining_Mg_subplot = exp(ln_Mass_remaining_Mg_subplot))%>%
  mutate(Mass_loss_Mg_subplot = C_mass_Mg_subplot_total - Mass_remaining_Mg_subplot)


#######2020 Flux: Add the Mass_remaining_Mg_ha from 2019 to the additions of coarse woody debris from disturbance in 2020 

dendro_data_CWD_subplot_2020 <- dendro_data_CWD_subplot%>%
  filter(year == 2020)

##merge the new 2020 additions with the new Mass remaining estimate from the 2019 data 

##Make a cleaner version of the CWD_2019_ag dataframe 

CWD_2019_subplot_agg_select <- CWD_2019_subplot_agg%>%
  select(subplot_id, rep_id, year, decay_class, Mass_remaining_Mg_subplot)

CWD_2020_subplot_agg <- merge(CWD_2019_subplot_agg_select,dendro_data_CWD_subplot_2020, by = c("subplot_id", "decay_class", "rep_id"), all = TRUE)%>%
  rename(Mass_additions_Mg_subplot_2020 = C_mass_Mg_subplot, Mass_remaining_Mg_subplot_2019 =Mass_remaining_Mg_subplot) %>%
  select(!year.x)%>%
  select(!year.y)
  
 
##Make the NAs = zero for addition 

CWD_2020_subplot_agg <- replace(CWD_2020_subplot_agg, is.na(CWD_2020_subplot_agg), 0)

##Make an aggregate column 

CWD_2020_subplot_agg <- CWD_2020_subplot_agg%>%
  mutate(C_mass_Mg_subplot_total = Mass_additions_Mg_subplot_2020 + Mass_remaining_Mg_subplot_2019)


####Apply the Simulated CWD mass loss equations from climate sensitivity models from the Dai et al 2021 and site specific K constants for downed and standing coarse woody debris 
##Equation: ln (Mt) = ln (M0) + k5 * ln (t +1) - k4 * t

##Mt = Mass remaining at time (t = years)
##M0 = Mass initial
##k4 = 0.12  k5 = 0.099 site-specific decay constants that were calculated from the same lat/long as UMBS (Dai et al. 2021, appendix, Table S3)
CWD_2020_subplot_agg <- CWD_2020_subplot_agg%>%
  mutate(ln_Mass_remaining_Mg_subplot = log(C_mass_Mg_subplot_total) + 0.099 * log(1+1) - 0.12 * 1)%>%
  mutate(Mass_remaining_Mg_subplot_2020 = exp(ln_Mass_remaining_Mg_subplot))%>%
  mutate(Mass_loss_Mg_subplot_2020 = C_mass_Mg_subplot_total - Mass_remaining_Mg_subplot_2020)



#######2021 Flux: Add the Mass_remaining_Mg_ha from 2020 to the additions of coarse woody debris from disturbance in 2021 

dendro_data_CWD_subplot_2021 <- dendro_data_CWD_subplot%>%
  filter(year == 2021)

##merge the new 2021 additions with the new Mass remaining estimate from the 2020 data 

##Make a cleaner version of the CWD_2020_agg dataframe 

CWD_2020_subplot_agg_select <- CWD_2020_subplot_agg%>%
  select(subplot_id, rep_id, decay_class, Mass_remaining_Mg_subplot_2020)

CWD_2021_subplot_agg <- merge(CWD_2020_subplot_agg_select,dendro_data_CWD_subplot_2021, by = c("subplot_id", "decay_class", "rep_id"), all = TRUE)%>%
  rename(Mass_additions_Mg_subplot_2021 = C_mass_Mg_subplot)%>%
  select(!year)


##Make the NAs = zero for addition 

CWD_2021_subplot_agg <- replace(CWD_2021_subplot_agg, is.na(CWD_2021_subplot_agg), 0)

##Make an aggregate column 

CWD_2021_subplot_agg <- CWD_2021_subplot_agg%>%
  mutate(C_mass_Mg_subplot_total = Mass_additions_Mg_subplot_2021 + Mass_remaining_Mg_subplot_2020)


####Apply the Simulated CWD mass loss equations from climate sensitivity models from the Dai et al 2021 and site specific K constants for downed and standing coarse woody debris 
##Equation: ln (Mt) = ln (M0) + k5 * ln (t +1) - k4 * t

##Mt = Mass remaining at time (t = years)
##M0 = Mass initial
##k4 = 0.12  k5 = 0.099 site-specific decay constants that were calculated from the same lat/long as UMBS (Dai et al. 2021, appendix, Table S3)
CWD_2021_subplot_agg <- CWD_2021_subplot_agg%>%
  mutate(ln_Mass_remaining_Mg_subplot = log(C_mass_Mg_subplot_total) + 0.099 * log(1+1) - 0.12 * 1)%>%
  mutate(Mass_remaining_Mg_subplot_2021 = exp(ln_Mass_remaining_Mg_subplot))%>%
  mutate(Mass_loss_Mg_subplot_2021 = C_mass_Mg_subplot_total - Mass_remaining_Mg_subplot_2021)



#######2022 Flux: Add the Mass_remaining_Mg_ha from 2020 to the additions of coarse woody debris from disturbance in 2021 

dendro_data_CWD_subplot_2022 <- dendro_data_CWD_subplot%>%
  filter(year == 2022)

##merge the new 2021 additions with the new Mass remaining estimate from the 2020 data 

##Make a cleaner version of the CWD_2021_agg dataframe 

CWD_2021_subplot_agg_select <- CWD_2021_subplot_agg%>%
  select(subplot_id, rep_id, decay_class, Mass_remaining_Mg_subplot_2021)

CWD_2022_subplot_agg <- merge(CWD_2021_subplot_agg_select,dendro_data_CWD_subplot_2022, by = c("subplot_id", "decay_class", "rep_id"), all = TRUE)%>%
  rename(Mass_additions_Mg_subplot_2022 = C_mass_Mg_subplot)%>%
  select(!year)


##Make the NAs = zero for addition 

CWD_2022_subplot_agg <- replace(CWD_2022_subplot_agg, is.na(CWD_2022_subplot_agg), 0)

##Make an aggregate column 

CWD_2022_subplot_agg <- CWD_2022_subplot_agg%>%
  mutate(C_mass_Mg_subplot_total = Mass_additions_Mg_subplot_2022 + Mass_remaining_Mg_subplot_2021)


####Apply the Simulated CWD mass loss equations from climate sensitivity models from the Dai et al 2021 and site specific K constants for downed and standing coarse woody debris 
##Equation: ln (Mt) = ln (M0) + k5 * ln (t +1) - k4 * t

##Mt = Mass remaining at time (t = years)
##M0 = Mass initial
##k4 = 0.12  k5 = 0.099 site-specific decay constants that were calculated from the same lat/long as UMBS (Dai et al. 2021, appendix, Table S3)
CWD_2022_subplot_agg <- CWD_2022_subplot_agg%>%
  mutate(ln_Mass_remaining_Mg_subplot = log(C_mass_Mg_subplot_total) + 0.099 * log(1+1) - 0.12 * 1)%>%
  mutate(Mass_remaining_Mg_subplot_2022 = exp(ln_Mass_remaining_Mg_subplot))%>%
  mutate(Mass_loss_Mg_subplot_2022 = C_mass_Mg_subplot_total - Mass_remaining_Mg_subplot_2022)

#########Now combine all the years together!!!##

##first, add a year column to each dataframe and rename some columns for consistency 
CWD_2019_subplot_agg_combined <- CWD_2019_subplot_agg%>%
  mutate(year = "2019")%>%
  select(subplot_id, rep_id, year, decay_class, Mass_loss_Mg_subplot, Mass_remaining_Mg_subplot)
  
CWD_2020_subplot_agg_combined <- CWD_2020_subplot_agg%>%
  mutate(year = "2020")%>%
  rename(Mass_loss_Mg_subplot = Mass_loss_Mg_subplot_2020, Mass_remaining_Mg_subplot = Mass_remaining_Mg_subplot_2020)%>%
  select(subplot_id, rep_id, year, decay_class,Mass_loss_Mg_subplot, Mass_remaining_Mg_subplot)

CWD_2021_subplot_agg_combined <- CWD_2021_subplot_agg%>%
  mutate(year = "2021")%>%
  rename(Mass_loss_Mg_subplot = Mass_loss_Mg_subplot_2021, Mass_remaining_Mg_subplot = Mass_remaining_Mg_subplot_2021)%>%
  select(subplot_id, rep_id, year,decay_class, Mass_loss_Mg_subplot, Mass_remaining_Mg_subplot)

CWD_2022_subplot_agg_combined <- CWD_2022_subplot_agg%>%
  mutate(year = "2022")%>%
  rename(Mass_loss_Mg_subplot = Mass_loss_Mg_subplot_2022, Mass_remaining_Mg_subplot = Mass_remaining_Mg_subplot_2022)%>%
  select(subplot_id, rep_id, year, decay_class,Mass_loss_Mg_subplot, Mass_remaining_Mg_subplot)

####################################### All Year Mass Loss Estimates from CWD ################################################
CWD_subplot_all_years <- rbind(CWD_2019_subplot_agg_combined,CWD_2020_subplot_agg_combined,CWD_2021_subplot_agg_combined,CWD_2022_subplot_agg_combined)

##Scale subplot values to hectare 

CWD_all_years <- CWD_subplot_all_years%>%
  mutate(Mass_loss_Mg_ha = Mass_loss_Mg_subplot*10)%>%
  mutate(Mass_remaining_Mg_ha = Mass_remaining_Mg_subplot*10)

###Add Severity and treatment to CWD Dataframe 
CWD_all_years <- CWD_all_years%>%
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
  ))


##Total across decay classes: 
CWD_all_years_total <- CWD_all_years%>%
  group_by(subplot_id, year, severity, rep_id, treatment)%>%
  summarize(Mass_loss_Mg_ha_total = sum(Mass_loss_Mg_ha))



#################################### Summary dataframe for Table ################

CWD_all_years_total_summary_severity <- CWD_all_years_total%>%
  group_by(severity, year)%>%
  summarize(Mass_loss_Mg_ha_mean = mean(Mass_loss_Mg_ha_total), Mass_loss_Mg_ha_se = std.error(Mass_loss_Mg_ha_total))

CWD_all_years_total_summary_treatment <- CWD_all_years_total%>%
  group_by(treatment, year)%>%
  summarize(Mass_loss_Mg_ha_mean = mean(Mass_loss_Mg_ha_total), Mass_loss_Mg_ha_se = std.error(Mass_loss_Mg_ha_total))


#######CWD Flux Figures ############################
forte_pal <- forte_colors()

# ggplot(CWD_all_years_total, aes(x = year, y = Mass_loss_Mg_ha, fill = severity)) +
#   geom_boxplot() +
#   theme_bw() +
#   scale_fill_manual(values = forte_pal)+
#   ylab("Carbon Mass Loss (Mg ha-1)")+
#   theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30))
# 
# ggsave(path = "Figures_Output", filename = "Mass_Loss_severity.png", height = 20, width =30, units = "in")
# 
# ##Figure of just decay class one
# CWD_all_years_1 <-  CWD_all_years%>%
#   filter(decay_class == "1")
# 
# ggplot(CWD_all_years_1, aes(x = year, y = Mass_loss_Mg_ha, fill = severity)) +
#   geom_boxplot() +
#   theme_bw() +
#   scale_fill_manual(values = forte_pal)+
#   ylab("Carbon Mass Loss (Mg ha-1)")+
#   theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30))
# 
# ggsave(path = "Figures_Output", filename = "Mass_Loss_site_model_decay1.png", height = 10, width =15, units = "in")

# ####By Replicate 
# ggplot(CWD_all_years_total, aes(x = year, y = Mass_loss_Mg_ha, fill = rep_id)) +
#   geom_boxplot() +
#   theme_bw() +
#   scale_fill_manual(values = c("#99E2E1", "#332752", "#CB3074", "#F7C667"))+
#   facet_wrap(~decay_class)+
#   ylab("Carbon Mass Loss (Mg ha-1)") +
#   theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30))
# 
# ggsave(path = "Figures_Output", filename = "Mass_Loss_replicate.png", height = 20, width =30, units = "in")
# 

#################################################(METHOD 2) ##############################################################################
############################################### Mass Loss, species-specific decay constant method ###########################################
#####################################################################################################################################################

####Create Subplot-specific species composition percentages in the canopy 

##Create a year column and filter out only the 2018 dates to get the baseline community composition 
df_composition <- df

df_composition$year <- format(as.Date(df_composition$date), format = "%Y")

df_composition$year <- as.factor(df_composition$year)
 
 df_composition <- df_composition%>% 
  filter(year == "2018")

 ##Calculate the percent biomass (kg) for each species by subplot and add species specific decay rates to dataframe 
 composition_decay_rate <- df_composition%>% 
 mutate(a = case_when(
   species == "ACPE" ~ 0.03117,
   species == "ACRU" ~ 0.03177,
   species == "ACSA" ~ 0.1693,
   species == "AMEL" ~ 0.1630,
   species == "BEPA" ~ 0.0301,
   species == "FAGR" ~ 0.1892,
   species == "PIRE" ~ 0.0526,
   species == "PIST" ~ 0.0408,
   species == "POGR" ~ 0.1387,
   species == "POTR" ~ 0.0589,
   species == "QURU" ~ 0.0398,
   species == "ABBA" ~ 0.0705,
   species == "TSCA" ~ 0.1617
 )) %>% 
   mutate(b = case_when(
     species == "ACPE" ~ 2.7780,
     species == "ACRU" ~ 2.7780,
     species == "ACSA" ~ 2.3436,
     species == "AMEL" ~ 2.4940,
     species == "BEPA" ~ 2.8387,
     species == "FAGR" ~ 2.3097,
     species == "PIRE" ~ 2.5258,
     species == "PIST" ~ 2.5735,
     species == "POGR" ~ 2.3498,
     species == "POTR" ~ 2.6235,
     species == "QURU" ~ 2.7734,
     species == "ABBA" ~ 2.4970,
     species == "TSCA" ~ 2.1536
   ))%>%
   mutate(biomass_kg = a*dbh_cm^b)%>%
   group_by(subplot_id, species)%>%
   summarize(biomass_kg_sum = sum(biomass_kg))%>%
  mutate(Percentage=paste0(round(biomass_kg_sum/sum(biomass_kg_sum),3)))%>%
   mutate(species_k = case_when( ###### Add species-species decay rates (k): from sup doc: Kahl et al. )
     species == "ACPE" ~ 0.053,
     species == "ACRU" ~ 0.053,
     species == "ACSA" ~ 0.053,
     species == "AMEL" ~ 0.042,
     species == "BEPA" ~ 0.042,
     species == "FAGR" ~ 0.069,
     species == "PIRE" ~ 0.015,
     species == "PIST" ~ 0.015,
     species == "POGR" ~ 0.055,
     species == "POTR" ~ 0.055,
     species == "QURU" ~ 0.021,
     species == "ABBA" ~ 0.035,
     species == "TSCA" ~ 0.002
   ))
 
 ##tranform to numeric 
 composition_decay_rate$Percentage <- as.numeric(composition_decay_rate$Percentage)
 
 ##Calculate subplot decay rates based on weight species-specific decay rates 
 composition_decay_rate_subplot <- composition_decay_rate%>%
   mutate(proportional_k = Percentage*species_k)%>%
   group_by(subplot_id)%>%
   summarize(subplot_k = sum(proportional_k))
 
##2019: Calculating Mass loss from species specific and biomass weighted decay rates for 2019 
 
 CWD_2019_subplot_agg_species <-  CWD_2019_subplot_agg%>%
   select(rep_id, subplot_id, decay_class, year, C_mass_Mg_subplot_total)%>%
   left_join(composition_decay_rate_subplot, by = "subplot_id")%>%
   mutate(C_Mass_loss_Mg_subplot = C_mass_Mg_subplot_total*subplot_k)%>%
   mutate(C_mass_remaining_Mg_subplot_total_2019 = C_mass_Mg_subplot_total - C_Mass_loss_Mg_subplot)
 
##2020: Calculating Mass loss from species specific and biomass weighted decay rates for 2020
 
 ##Select necessary columns from the previous year 
 CWD_2019_subplot_agg_species_select <-  CWD_2019_subplot_agg_species%>%
   select(subplot_id, rep_id, decay_class, C_mass_remaining_Mg_subplot_total_2019)
 
 ##Merge the additions from the new year dataframe with the mass remaining dataframe from previous year
 CWD_2020_subplot_agg_species<- merge( CWD_2019_subplot_agg_species_select,dendro_data_CWD_subplot_2020, by = c("subplot_id", "decay_class", "rep_id"), all = TRUE)%>%
   rename(Mass_additions_Mg_subplot_2020 = C_mass_Mg_subplot)%>%
   select(!year)

 ##Make the NAs = zero for addition 
 
 CWD_2020_subplot_agg_species <- replace(CWD_2020_subplot_agg_species, is.na(CWD_2020_subplot_agg_species), 0)
 
 ###Calculate mass loss and mass remaining from species-species and subplot specific decay constants 
 CWD_2020_subplot_agg_species <- CWD_2020_subplot_agg_species%>%
   mutate(C_mass_Mg_subplot_total = Mass_additions_Mg_subplot_2020 + C_mass_remaining_Mg_subplot_total_2019)%>%
   select(rep_id, subplot_id, decay_class, C_mass_Mg_subplot_total)%>%
   left_join(composition_decay_rate_subplot, by = "subplot_id")%>%
   mutate(C_Mass_loss_Mg_subplot = C_mass_Mg_subplot_total*subplot_k)%>%
   mutate(C_mass_remaining_Mg_subplot_total_2020 = C_mass_Mg_subplot_total - C_Mass_loss_Mg_subplot)
 
 ##2021: Calculating Mass loss from species specific and biomass weighted decay rates for 2020
 
 ##Select neccessary columns from the previous year 
 CWD_2020_subplot_agg_species_select <-  CWD_2020_subplot_agg_species%>%
   select(subplot_id, rep_id, decay_class, C_mass_remaining_Mg_subplot_total_2020)
 
 ##Merge the additions from the new year dataframe with the mass remaining dataframe from previous year
 CWD_2021_subplot_agg_species<- merge( CWD_2020_subplot_agg_species_select,dendro_data_CWD_subplot_2021, by = c("subplot_id", "decay_class", "rep_id"), all = TRUE)%>%
   rename(Mass_additions_Mg_subplot_2021 = C_mass_Mg_subplot)%>%
   select(!year)
 
 ##Make the NAs = zero for addition 
 
 CWD_2021_subplot_agg_species <- replace(CWD_2021_subplot_agg_species, is.na(CWD_2021_subplot_agg_species), 0)
 
 ###Calculate mass loss and mass remaining from species-species and subplot specific decay constants 
 CWD_2021_subplot_agg_species <- CWD_2021_subplot_agg_species%>%
   mutate(C_mass_Mg_subplot_total = Mass_additions_Mg_subplot_2021 + C_mass_remaining_Mg_subplot_total_2020)%>%
   select(rep_id, subplot_id, decay_class, C_mass_Mg_subplot_total)%>%
   left_join(composition_decay_rate_subplot, by = "subplot_id")%>%
   mutate(C_Mass_loss_Mg_subplot = C_mass_Mg_subplot_total*subplot_k)%>%
   mutate(C_mass_remaining_Mg_subplot_total_2021 = C_mass_Mg_subplot_total - C_Mass_loss_Mg_subplot)
 
 ##2022: Calculating Mass loss from species specific and biomass weighted decay rates for 2020
 
 ##Select neccessary columns from the previous year 
 CWD_2021_subplot_agg_species_select <-  CWD_2021_subplot_agg_species%>%
   select(subplot_id, rep_id, decay_class, C_mass_remaining_Mg_subplot_total_2021)
 
 ##Merge the additions from the new year dataframe with the mass remaining dataframe from previous year
 CWD_2022_subplot_agg_species<- merge( CWD_2021_subplot_agg_species_select,dendro_data_CWD_subplot_2022, by = c("subplot_id", "decay_class", "rep_id"), all = TRUE)%>%
   rename(Mass_additions_Mg_subplot_2022 = C_mass_Mg_subplot)%>%
   select(!year)
 
 ##Make the NAs = zero for addition 
 
 CWD_2022_subplot_agg_species <- replace(CWD_2022_subplot_agg_species, is.na(CWD_2022_subplot_agg_species), 0)
 
 ###Calculate mass loss and mass remaining from species-species and subplot specific decay constants 
 CWD_2022_subplot_agg_species <- CWD_2022_subplot_agg_species%>%
   mutate(C_mass_Mg_subplot_total = Mass_additions_Mg_subplot_2022 + C_mass_remaining_Mg_subplot_total_2021)%>%
   select(rep_id, subplot_id, decay_class, C_mass_Mg_subplot_total)%>%
   left_join(composition_decay_rate_subplot, by = "subplot_id")%>%
   mutate(C_Mass_loss_Mg_subplot = C_mass_Mg_subplot_total*subplot_k)%>%
   mutate(C_mass_remaining_Mg_subplot_total_2022 = C_mass_Mg_subplot_total - C_Mass_loss_Mg_subplot)
 
 
 #########Now combine all the years together!!!##
 
 ##first, add a year column to each dataframe and rename some columns for consistency 
 CWD_2019_subplot_agg_species_combined <- CWD_2019_subplot_agg_species%>%
   mutate(year = "2019")%>%
   select(subplot_id, rep_id, year, decay_class, C_Mass_loss_Mg_subplot,C_mass_remaining_Mg_subplot_total_2019)%>%
   rename(Mass_remaining_Mg_subplot = C_mass_remaining_Mg_subplot_total_2019)
 
 CWD_2020_subplot_agg_species_combined <- CWD_2020_subplot_agg_species%>%
   mutate(year = "2020")%>%
   select(subplot_id, rep_id, year, decay_class, C_Mass_loss_Mg_subplot,C_mass_remaining_Mg_subplot_total_2020)%>%
   rename(Mass_remaining_Mg_subplot = C_mass_remaining_Mg_subplot_total_2020)
 
 CWD_2021_subplot_agg_species_combined <- CWD_2021_subplot_agg_species%>%
   mutate(year = "2021")%>%
   select(subplot_id, rep_id, year, decay_class, C_Mass_loss_Mg_subplot,C_mass_remaining_Mg_subplot_total_2021)%>%
   rename(Mass_remaining_Mg_subplot = C_mass_remaining_Mg_subplot_total_2021)
 
 CWD_2022_subplot_agg_species_combined <- CWD_2022_subplot_agg_species%>%
   mutate(year = "2022")%>%
   select(subplot_id, rep_id, year, decay_class, C_Mass_loss_Mg_subplot,C_mass_remaining_Mg_subplot_total_2022)%>%
   rename(Mass_remaining_Mg_subplot = C_mass_remaining_Mg_subplot_total_2022)
 
 
 ####################################### All Year Mass Loss Estimates from CWD ################################################
 CWD_subplot_all_years_species <- rbind(CWD_2019_subplot_agg_species_combined,CWD_2020_subplot_agg_species_combined,CWD_2021_subplot_agg_species_combined,CWD_2022_subplot_agg_species_combined)
 
 ##Scale subplot values to hectare 
 
 CWD_all_years_species <- CWD_subplot_all_years_species%>%
   mutate(Mass_loss_Mg_ha = C_Mass_loss_Mg_subplot*10)%>%
   mutate(Mass_remaining_Mg_ha = Mass_remaining_Mg_subplot*10)
 
 ###Add Severity and treatment to CWD Dataframe 
 CWD_all_years_species <-  CWD_all_years_species%>%
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
   ))
 

 ##Total across decay classes: Before I averaged these values!!!!!!! 
 CWD_all_years_species_total <- CWD_all_years_species%>%
   group_by(subplot_id, year, severity, rep_id, treatment)%>%
   summarize(Mass_loss_Mg_ha_total = sum(Mass_loss_Mg_ha))
 
 
 
 
 ################################## Summary Dataframes for table ##########################
 CWD_all_years_species_total_summary_severity <-  CWD_all_years_species_total%>%
   group_by(severity, year)%>%
   summarize(Mass_loss_Mg_ha_mean = mean(Mass_loss_Mg_ha_total), Mass_loss_Mg_ha_se = std.error(Mass_loss_Mg_ha_total))
 
 CWD_all_years_species_total_summary_treatment <-  CWD_all_years_species_total%>%
   group_by(treatment, year)%>%
   summarize(Mass_loss_Mg_ha_mean = mean(Mass_loss_Mg_ha_total), Mass_loss_Mg_ha_se = std.error(Mass_loss_Mg_ha_total))
 
 
 
 # ###Figures 
 # 
 # ####By Severity and decay_class 
 # 
 # ###severity and year summary
 # CWD_all_years_species_severity <-  CWD_all_years_species_total%>%
 #   group_by(severity, year)%>%
 #   summarize(Mass_loss_Mg_ha_mean = mean(Mass_loss_Mg_ha_total), Mass_loss_Mg_ha_se = std.error(Mass_loss_Mg_ha_total))
 #  
 # write.csv( CWD_all_years_species_severity, "Figure_Output\\CWD_table.csv", row.names=FALSE) 
 # 
 # ggplot(CWD_all_years_species_total, aes(x = year, y = Mass_loss_Mg_ha_total, fill = severity)) +
 #   geom_boxplot() +
 #   theme_bw() +
 #   scale_fill_manual(values = forte_pal)+
 #   facet_wrap(~decay_class) +
 #   ylab("Carbon Mass Loss (Mg ha-1)")+
 #   theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30))
 # 
 # ggsave(path = "Figures_Output", filename = "Mass_Loss_severity.png", height = 20, width =30, units = "in")
 # 
 # ##Figure of just decay class one
 # CWD_all_years_species_1 <-  CWD_all_years_species_total%>%
 #   filter(decay_class == "1")
 # 
 # ggplot(CWD_all_years_species_1_total, aes(x = year, y = Mass_loss_Mg_ha, fill = severity)) +
 #   geom_boxplot() +
 #   theme_bw() +
 #   scale_fill_manual(values = forte_pal)+
 #   ylab("Carbon Mass Loss (Mg ha-1)")+
 #   theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30))
 # 
 # 
 # ggsave(path = "Figures_Output", filename = "Mass_Loss_species_model_decay1.png", height = 10, width =15, units = "in")
 # 
 # 
 # ####By Replicate 
 # ggplot(CWD_all_years, aes(x = year, y = Mass_loss_Mg_ha, fill = rep_id)) +
 #   geom_boxplot() +
 #   theme_bw() +
 #   scale_fill_manual(values = c("#99E2E1", "#332752", "#CB3074", "#F7C667"))+
 #   facet_wrap(~decay_class)+
 #   ylab("Carbon Mass Loss (Mg ha-1)") +
 #   theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30))
 #   
 # 
 
##################################### (METHOD 3) ################################################################################################
############################# Tower Temperature and Moisture CWD flux estimate method ############################################################
#####################################################################################################################################

library(lubridate)
library(openair)
library(stringr)
options(scipen=999)

##Read in Raw Tower Data: DO NOT OPEN THIS FILE IN EXCEL, IT WILL WRECK YOUR SHIT
tower_raw <- read.csv("Data_Tower/Tower_download/AMF_US-UMB_BASE_HH_18-5.csv", na.strings = c("-9999", "NA"), skip = 2)

##CONVERT TIMESTAMP INTO POSIXct class 
tower_raw <- tower_raw%>%
mutate(TIMESTAMP = ymd_hm(TIMESTAMP_START))

tower_raw$TIMESTAMP <- as.POSIXct(tower_raw$TIMESTAMP, tz = "", "%Y-%m-%d %H:%M")

##SELECT ONLY TEMPERATURE AND MOISTURE COLUMNS 
tower_T_SWC <- tower_raw%>%
  select(TIMESTAMP, TS_1_2_1, SWC_1_2_1)

###SELECT ONLY THE YEARS OF FORTE ANALYSIS 

tower_T_SWC <- tower_T_SWC%>%
  filter(TIMESTAMP > as.POSIXct("2018-12-31 23:00:00"))

############################## Model Temperature data from tower data #################
####Create a Temperature Only Dataframe 
tower_T <- tower_T_SWC%>%
  select(TIMESTAMP, TS_1_2_1)%>%
  filter(!is.na(TS_1_2_1))

##Average across every hour 
Tower_T_hour <- tower_T%>%
  group_by(TIMESTAMP = floor_date(TIMESTAMP, "1 hour"))%>%
  summarize(TS_1_2_1_hour = mean(TS_1_2_1))

##Average across every 6 hours 
Tower_T_6hour <- tower_T%>%
  group_by(TIMESTAMP = floor_date(TIMESTAMP, "6 hours"))%>%
  summarize(TS_1_2_1_hour = mean(TS_1_2_1))


######Download point Rs, VWC, Ts for 2019-2021 from Google Drive####

#########################################################2019 Data ##################################################################
# Direct Google Drive link to "FoRTE/data/soil_respiration"
as_id("https://drive.google.com/drive/folders/1HHnDpTj32O-aaFavUzugzIxojf_BGEei") %>% 
  drive_ls ->
  gdfiles

# Create a new data directory for files, if necessary
data_dir <- "googledrive_data/"
if(!dir.exists(data_dir)) dir.create(data_dir)

#Download date
for(f in seq_len(nrow(gdfiles))) {
  cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
  drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
}

##Read in Rs point measurement files 
raw_2019 <- read.csv("googledrive_data/Rs_2019.csv", na.strings = c("NA","na"))

###Combine plot level info into one subplot column
raw_2019$Subplot_ID <- str_c(raw_2019$Rep_ID, '',raw_2019$Plot_ID,'', raw_2019$Subplot)


##Keep the columns that we need 
point_2019 <- raw_2019%>%
  select(Subplot_ID, soilTemp, VWC, date, HHMMSS)

##Convert date and time columns to datetime and combine date and time column into standard format for tower data  

point_2019$date <- as.POSIXct(point_2019$date)


point_2019$HHMMSS <- as.POSIXct(point_2019$HHMMSS, tz = "", format = "%H:%M:%S")

point_2019$HHMMSS <- format(as.POSIXct(point_2019$HHMMSS), format = "%H")

###Create a 6 hour timestamp variable as well 
point_2019 <- point_2019%>%
  mutate(HH = case_when(HHMMSS >= "00" & HHMMSS <= "06" ~ "00:00:00", 
                        HHMMSS >= "07" & HHMMSS <= "12" ~ "06:00:00", 
                        HHMMSS >= "13" & HHMMSS <= "18" ~ "12:00:00",
                        HHMMSS >= "19" & HHMMSS <= "23" ~ "18:00:00"))

point_2019$HH <- as.POSIXct(point_2019$HH, tz = "", format = "%H:%M:%S")

point_2019$HH <- format(as.POSIXct(point_2019$HH), format = "%H")

point_2019 <- point_2019%>%
  mutate(Timestamp = as.POSIXct(paste(date, HHMMSS), format="%Y-%m-%d %H"))

point_2019 <- point_2019%>%
  mutate(Timestamp_6 = as.POSIXct(paste(date, HH), format="%Y-%m-%d %H"))

point_2019$year <- format(as.POSIXct(point_2019$Timestamp), format = "%Y")

point_2019$Timestamp_6 <- as.POSIXct(format(point_2019$Timestamp_6), tz = "UTC")
point_2019$Timestamp <- as.POSIXct(format(point_2019$Timestamp), tz = "UTC")

### Average temperature and moisture over the subplot per timestamp 

point_2019_mean_6 <- point_2019%>%
  group_by(date, Subplot_ID, Timestamp_6)%>%
  summarize(ave_soilTemp = mean(soilTemp), ave_VWC = mean(VWC))

##Separate temperature and moisture columns and filter out NAs


point_2019_mean_temp_6 <- point_2019_mean_6%>%
  select(date, Subplot_ID, Timestamp_6, ave_soilTemp)%>%
  filter(!is.na(ave_soilTemp))%>%
  rename(ave_point_temp = ave_soilTemp)

point_2019_mean_VWC_6 <- point_2019_mean_6%>%
  select(date, Subplot_ID, Timestamp_6, ave_VWC)%>%
  filter(!is.na(ave_VWC))%>%
  rename(ave_point_SWC = ave_VWC)

##Add tower data to match the point measurements 

##Rename the tower variables and select only the 2019 Timestamps 

Tower_T_6hour <- Tower_T_6hour%>%
  rename(Timestamp_6 = TIMESTAMP, tower_temp = TS_1_2_1_hour)

Tower_T_6hour_2019 <- Tower_T_6hour%>%
  filter(between(Timestamp_6, as.POSIXct('2019-01-01 00:00:00'), as.POSIXct('2019-12-31 18:00:00')))


###Join Tower and point data for 2019 

Tower_T_6hour_2019_combined <- Tower_T_6hour_2019 %>% 
  left_join(point_2019_mean_temp_6, by = "Timestamp_6")


###Run regression analyses for the relationship between tower and point measurements
Tower_T_6hour_2019_regression <- Tower_T_6hour_2019_combined%>%
  filter(!is.na(ave_point_temp))

###Run Regression to look at summary statistics (All models are highly significant)
library(broom)

fitted_model_temp_6 <-  Tower_T_6hour_2019_regression%>%
  nest_by(Subplot_ID)%>%
  mutate(model = list(lm(ave_point_temp ~ tower_temp, data = data)))%>%
  summarize(tidy(model, conf.int = TRUE, conf.level = 0.95))%>%
  select(Subplot_ID, term, conf.low, conf.high)


fitted_model_temp_6 <- fitted_model_temp_6%>%
 pivot_wider(names_from = term, values_from = c( conf.low, conf.high))

###Organize model coefficients into a dataframe 
 library(data.table)
# 
Tower_T_6hour_2019_regression <- data.table(Tower_T_6hour_2019_regression)
# 
Tower_T_6hour_2019_regression <- Tower_T_6hour_2019_regression[,as.list(coef(lm(ave_point_temp ~ tower_temp))), by=Subplot_ID]
# 
Tower_T_6hour_2019_regression <- as.data.frame.matrix(Tower_T_6hour_2019_regression)

Tower_T_6hour_2019_regression <- merge(Tower_T_6hour_2019_regression,fitted_model_temp_6, by = "Subplot_ID" )


###Apply subplot specific regression models to model hourly temperature in each subplot for 2019 (temp(modeled) = m(tower_temp) + b)
##mean
Tower_T_6hour_2019_modeled <- Tower_T_6hour_2019%>%
  mutate(A01E = Tower_T_6hour_2019_regression[1,3]*tower_temp + Tower_T_6hour_2019_regression[1,2])%>%
  mutate(A01W = Tower_T_6hour_2019_regression[2,3]*tower_temp + Tower_T_6hour_2019_regression[2,2])%>%
  mutate(A02E = Tower_T_6hour_2019_regression[3,3]*tower_temp + Tower_T_6hour_2019_regression[3,2])%>%
  mutate(A02W = Tower_T_6hour_2019_regression[4,3]*tower_temp + Tower_T_6hour_2019_regression[4,2])%>%
  mutate(A03E = Tower_T_6hour_2019_regression[5,3]*tower_temp + Tower_T_6hour_2019_regression[5,2])%>%
  mutate(A03W = Tower_T_6hour_2019_regression[6,3]*tower_temp + Tower_T_6hour_2019_regression[6,2])%>%
  mutate(A04E = Tower_T_6hour_2019_regression[7,3]*tower_temp + Tower_T_6hour_2019_regression[7,2])%>%
  mutate(A04W = Tower_T_6hour_2019_regression[8,3]*tower_temp + Tower_T_6hour_2019_regression[8,2])%>%  
  mutate(B01E = Tower_T_6hour_2019_regression[9,3]*tower_temp + Tower_T_6hour_2019_regression[9,2])%>%
  mutate(B01W = Tower_T_6hour_2019_regression[10,3]*tower_temp + Tower_T_6hour_2019_regression[10,2])%>%
  mutate(B02E = Tower_T_6hour_2019_regression[11,3]*tower_temp + Tower_T_6hour_2019_regression[11,2])%>%
  mutate(B02W = Tower_T_6hour_2019_regression[12,3]*tower_temp + Tower_T_6hour_2019_regression[12,2])%>%
  mutate(B03E = Tower_T_6hour_2019_regression[13,3]*tower_temp + Tower_T_6hour_2019_regression[13,2])%>%
  mutate(B03W = Tower_T_6hour_2019_regression[14,3]*tower_temp + Tower_T_6hour_2019_regression[14,2])%>%
  mutate(B04E = Tower_T_6hour_2019_regression[15,3]*tower_temp + Tower_T_6hour_2019_regression[15,2])%>%
  mutate(B04W = Tower_T_6hour_2019_regression[16,3]*tower_temp + Tower_T_6hour_2019_regression[16,2])%>%
  mutate(C01E = Tower_T_6hour_2019_regression[17,3]*tower_temp + Tower_T_6hour_2019_regression[17,2])%>%
  mutate(C01W = Tower_T_6hour_2019_regression[18,3]*tower_temp + Tower_T_6hour_2019_regression[18,2])%>%
  mutate(C02E = Tower_T_6hour_2019_regression[19,3]*tower_temp + Tower_T_6hour_2019_regression[19,2])%>%
  mutate(C02W = Tower_T_6hour_2019_regression[20,3]*tower_temp + Tower_T_6hour_2019_regression[20,2])%>%
  mutate(C03E = Tower_T_6hour_2019_regression[21,3]*tower_temp + Tower_T_6hour_2019_regression[21,2])%>%
  mutate(C03W = Tower_T_6hour_2019_regression[22,3]*tower_temp + Tower_T_6hour_2019_regression[22,2])%>%
  mutate(C04E = Tower_T_6hour_2019_regression[23,3]*tower_temp + Tower_T_6hour_2019_regression[23,2])%>%
  mutate(C04W = Tower_T_6hour_2019_regression[24,3]*tower_temp + Tower_T_6hour_2019_regression[24,2])%>%
  mutate(D01E = Tower_T_6hour_2019_regression[25,3]*tower_temp + Tower_T_6hour_2019_regression[25,2])%>%
  mutate(D01W = Tower_T_6hour_2019_regression[26,3]*tower_temp + Tower_T_6hour_2019_regression[26,2])%>%
  mutate(D02E = Tower_T_6hour_2019_regression[27,3]*tower_temp + Tower_T_6hour_2019_regression[27,2])%>%
  mutate(D02W = Tower_T_6hour_2019_regression[28,3]*tower_temp + Tower_T_6hour_2019_regression[28,2])%>%
  mutate(D03E = Tower_T_6hour_2019_regression[29,3]*tower_temp + Tower_T_6hour_2019_regression[29,2])%>%
  mutate(D03W = Tower_T_6hour_2019_regression[30,3]*tower_temp + Tower_T_6hour_2019_regression[30,2])%>%
  mutate(D04E = Tower_T_6hour_2019_regression[31,3]*tower_temp + Tower_T_6hour_2019_regression[31,2])%>%
  mutate(D04W = Tower_T_6hour_2019_regression[32,3]*tower_temp + Tower_T_6hour_2019_regression[32,2])

##low
Tower_T_6hour_2019_modeled_low <- Tower_T_6hour_2019%>%
  mutate(A01E = Tower_T_6hour_2019_regression[1,5]*tower_temp + Tower_T_6hour_2019_regression[1,4])%>%
  mutate(A01W = Tower_T_6hour_2019_regression[2,5]*tower_temp + Tower_T_6hour_2019_regression[2,4])%>%
  mutate(A02E = Tower_T_6hour_2019_regression[3,5]*tower_temp + Tower_T_6hour_2019_regression[3,4])%>%
  mutate(A02W = Tower_T_6hour_2019_regression[4,5]*tower_temp + Tower_T_6hour_2019_regression[4,4])%>%
  mutate(A03E = Tower_T_6hour_2019_regression[5,5]*tower_temp + Tower_T_6hour_2019_regression[5,4])%>%
  mutate(A03W = Tower_T_6hour_2019_regression[6,5]*tower_temp + Tower_T_6hour_2019_regression[6,4])%>%
  mutate(A04E = Tower_T_6hour_2019_regression[7,5]*tower_temp + Tower_T_6hour_2019_regression[7,4])%>%
  mutate(A04W = Tower_T_6hour_2019_regression[8,5]*tower_temp + Tower_T_6hour_2019_regression[8,4])%>%  
  mutate(B01E = Tower_T_6hour_2019_regression[9,5]*tower_temp + Tower_T_6hour_2019_regression[9,4])%>%
  mutate(B01W = Tower_T_6hour_2019_regression[10,5]*tower_temp + Tower_T_6hour_2019_regression[10,4])%>%
  mutate(B02E = Tower_T_6hour_2019_regression[11,5]*tower_temp + Tower_T_6hour_2019_regression[11,4])%>%
  mutate(B02W = Tower_T_6hour_2019_regression[12,5]*tower_temp + Tower_T_6hour_2019_regression[12,4])%>%
  mutate(B03E = Tower_T_6hour_2019_regression[13,5]*tower_temp + Tower_T_6hour_2019_regression[13,4])%>%
  mutate(B03W = Tower_T_6hour_2019_regression[14,5]*tower_temp + Tower_T_6hour_2019_regression[14,4])%>%
  mutate(B04E = Tower_T_6hour_2019_regression[15,5]*tower_temp + Tower_T_6hour_2019_regression[15,4])%>%
  mutate(B04W = Tower_T_6hour_2019_regression[16,5]*tower_temp + Tower_T_6hour_2019_regression[16,4])%>%
  mutate(C01E = Tower_T_6hour_2019_regression[17,5]*tower_temp + Tower_T_6hour_2019_regression[17,4])%>%
  mutate(C01W = Tower_T_6hour_2019_regression[18,5]*tower_temp + Tower_T_6hour_2019_regression[18,4])%>%
  mutate(C02E = Tower_T_6hour_2019_regression[19,5]*tower_temp + Tower_T_6hour_2019_regression[19,4])%>%
  mutate(C02W = Tower_T_6hour_2019_regression[20,5]*tower_temp + Tower_T_6hour_2019_regression[20,4])%>%
  mutate(C03E = Tower_T_6hour_2019_regression[21,5]*tower_temp + Tower_T_6hour_2019_regression[21,4])%>%
  mutate(C03W = Tower_T_6hour_2019_regression[22,5]*tower_temp + Tower_T_6hour_2019_regression[22,4])%>%
  mutate(C04E = Tower_T_6hour_2019_regression[23,5]*tower_temp + Tower_T_6hour_2019_regression[23,4])%>%
  mutate(C04W = Tower_T_6hour_2019_regression[24,5]*tower_temp + Tower_T_6hour_2019_regression[24,4])%>%
  mutate(D01E = Tower_T_6hour_2019_regression[25,5]*tower_temp + Tower_T_6hour_2019_regression[25,4])%>%
  mutate(D01W = Tower_T_6hour_2019_regression[26,5]*tower_temp + Tower_T_6hour_2019_regression[26,4])%>%
  mutate(D02E = Tower_T_6hour_2019_regression[27,5]*tower_temp + Tower_T_6hour_2019_regression[27,4])%>%
  mutate(D02W = Tower_T_6hour_2019_regression[28,5]*tower_temp + Tower_T_6hour_2019_regression[28,4])%>%
  mutate(D03E = Tower_T_6hour_2019_regression[29,5]*tower_temp + Tower_T_6hour_2019_regression[29,4])%>%
  mutate(D03W = Tower_T_6hour_2019_regression[30,5]*tower_temp + Tower_T_6hour_2019_regression[30,4])%>%
  mutate(D04E = Tower_T_6hour_2019_regression[31,5]*tower_temp + Tower_T_6hour_2019_regression[31,4])%>%
  mutate(D04W = Tower_T_6hour_2019_regression[32,5]*tower_temp + Tower_T_6hour_2019_regression[32,4])


##high
Tower_T_6hour_2019_modeled_high <- Tower_T_6hour_2019%>%
  mutate(A01E = Tower_T_6hour_2019_regression[1,7]*tower_temp + Tower_T_6hour_2019_regression[1,6])%>%
  mutate(A01W = Tower_T_6hour_2019_regression[2,7]*tower_temp + Tower_T_6hour_2019_regression[2,6])%>%
  mutate(A02E = Tower_T_6hour_2019_regression[3,7]*tower_temp + Tower_T_6hour_2019_regression[3,6])%>%
  mutate(A02W = Tower_T_6hour_2019_regression[4,7]*tower_temp + Tower_T_6hour_2019_regression[4,6])%>%
  mutate(A03E = Tower_T_6hour_2019_regression[5,7]*tower_temp + Tower_T_6hour_2019_regression[5,6])%>%
  mutate(A03W = Tower_T_6hour_2019_regression[6,7]*tower_temp + Tower_T_6hour_2019_regression[6,6])%>%
  mutate(A04E = Tower_T_6hour_2019_regression[7,7]*tower_temp + Tower_T_6hour_2019_regression[7,6])%>%
  mutate(A04W = Tower_T_6hour_2019_regression[8,7]*tower_temp + Tower_T_6hour_2019_regression[8,6])%>%  
  mutate(B01E = Tower_T_6hour_2019_regression[9,7]*tower_temp + Tower_T_6hour_2019_regression[9,6])%>%
  mutate(B01W = Tower_T_6hour_2019_regression[10,7]*tower_temp + Tower_T_6hour_2019_regression[10,6])%>%
  mutate(B02E = Tower_T_6hour_2019_regression[11,7]*tower_temp + Tower_T_6hour_2019_regression[11,6])%>%
  mutate(B02W = Tower_T_6hour_2019_regression[12,7]*tower_temp + Tower_T_6hour_2019_regression[12,6])%>%
  mutate(B03E = Tower_T_6hour_2019_regression[13,7]*tower_temp + Tower_T_6hour_2019_regression[13,6])%>%
  mutate(B03W = Tower_T_6hour_2019_regression[14,7]*tower_temp + Tower_T_6hour_2019_regression[14,6])%>%
  mutate(B04E = Tower_T_6hour_2019_regression[15,7]*tower_temp + Tower_T_6hour_2019_regression[15,6])%>%
  mutate(B04W = Tower_T_6hour_2019_regression[16,7]*tower_temp + Tower_T_6hour_2019_regression[16,6])%>%
  mutate(C01E = Tower_T_6hour_2019_regression[17,7]*tower_temp + Tower_T_6hour_2019_regression[17,6])%>%
  mutate(C01W = Tower_T_6hour_2019_regression[18,7]*tower_temp + Tower_T_6hour_2019_regression[18,6])%>%
  mutate(C02E = Tower_T_6hour_2019_regression[19,7]*tower_temp + Tower_T_6hour_2019_regression[19,6])%>%
  mutate(C02W = Tower_T_6hour_2019_regression[20,7]*tower_temp + Tower_T_6hour_2019_regression[20,6])%>%
  mutate(C03E = Tower_T_6hour_2019_regression[21,7]*tower_temp + Tower_T_6hour_2019_regression[21,6])%>%
  mutate(C03W = Tower_T_6hour_2019_regression[22,7]*tower_temp + Tower_T_6hour_2019_regression[22,6])%>%
  mutate(C04E = Tower_T_6hour_2019_regression[23,7]*tower_temp + Tower_T_6hour_2019_regression[23,6])%>%
  mutate(C04W = Tower_T_6hour_2019_regression[24,7]*tower_temp + Tower_T_6hour_2019_regression[24,6])%>%
  mutate(D01E = Tower_T_6hour_2019_regression[25,7]*tower_temp + Tower_T_6hour_2019_regression[25,6])%>%
  mutate(D01W = Tower_T_6hour_2019_regression[26,7]*tower_temp + Tower_T_6hour_2019_regression[26,6])%>%
  mutate(D02E = Tower_T_6hour_2019_regression[27,7]*tower_temp + Tower_T_6hour_2019_regression[27,6])%>%
  mutate(D02W = Tower_T_6hour_2019_regression[28,7]*tower_temp + Tower_T_6hour_2019_regression[28,6])%>%
  mutate(D03E = Tower_T_6hour_2019_regression[29,7]*tower_temp + Tower_T_6hour_2019_regression[29,6])%>%
  mutate(D03W = Tower_T_6hour_2019_regression[30,7]*tower_temp + Tower_T_6hour_2019_regression[30,6])%>%
  mutate(D04E = Tower_T_6hour_2019_regression[31,7]*tower_temp + Tower_T_6hour_2019_regression[31,6])%>%
  mutate(D04W = Tower_T_6hour_2019_regression[32,7]*tower_temp + Tower_T_6hour_2019_regression[32,6])

  


Tower_T_6hour_2019_modeled <- gather(Tower_T_6hour_2019_modeled, Subplot_ID, modeled_temp, A01E:D04W, factor_key = TRUE)

Tower_T_6hour_2019_modeled_low <- gather(Tower_T_6hour_2019_modeled_low, Subplot_ID, modeled_temp_low, A01E:D04W, factor_key = TRUE)

Tower_T_6hour_2019_modeled_high <- gather(Tower_T_6hour_2019_modeled_high, Subplot_ID, modeled_temp_high, A01E:D04W, factor_key = TRUE)

Tower_T_6hour_2019_modeled <- merge(Tower_T_6hour_2019_modeled,Tower_T_6hour_2019_modeled_low, by = c("Timestamp_6", "tower_temp", "Subplot_ID") )

Tower_T_6hour_2019_modeled <- merge(Tower_T_6hour_2019_modeled,Tower_T_6hour_2019_modeled_high, by = c("Timestamp_6", "tower_temp", "Subplot_ID") )

write.csv(Tower_T_6hour_2019_modeled, "modeled_6hr_Ts_2019.csv", row.names=FALSE)


##plot modeled temperature data

##Create a dataframe with average modeled temp per day 

##Average across day 
Tower_T_day_2019_modeled_6 <- Tower_T_6hour_2019_modeled%>%
  group_by(Subplot_ID, Timestamp_6 = floor_date(Timestamp_6, "1 day"))%>%
  summarize(tower_temp_day = mean(tower_temp), modeled_temp_day = mean(modeled_temp))

##Figure of modeled temperature average per day for all subplots and include the tower data as well. 
ggplot(Tower_T_day_2019_modeled_6, aes(x = Timestamp_6)) +
  geom_line(aes(y = modeled_temp_day,  group = Subplot_ID, color = Subplot_ID), linewidth = 1) +
  geom_point(aes(y = tower_temp_day), color = "black", size = 3) +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  guides(color=guide_legend(ncol =1))


ggsave(path = "Figures_Output", filename = "Modeled_temp.png", height = 20, width =30, units = "in")


######################## Model SWC from tower measurements (same as temperature pipeline) #################################

####Create a SWC Only Dataframe 
tower_SWC <- tower_T_SWC%>%
  select(TIMESTAMP, SWC_1_2_1)%>%
  filter(!is.na(SWC_1_2_1))

##Average across every 6 hour 
Tower_SWC_6hour <- tower_SWC%>%
  group_by(TIMESTAMP = floor_date(TIMESTAMP, "6 hour"))%>%
  summarize(SWC_1_2_1_hour = mean(SWC_1_2_1))


##Add tower data to match the point measurements 

##Rename the tower variables and select only the 2019 Timestamps 
Tower_SWC_6hour <- Tower_SWC_6hour%>%
  rename(Timestamp_6 = TIMESTAMP, tower_SWC = SWC_1_2_1_hour)

Tower_SWC_6hour_2019 <- Tower_SWC_6hour%>%
  filter(between(Timestamp_6, as.POSIXct('2019-01-01 04:00:00'), as.POSIXct('2019-12-31 23:00:00')))

###Join Tower and point data for 2019 

Tower_SWC_6hour_2019_combined <- Tower_SWC_6hour_2019 %>% 
  left_join(point_2019_mean_VWC_6, by = "Timestamp_6")

###Run regression analyses for the relationship between tower and point measurements (SWC)

Tower_SWC_6hour_2019_regression <- Tower_SWC_6hour_2019_combined%>%
  filter(!is.na(ave_point_SWC))

###Run Regression to look at summary statistics (All models are highly significant)
fitted_model_SWC_6 <-  Tower_SWC_6hour_2019_regression%>%
  nest_by(Subplot_ID)%>%
  mutate(model = list(lm(ave_point_SWC ~ tower_SWC, data = data)))%>%
  summarize(tidy(model))

###Organize model coefficients into a dataframe 

Tower_SWC_6hour_2019_regression <- data.table(Tower_SWC_6hour_2019_regression)

Tower_SWC_6hour_2019_regression <- Tower_SWC_6hour_2019_regression[,as.list(coef(lm(ave_point_SWC ~ tower_SWC))), by=Subplot_ID]

Tower_SWC_6hour_2019_regression  <- as.data.frame.matrix(Tower_SWC_6hour_2019_regression)

###Apply subplot specific regression models to model hourly SWC in each subplot for 2019 (SWC(modeled) = m(tower_SWC) + b): LINEAR MODEL!!!!!!

Tower_SWC_6hour_2019_modeled <- Tower_SWC_6hour_2019%>%
  mutate(D02E = Tower_SWC_6hour_2019_regression[1,3]*tower_SWC +Tower_SWC_6hour_2019_regression[1,2])%>%
  mutate(D02W = Tower_SWC_6hour_2019_regression[2,3]*tower_SWC + Tower_SWC_6hour_2019_regression[2,2])%>%
  mutate(D03E = Tower_SWC_6hour_2019_regression[3,3]*tower_SWC + Tower_SWC_6hour_2019_regression[3,2])%>%
  mutate(D03W = Tower_SWC_6hour_2019_regression[4,3]*tower_SWC + Tower_SWC_6hour_2019_regression[4,2])%>%
  mutate(D04E = Tower_SWC_6hour_2019_regression[5,3]*tower_SWC + Tower_SWC_6hour_2019_regression[5,2])%>%
  mutate(D04W = Tower_SWC_6hour_2019_regression[6,3]*tower_SWC + Tower_SWC_6hour_2019_regression[6,2])%>%
  mutate(C01E = Tower_SWC_6hour_2019_regression[7,3]*tower_SWC + Tower_SWC_6hour_2019_regression[7,2])%>%
  mutate(C01W = Tower_SWC_6hour_2019_regression[8,3]*tower_SWC + Tower_SWC_6hour_2019_regression[8,2])%>%
  mutate(C02E = Tower_SWC_6hour_2019_regression[9,3]*tower_SWC + Tower_SWC_6hour_2019_regression[9,2])%>%
  mutate(C02W = Tower_SWC_6hour_2019_regression[10,3]*tower_SWC + Tower_SWC_6hour_2019_regression[10,2])%>%
  mutate(C03E = Tower_SWC_6hour_2019_regression[11,3]*tower_SWC + Tower_SWC_6hour_2019_regression[11,2])%>%
  mutate(C03W = Tower_SWC_6hour_2019_regression[12,3]*tower_SWC + Tower_SWC_6hour_2019_regression[12,2])%>%
  mutate(C04E = Tower_SWC_6hour_2019_regression[13,3]*tower_SWC + Tower_SWC_6hour_2019_regression[13,2])%>%
  mutate(C04W = Tower_SWC_6hour_2019_regression[14,3]*tower_SWC + Tower_SWC_6hour_2019_regression[14,2])%>%
  mutate(D01E = Tower_SWC_6hour_2019_regression[15,3]*tower_SWC + Tower_SWC_6hour_2019_regression[15,2])%>%
  mutate(D01W = Tower_SWC_6hour_2019_regression[16,3]*tower_SWC + Tower_SWC_6hour_2019_regression[16,2])%>%
  mutate(B01E = Tower_SWC_6hour_2019_regression[17,3]*tower_SWC + Tower_SWC_6hour_2019_regression[17,2])%>%
  mutate(B01W = Tower_SWC_6hour_2019_regression[18,3]*tower_SWC + Tower_SWC_6hour_2019_regression[18,2])%>%
  mutate(B02E = Tower_SWC_6hour_2019_regression[19,3]*tower_SWC + Tower_SWC_6hour_2019_regression[19,2])%>%
  mutate(B02W = Tower_SWC_6hour_2019_regression[20,3]*tower_SWC + Tower_SWC_6hour_2019_regression[20,2])%>%
  mutate(B03E = Tower_SWC_6hour_2019_regression[21,3]*tower_SWC + Tower_SWC_6hour_2019_regression[21,2])%>%
  mutate(B03W = Tower_SWC_6hour_2019_regression[22,3]*tower_SWC + Tower_SWC_6hour_2019_regression[22,2])%>%
  mutate(B04E = Tower_SWC_6hour_2019_regression[23,3]*tower_SWC + Tower_SWC_6hour_2019_regression[23,2])%>%
  mutate(B04W = Tower_SWC_6hour_2019_regression[24,3]*tower_SWC + Tower_SWC_6hour_2019_regression[24,2])%>%
  mutate(A01E = Tower_SWC_6hour_2019_regression[25,3]*tower_SWC + Tower_SWC_6hour_2019_regression[25,2])%>%
  mutate(A01W = Tower_SWC_6hour_2019_regression[26,3]*tower_SWC + Tower_SWC_6hour_2019_regression[26,2])%>%
  mutate(A02E = Tower_SWC_6hour_2019_regression[27,3]*tower_SWC + Tower_SWC_6hour_2019_regression[27,2])%>%
  mutate(A02W = Tower_SWC_6hour_2019_regression[28,3]*tower_SWC + Tower_SWC_6hour_2019_regression[28,2])%>%
  mutate(A03E = Tower_SWC_6hour_2019_regression[29,3]*tower_SWC + Tower_SWC_6hour_2019_regression[29,2])%>%
  mutate(A03W = Tower_SWC_6hour_2019_regression[30,3]*tower_SWC + Tower_SWC_6hour_2019_regression[30,2])%>%
  mutate(A04E = Tower_SWC_6hour_2019_regression[31,3]*tower_SWC + Tower_SWC_6hour_2019_regression[31,2])%>%
  mutate(A04W = Tower_SWC_6hour_2019_regression[32,3]*tower_SWC + Tower_SWC_6hour_2019_regression[32,2])

  
  Tower_SWC_6hour_2019_modeled <- gather(Tower_SWC_6hour_2019_modeled, Subplot_ID, modeled_SWC, D02E:A04W, factor_key = TRUE)
  
  ##plot modeled SWC data
  
  ##Create a dataframe with average modeled SWC per day 
  
  ##Average across day 
  Tower_SWC_day_2019_modeled_6 <- Tower_SWC_6hour_2019_modeled%>%
    group_by(Subplot_ID, Timestamp_6 = floor_date(Timestamp_6, "1 day"))%>%
    summarize(tower_SWC_day = mean(tower_SWC), modeled_SWC_day = mean(modeled_SWC))
  
  ##Figure of modeled temperature average per day for all subplots and include the tower data as well. 
  ggplot(Tower_SWC_day_2019_modeled_6, aes(x = Timestamp_6)) +
    geom_line(aes(y = modeled_SWC_day,  group = Subplot_ID, color = Subplot_ID), linewidth = 1) +
    geom_point(aes(y = tower_SWC_day), color = "black", size = 3) +
    theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
    guides(color=guide_legend(ncol =1))
  
  ggsave(path = "Figures_Output", filename = "Modeled_VWC_2019.png", height = 20, width =30, units = "in")
  
################################################## 2020 Data ################################################################################ 
  # Direct Google Drive link to "FoRTE/data/soil_respiration"
as_id("https://drive.google.com/drive/folders/1HHnDpTj32O-aaFavUzugzIxojf_BGEei") %>% 
    drive_ls ->
    gdfiles
  
  # Create a new data directory for files, if necessary
  data_dir <- "googledrive_data/"
  if(!dir.exists(data_dir)) dir.create(data_dir)
  
  #Download date
  for(f in seq_len(nrow(gdfiles))) {
    cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
    drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
  }
  
##Read in Rs point measurement files 
  raw_2020 <- read.csv("googledrive_data/Rs_2020.csv", na.strings = c("NA","na", ""))
  
###Combine plot level info into one subplot column
  raw_2020$Subplot_ID <- str_c(raw_2020$Rep_ID, '',raw_2020$Plot_ID,'', raw_2020$Subplot)

##Keep the columns that we need 
  point_2020 <- raw_2020%>%
    select(Subplot_ID, soilTemp, VWC, date, HH.MM.SS)  
##Convert date and time columns to datetime and combine date and time column into standard format for tower data  
  
  point_2020$date <- as.POSIXct(point_2020$date)
  
  point_2020$HH.MM.SS <- as.POSIXct(point_2020$HH.MM.SS, tz = "", format = "%H:%M:%S")
  
  point_2020$HH.MM.SS <- format(as.POSIXct(point_2020$HH.MM.SS), format = "%H")
  
  point_2020 <- point_2020%>%
    mutate(Timestamp_6 = as.POSIXct(paste(date, HH.MM.SS), format="%Y-%m-%d %H"))
  
  point_2020$year <- format(as.POSIXct(point_2020$Timestamp_6), format = "%Y")
  
  point_2020$Timestamp_6 <- as.POSIXct(format(point_2020$Timestamp_6), tz = "UTC")
  point_2020$Timestamp <- as.POSIXct(format(point_2020$Timestamp), tz = "UTC")
  
  ### Average temperature and moisture over the subplot per timestamp 
  
  point_2020_mean_temp_6 <- point_2020%>%
    group_by(date, Subplot_ID, Timestamp_6)%>%
    filter(!is.na(soilTemp))%>%
    summarize(ave_soilTemp = mean(soilTemp))
  
  
  
  point_2020_mean_VWC_6 <- point_2020%>%
    group_by(date, Subplot_ID, Timestamp_6)%>%
    filter(!is.na(VWC))%>%
    summarize(ave_VWC = mean(VWC))
  
  ##Add tower data to match the point measurements 
  
  ##Rename the tower variables and select only the 2020 Timestamps 
  
  
  Tower_T_6hour_2020 <- Tower_T_6hour%>%
    filter(between(Timestamp_6, as.POSIXct('2020-01-01 00:00:00'), as.POSIXct('2020-12-31 18:00:00')))
  
  
  ###Join Tower and point data for 2020 
  
  Tower_T_6hour_2020_combined <- Tower_T_6hour_2020 %>% 
    left_join(point_2020_mean_temp_6, by = "Timestamp_6")
  
  
  ###Run regression analyses for the relationship between tower and point measurements
  Tower_T_6hour_2020_regression <- Tower_T_6hour_2020_combined%>%
    filter(!is.na(ave_soilTemp))
  
  fitted_model_temp_6_2020 <-  Tower_T_6hour_2020_regression%>%
    nest_by(Subplot_ID)%>%
    mutate(model = list(lm(ave_soilTemp ~ tower_temp, data = data)))%>%
    summarize(tidy(model, conf.int = TRUE, conf.level = 0.95))%>%
    select(Subplot_ID, term, conf.low, conf.high)
  
  
  fitted_model_temp_6_2020 <- fitted_model_temp_6_2020%>%
    pivot_wider(names_from = term, values_from = c( conf.low, conf.high))
  
  ###Organize model coefficients into a dataframe 
  library(data.table)
  # 
  Tower_T_6hour_2020_regression <- data.table(Tower_T_6hour_2020_regression)
  
  Tower_T_6hour_2020_regression <- Tower_T_6hour_2020_regression[,as.list(coef(lm(ave_soilTemp ~ tower_temp))), by=Subplot_ID]
  # 
  Tower_T_6hour_2020_regression <- as.data.frame.matrix(Tower_T_6hour_2020_regression)
  
  Tower_T_6hour_2020_regression <- merge(Tower_T_6hour_2020_regression,fitted_model_temp_6_2020, by = "Subplot_ID" )
  
  
  
  Tower_T_6hour_2020_modeled <- Tower_T_6hour_2020%>%
    mutate(A01E = Tower_T_6hour_2020_regression[1,3]*tower_temp + Tower_T_6hour_2020_regression[1,2])%>%
    mutate(A01W = Tower_T_6hour_2020_regression[2,3]*tower_temp + Tower_T_6hour_2020_regression[2,2])%>%
    mutate(A02E = Tower_T_6hour_2020_regression[3,3]*tower_temp + Tower_T_6hour_2020_regression[3,2])%>%
    mutate(A02W = Tower_T_6hour_2020_regression[4,3]*tower_temp + Tower_T_6hour_2020_regression[4,2])%>%
    mutate(A03E = Tower_T_6hour_2020_regression[5,3]*tower_temp + Tower_T_6hour_2020_regression[5,2])%>%
    mutate(A03W = Tower_T_6hour_2020_regression[6,3]*tower_temp + Tower_T_6hour_2020_regression[6,2])%>%
    mutate(A04E = Tower_T_6hour_2020_regression[7,3]*tower_temp + Tower_T_6hour_2020_regression[7,2])%>%
    mutate(A04W = Tower_T_6hour_2020_regression[8,3]*tower_temp + Tower_T_6hour_2020_regression[8,2])%>%  
    mutate(B01E = Tower_T_6hour_2020_regression[9,3]*tower_temp + Tower_T_6hour_2020_regression[9,2])%>%
    mutate(B01W = Tower_T_6hour_2020_regression[10,3]*tower_temp + Tower_T_6hour_2020_regression[10,2])%>%
    mutate(B02E = Tower_T_6hour_2020_regression[11,3]*tower_temp + Tower_T_6hour_2020_regression[11,2])%>%
    mutate(B02W = Tower_T_6hour_2020_regression[12,3]*tower_temp + Tower_T_6hour_2020_regression[12,2])%>%
    mutate(B03E = Tower_T_6hour_2020_regression[13,3]*tower_temp + Tower_T_6hour_2020_regression[13,2])%>%
    mutate(B03W = Tower_T_6hour_2020_regression[14,3]*tower_temp + Tower_T_6hour_2020_regression[14,2])%>%
    mutate(B04E = Tower_T_6hour_2020_regression[15,3]*tower_temp + Tower_T_6hour_2020_regression[15,2])%>%
    mutate(B04W = Tower_T_6hour_2020_regression[16,3]*tower_temp + Tower_T_6hour_2020_regression[16,2])%>%
    mutate(C01E = Tower_T_6hour_2020_regression[17,3]*tower_temp + Tower_T_6hour_2020_regression[17,2])%>%
    mutate(C01W = Tower_T_6hour_2020_regression[18,3]*tower_temp + Tower_T_6hour_2020_regression[18,2])%>%
    mutate(C02E = Tower_T_6hour_2020_regression[19,3]*tower_temp + Tower_T_6hour_2020_regression[19,2])%>%
    mutate(C02W = Tower_T_6hour_2020_regression[20,3]*tower_temp + Tower_T_6hour_2020_regression[20,2])%>%
    mutate(C03E = Tower_T_6hour_2020_regression[21,3]*tower_temp + Tower_T_6hour_2020_regression[21,2])%>%
    mutate(C03W = Tower_T_6hour_2020_regression[22,3]*tower_temp + Tower_T_6hour_2020_regression[22,2])%>%
    mutate(C04E = Tower_T_6hour_2020_regression[23,3]*tower_temp + Tower_T_6hour_2020_regression[23,2])%>%
    mutate(C04W = Tower_T_6hour_2020_regression[24,3]*tower_temp + Tower_T_6hour_2020_regression[24,2])%>%
    mutate(D01E = Tower_T_6hour_2020_regression[25,3]*tower_temp + Tower_T_6hour_2020_regression[25,2])%>%
    mutate(D01W = Tower_T_6hour_2020_regression[26,3]*tower_temp + Tower_T_6hour_2020_regression[26,2])%>%
    mutate(D02E = Tower_T_6hour_2020_regression[27,3]*tower_temp + Tower_T_6hour_2020_regression[27,2])%>%
    mutate(D02W = Tower_T_6hour_2020_regression[28,3]*tower_temp + Tower_T_6hour_2020_regression[28,2])%>%
    mutate(D03E = Tower_T_6hour_2020_regression[29,3]*tower_temp + Tower_T_6hour_2020_regression[29,2])%>%
    mutate(D03W = Tower_T_6hour_2020_regression[30,3]*tower_temp + Tower_T_6hour_2020_regression[30,2])%>%
    mutate(D04E = Tower_T_6hour_2020_regression[31,3]*tower_temp + Tower_T_6hour_2020_regression[31,2])%>%
    mutate(D04W = Tower_T_6hour_2020_regression[32,3]*tower_temp + Tower_T_6hour_2020_regression[32,2])
  
  ##low
  Tower_T_6hour_2020_modeled_low <- Tower_T_6hour_2020%>%
    mutate(A01E = Tower_T_6hour_2020_regression[1,5]*tower_temp + Tower_T_6hour_2020_regression[1,2])%>%
    mutate(A01W = Tower_T_6hour_2020_regression[2,5]*tower_temp + Tower_T_6hour_2020_regression[2,4])%>%
    mutate(A02E = Tower_T_6hour_2020_regression[3,5]*tower_temp + Tower_T_6hour_2020_regression[3,4])%>%
    mutate(A02W = Tower_T_6hour_2020_regression[4,5]*tower_temp + Tower_T_6hour_2020_regression[4,4])%>%
    mutate(A03E = Tower_T_6hour_2020_regression[5,5]*tower_temp + Tower_T_6hour_2020_regression[5,4])%>%
    mutate(A03W = Tower_T_6hour_2020_regression[6,5]*tower_temp + Tower_T_6hour_2020_regression[6,4])%>%
    mutate(A04E = Tower_T_6hour_2020_regression[7,5]*tower_temp + Tower_T_6hour_2020_regression[7,4])%>%
    mutate(A04W = Tower_T_6hour_2020_regression[8,5]*tower_temp + Tower_T_6hour_2020_regression[8,4])%>%  
    mutate(B01E = Tower_T_6hour_2020_regression[9,5]*tower_temp + Tower_T_6hour_2020_regression[9,4])%>%
    mutate(B01W = Tower_T_6hour_2020_regression[10,5]*tower_temp + Tower_T_6hour_2020_regression[10,4])%>%
    mutate(B02E = Tower_T_6hour_2020_regression[11,5]*tower_temp + Tower_T_6hour_2020_regression[11,4])%>%
    mutate(B02W = Tower_T_6hour_2020_regression[12,5]*tower_temp + Tower_T_6hour_2020_regression[12,4])%>%
    mutate(B03E = Tower_T_6hour_2020_regression[13,5]*tower_temp + Tower_T_6hour_2020_regression[13,4])%>%
    mutate(B03W = Tower_T_6hour_2020_regression[14,5]*tower_temp + Tower_T_6hour_2020_regression[14,4])%>%
    mutate(B04E = Tower_T_6hour_2020_regression[15,5]*tower_temp + Tower_T_6hour_2020_regression[15,4])%>%
    mutate(B04W = Tower_T_6hour_2020_regression[16,5]*tower_temp + Tower_T_6hour_2020_regression[16,4])%>%
    mutate(C01E = Tower_T_6hour_2020_regression[17,5]*tower_temp + Tower_T_6hour_2020_regression[17,4])%>%
    mutate(C01W = Tower_T_6hour_2020_regression[18,5]*tower_temp + Tower_T_6hour_2020_regression[18,4])%>%
    mutate(C02E = Tower_T_6hour_2020_regression[19,5]*tower_temp + Tower_T_6hour_2020_regression[19,4])%>%
    mutate(C02W = Tower_T_6hour_2020_regression[20,5]*tower_temp + Tower_T_6hour_2020_regression[20,4])%>%
    mutate(C03E = Tower_T_6hour_2020_regression[21,5]*tower_temp + Tower_T_6hour_2020_regression[21,4])%>%
    mutate(C03W = Tower_T_6hour_2020_regression[22,5]*tower_temp + Tower_T_6hour_2020_regression[22,4])%>%
    mutate(C04E = Tower_T_6hour_2020_regression[23,5]*tower_temp + Tower_T_6hour_2020_regression[23,4])%>%
    mutate(C04W = Tower_T_6hour_2020_regression[24,5]*tower_temp + Tower_T_6hour_2020_regression[24,4])%>%
    mutate(D01E = Tower_T_6hour_2020_regression[25,5]*tower_temp + Tower_T_6hour_2020_regression[25,4])%>%
    mutate(D01W = Tower_T_6hour_2020_regression[26,5]*tower_temp + Tower_T_6hour_2020_regression[26,4])%>%
    mutate(D02E = Tower_T_6hour_2020_regression[27,5]*tower_temp + Tower_T_6hour_2020_regression[27,4])%>%
    mutate(D02W = Tower_T_6hour_2020_regression[28,5]*tower_temp + Tower_T_6hour_2020_regression[28,4])%>%
    mutate(D03E = Tower_T_6hour_2020_regression[29,5]*tower_temp + Tower_T_6hour_2020_regression[29,4])%>%
    mutate(D03W = Tower_T_6hour_2020_regression[30,5]*tower_temp + Tower_T_6hour_2020_regression[30,4])%>%
    mutate(D04E = Tower_T_6hour_2020_regression[31,5]*tower_temp + Tower_T_6hour_2020_regression[31,4])%>%
    mutate(D04W = Tower_T_6hour_2020_regression[32,5]*tower_temp + Tower_T_6hour_2020_regression[32,4])
  
  
  ##high
  Tower_T_6hour_2020_modeled_high <- Tower_T_6hour_2020%>%
    mutate(A01E = Tower_T_6hour_2020_regression[1,7]*tower_temp + Tower_T_6hour_2020_regression[1,6])%>%
    mutate(A01W = Tower_T_6hour_2020_regression[2,7]*tower_temp + Tower_T_6hour_2020_regression[2,6])%>%
    mutate(A02E = Tower_T_6hour_2020_regression[3,7]*tower_temp + Tower_T_6hour_2020_regression[3,6])%>%
    mutate(A02W = Tower_T_6hour_2020_regression[4,7]*tower_temp + Tower_T_6hour_2020_regression[4,6])%>%
    mutate(A03E = Tower_T_6hour_2020_regression[5,7]*tower_temp + Tower_T_6hour_2020_regression[5,6])%>%
    mutate(A03W = Tower_T_6hour_2020_regression[6,7]*tower_temp + Tower_T_6hour_2020_regression[6,6])%>%
    mutate(A04E = Tower_T_6hour_2020_regression[7,7]*tower_temp + Tower_T_6hour_2020_regression[7,6])%>%
    mutate(A04W = Tower_T_6hour_2020_regression[8,7]*tower_temp + Tower_T_6hour_2020_regression[8,6])%>%  
    mutate(B01E = Tower_T_6hour_2020_regression[9,7]*tower_temp + Tower_T_6hour_2020_regression[9,6])%>%
    mutate(B01W = Tower_T_6hour_2020_regression[10,7]*tower_temp + Tower_T_6hour_2020_regression[10,6])%>%
    mutate(B02E = Tower_T_6hour_2020_regression[11,7]*tower_temp + Tower_T_6hour_2020_regression[11,6])%>%
    mutate(B02W = Tower_T_6hour_2020_regression[12,7]*tower_temp + Tower_T_6hour_2020_regression[12,6])%>%
    mutate(B03E = Tower_T_6hour_2020_regression[13,7]*tower_temp + Tower_T_6hour_2020_regression[13,6])%>%
    mutate(B03W = Tower_T_6hour_2020_regression[14,7]*tower_temp + Tower_T_6hour_2020_regression[14,6])%>%
    mutate(B04E = Tower_T_6hour_2020_regression[15,7]*tower_temp + Tower_T_6hour_2020_regression[15,6])%>%
    mutate(B04W = Tower_T_6hour_2020_regression[16,7]*tower_temp + Tower_T_6hour_2020_regression[16,6])%>%
    mutate(C01E = Tower_T_6hour_2020_regression[17,7]*tower_temp + Tower_T_6hour_2020_regression[17,6])%>%
    mutate(C01W = Tower_T_6hour_2020_regression[18,7]*tower_temp + Tower_T_6hour_2020_regression[18,6])%>%
    mutate(C02E = Tower_T_6hour_2020_regression[19,7]*tower_temp + Tower_T_6hour_2020_regression[19,6])%>%
    mutate(C02W = Tower_T_6hour_2020_regression[20,7]*tower_temp + Tower_T_6hour_2020_regression[20,6])%>%
    mutate(C03E = Tower_T_6hour_2020_regression[21,7]*tower_temp + Tower_T_6hour_2020_regression[21,6])%>%
    mutate(C03W = Tower_T_6hour_2020_regression[22,7]*tower_temp + Tower_T_6hour_2020_regression[22,6])%>%
    mutate(C04E = Tower_T_6hour_2020_regression[23,7]*tower_temp + Tower_T_6hour_2020_regression[23,6])%>%
    mutate(C04W = Tower_T_6hour_2020_regression[24,7]*tower_temp + Tower_T_6hour_2020_regression[24,6])%>%
    mutate(D01E = Tower_T_6hour_2020_regression[25,7]*tower_temp + Tower_T_6hour_2020_regression[25,6])%>%
    mutate(D01W = Tower_T_6hour_2020_regression[26,7]*tower_temp + Tower_T_6hour_2020_regression[26,6])%>%
    mutate(D02E = Tower_T_6hour_2020_regression[27,7]*tower_temp + Tower_T_6hour_2020_regression[27,6])%>%
    mutate(D02W = Tower_T_6hour_2020_regression[28,7]*tower_temp + Tower_T_6hour_2020_regression[28,6])%>%
    mutate(D03E = Tower_T_6hour_2020_regression[29,7]*tower_temp + Tower_T_6hour_2020_regression[29,6])%>%
    mutate(D03W = Tower_T_6hour_2020_regression[30,7]*tower_temp + Tower_T_6hour_2020_regression[30,6])%>%
    mutate(D04E = Tower_T_6hour_2020_regression[31,7]*tower_temp + Tower_T_6hour_2020_regression[31,6])%>%
    mutate(D04W = Tower_T_6hour_2020_regression[32,7]*tower_temp + Tower_T_6hour_2020_regression[32,6])
  
  Tower_T_6hour_2020_modeled <- gather(Tower_T_6hour_2020_modeled, Subplot_ID, modeled_temp, A01E:D04W, factor_key = TRUE)
  
  Tower_T_6hour_2020_modeled_low <- gather(Tower_T_6hour_2020_modeled_low, Subplot_ID, modeled_temp_low, A01E:D04W, factor_key = TRUE)
  
  Tower_T_6hour_2020_modeled_high <- gather(Tower_T_6hour_2020_modeled_high, Subplot_ID, modeled_temp_high, A01E:D04W, factor_key = TRUE)
  
  Tower_T_6hour_2020_modeled <- merge(Tower_T_6hour_2020_modeled,Tower_T_6hour_2020_modeled_low, by = c("Timestamp_6", "tower_temp", "Subplot_ID") )
  
  Tower_T_6hour_2020_modeled <- merge(Tower_T_6hour_2020_modeled,Tower_T_6hour_2020_modeled_high, by = c("Timestamp_6", "tower_temp", "Subplot_ID") )
  
  write.csv(Tower_T_6hour_2020_modeled, "modeled_6hr_Ts_2020.csv", row.names=FALSE)
  
  ##plot modeled temperature data
  
  ##Create a dataframe with average modeled temp per day 
  
  ##Average across day 
  Tower_T_day_2020_modeled_6 <- Tower_T_6hour_2020_modeled%>%
    group_by(Subplot_ID, Timestamp_6 = floor_date(Timestamp_6, "1 day"))%>%
    summarize(tower_temp_day = mean(tower_temp), modeled_temp_day = mean(modeled_temp))
  
  ##Figure of modeled temperature average per day for all subplots and include the tower data as well. 
  ggplot(Tower_T_day_2020_modeled_6, aes(x = Timestamp_6)) +
    geom_line(aes(y = modeled_temp_day,  group = Subplot_ID, color = Subplot_ID), linewidth = 1) +
    geom_point(aes(y = tower_temp_day), color = "black", size = 3) +
    theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
    guides(color=guide_legend(ncol =1))
  
  ggsave(path = "Figures_Output", filename = "Modeled_T_2020.png", height = 20, width =30, units = "in")
  
  
  ######################## Model SWC from tower measurements (same as temperature pipeline) #################################
  
  
  
  ##Add tower data to match the point measurements 
  
  ##Rename the tower variables and select only the 2020 Timestamps 
  
  Tower_SWC_6hour_2020 <- Tower_SWC_6hour%>%
    filter(between(Timestamp_6, as.POSIXct('2020-01-01 04:00:00'), as.POSIXct('2020-12-31 23:00:00')))
  
  ###Join Tower and point data for 2020
  
  Tower_SWC_6hour_2020_combined <- Tower_SWC_6hour_2020 %>% 
    left_join(point_2020_mean_VWC_6, by = "Timestamp_6")
  
  ###Run regression analyses for the relationship between tower and point measurements (SWC)
  
  Tower_SWC_6hour_2020_regression <- Tower_SWC_6hour_2020_combined%>%
    filter(!is.na(ave_VWC))
  
  ###Run Regression to look at summary statistics (All models are highly significant)
  fitted_model_SWC_6_2020 <-  Tower_SWC_6hour_2020_regression%>%
    nest_by(Subplot_ID)%>%
    mutate(model = list(lm(ave_VWC ~ tower_SWC, data = data)))%>%
    summarize(tidy(model))
  
  ###Organize model coefficients into a dataframe 
  
  Tower_SWC_6hour_2020_regression <- data.table(Tower_SWC_6hour_2020_regression)
  
  Tower_SWC_6hour_2020_regression <- Tower_SWC_6hour_2020_regression[,as.list(coef(lm(ave_VWC ~ tower_SWC))), by=Subplot_ID]
  
  Tower_SWC_6hour_2020_regression  <- as.data.frame.matrix(Tower_SWC_6hour_2020_regression)
  
  ###Apply subplot specific regression models to model hourly SWC in each subplot for 2021 (SWC(modeled) = m(tower_SWC) + b): LINEAR MODEL!!!!!!
  
  Tower_SWC_6hour_2020_modeled <- Tower_SWC_6hour_2020%>%
    mutate(B01E = Tower_SWC_6hour_2020_regression[1,3]*tower_SWC + Tower_SWC_6hour_2020_regression[1,2])%>%
    mutate(B01W = Tower_SWC_6hour_2020_regression[2,3]*tower_SWC + Tower_SWC_6hour_2020_regression[2,2])%>%
    mutate(B02E = Tower_SWC_6hour_2020_regression[3,3]*tower_SWC + Tower_SWC_6hour_2020_regression[3,2])%>%
    mutate(B02W = Tower_SWC_6hour_2020_regression[4,3]*tower_SWC + Tower_SWC_6hour_2020_regression[4,2])%>%
    mutate(B03E = Tower_SWC_6hour_2020_regression[5,3]*tower_SWC + Tower_SWC_6hour_2020_regression[5,2])%>%
    mutate(B03W = Tower_SWC_6hour_2020_regression[6,3]*tower_SWC + Tower_SWC_6hour_2020_regression[6,2])%>%
    mutate(B04E = Tower_SWC_6hour_2020_regression[7,3]*tower_SWC + Tower_SWC_6hour_2020_regression[7,2])%>%
    mutate(B04W = Tower_SWC_6hour_2020_regression[8,3]*tower_SWC + Tower_SWC_6hour_2020_regression[8,2])%>%
    mutate(A01E = Tower_SWC_6hour_2020_regression[9,3]*tower_SWC + Tower_SWC_6hour_2020_regression[9,2])%>%
    mutate(A01W = Tower_SWC_6hour_2020_regression[10,3]*tower_SWC + Tower_SWC_6hour_2020_regression[10,2])%>%
    mutate(A02E = Tower_SWC_6hour_2020_regression[11,3]*tower_SWC + Tower_SWC_6hour_2020_regression[11,2])%>%
    mutate(A02W = Tower_SWC_6hour_2020_regression[12,3]*tower_SWC + Tower_SWC_6hour_2020_regression[12,2])%>%
    mutate(A03E = Tower_SWC_6hour_2020_regression[13,3]*tower_SWC + Tower_SWC_6hour_2020_regression[13,2])%>%
    mutate(A03W = Tower_SWC_6hour_2020_regression[14,3]*tower_SWC + Tower_SWC_6hour_2020_regression[14,2])%>%
    mutate(A04E = Tower_SWC_6hour_2020_regression[15,3]*tower_SWC + Tower_SWC_6hour_2020_regression[15,2])%>%
    mutate(A04W = Tower_SWC_6hour_2020_regression[16,3]*tower_SWC + Tower_SWC_6hour_2020_regression[16,2])%>%
    mutate(C01E = Tower_SWC_6hour_2020_regression[17,3]*tower_SWC + Tower_SWC_6hour_2020_regression[17,2])%>%
    mutate(C01W = Tower_SWC_6hour_2020_regression[18,3]*tower_SWC + Tower_SWC_6hour_2020_regression[18,2])%>%
    mutate(C02E = Tower_SWC_6hour_2020_regression[19,3]*tower_SWC + Tower_SWC_6hour_2020_regression[19,2])%>%
    mutate(C02W = Tower_SWC_6hour_2020_regression[20,3]*tower_SWC + Tower_SWC_6hour_2020_regression[20,2])%>%
    mutate(C03E = Tower_SWC_6hour_2020_regression[21,3]*tower_SWC + Tower_SWC_6hour_2020_regression[21,2])%>%
    mutate(C03W = Tower_SWC_6hour_2020_regression[22,3]*tower_SWC + Tower_SWC_6hour_2020_regression[22,2])%>%
    mutate(C04E = Tower_SWC_6hour_2020_regression[23,3]*tower_SWC + Tower_SWC_6hour_2020_regression[23,2])%>%
    mutate(C04W = Tower_SWC_6hour_2020_regression[24,3]*tower_SWC + Tower_SWC_6hour_2020_regression[24,2])%>%
    mutate(D01E = Tower_SWC_6hour_2020_regression[25,3]*tower_SWC + Tower_SWC_6hour_2020_regression[25,2])%>%
    mutate(D01W = Tower_SWC_6hour_2020_regression[26,3]*tower_SWC + Tower_SWC_6hour_2020_regression[26,2])%>%
    mutate(D02E = Tower_SWC_6hour_2020_regression[27,3]*tower_SWC + Tower_SWC_6hour_2020_regression[27,2])%>%
    mutate(D02W = Tower_SWC_6hour_2020_regression[28,3]*tower_SWC + Tower_SWC_6hour_2020_regression[28,2])%>%
    mutate(D03E = Tower_SWC_6hour_2020_regression[29,3]*tower_SWC + Tower_SWC_6hour_2020_regression[29,2])%>%
    mutate(D03W = Tower_SWC_6hour_2020_regression[30,3]*tower_SWC + Tower_SWC_6hour_2020_regression[30,2])%>%
    mutate(D04E = Tower_SWC_6hour_2020_regression[31,3]*tower_SWC + Tower_SWC_6hour_2020_regression[31,2])%>%
    mutate(D04W = Tower_SWC_6hour_2020_regression[32,3]*tower_SWC + Tower_SWC_6hour_2020_regression[32,2])
  
  
  
  Tower_SWC_6hour_2020_modeled <- gather(Tower_SWC_6hour_2020_modeled, Subplot_ID, modeled_SWC, B01E:D04W, factor_key = TRUE)
  
  ##plot modeled SWC data
  
  ##Create a dataframe with average modeled SWC per day 
  
  ##Average across day 
  Tower_SWC_day_2020_modeled_6 <- Tower_SWC_6hour_2020_modeled%>%
    group_by(Subplot_ID, Timestamp_6 = floor_date(Timestamp_6, "1 day"))%>%
    summarize(tower_SWC_day = mean(tower_SWC), modeled_SWC_day = mean(modeled_SWC))
  
  ##Figure of modeled SWC average per day for all subplots and include the tower data as well. 
  ggplot(Tower_SWC_day_2020_modeled_6, aes(x = Timestamp_6)) +
    geom_line(aes(y = modeled_SWC_day,  group = Subplot_ID, color = Subplot_ID), linewidth = 1) +
    geom_point(aes(y = tower_SWC_day), color = "black", size = 3) +
    theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
    guides(color=guide_legend(ncol =1))
  
  ggsave(path = "Figures_Output", filename = "Modeled_VWC_2020.png", height = 20, width =30, units = "in")
  

  
################################################## 2021 Data ############################################################################
  
  ##Read in Rs point measurement files 
  raw_2021 <- read.csv("googledrive_data/Rs_2021.csv", na.strings = c("NA","na", ""))
  
  ###Combine plot level info into one subplot column
  raw_2021$Subplot_ID <- str_c(raw_2021$Rep_ID, '',raw_2021$Plot_ID,'', raw_2021$Subplot)
  
  ##Keep the columns that we need 
  point_2021 <- raw_2021%>%
    select(Subplot_ID, soilTemp, VWC, date, time)  
  ##Convert date and time columns to datetime and combine date and time column into standard format for tower data  
  
  point_2021$date <- as.POSIXct(point_2021$date)
  
  
  point_2021$time <- as.POSIXct(point_2021$time, tz = "", format = "%H:%M:%S")
  
  point_2021$time <- format(as.POSIXct(point_2021$time), format = "%H")
  
  point_2021 <- point_2021%>%
    mutate(Timestamp_6 = as.POSIXct(paste(date, time), format="%Y-%m-%d %H"))
  
  point_2021 <-   point_2021%>%
    mutate(Timestamp_6 = as.POSIXct(paste(date, time), format="%Y-%m-%d %H"))
  
  point_2021$year <- format(as.POSIXct(point_2021$Timestamp), format = "%Y")
  
  point_2021$Timestamp_6 <- as.POSIXct(format(point_2021$Timestamp_6), tz = "UTC")
  point_2021$Timestamp <- as.POSIXct(format(point_2021$Timestamp), tz = "UTC")
  
  ### Average temperature and moisture over the subplot per timestamp 
  
  point_2021_mean_temp_6 <- point_2021%>%
    group_by(date, Subplot_ID, Timestamp_6)%>%
    filter(!is.na(soilTemp))%>%
    summarize(ave_soilTemp = mean(soilTemp))
  
  
  
  point_2021_mean_VWC_6 <- point_2021%>%
    group_by(date, Subplot_ID, Timestamp_6)%>%
    filter(!is.na(VWC))%>%
    summarize(ave_VWC = mean(VWC))
  
  ##Add tower data to match the point measurements 
  
  ##Rename the tower variables and select only the 2020 Timestamps 
  
  
  Tower_T_6hour_2021 <- Tower_T_6hour%>%
    filter(between(Timestamp_6, as.POSIXct('2021-01-01 00:00:00'), as.POSIXct('2021-12-31 18:00:00')))
  
  
  ###Join Tower and point data for 2020 
  
  Tower_T_6hour_2021_combined <- Tower_T_6hour_2021 %>% 
    left_join(point_2021_mean_temp_6, by = "Timestamp_6")
  
  
  ###Run regression analyses for the relationship between tower and point measurements
  Tower_T_6hour_2021_regression <- Tower_T_6hour_2021_combined%>%
    filter(!is.na(ave_soilTemp))
  
  fitted_model_temp_6_2021 <-  Tower_T_6hour_2021_regression%>%
    nest_by(Subplot_ID)%>%
    mutate(model = list(lm(ave_soilTemp ~ tower_temp, data = data)))%>%
    summarize(tidy(model, conf.int = TRUE, conf.level = 0.95))%>%
    select(Subplot_ID, term, conf.low, conf.high)
  
  
  fitted_model_temp_6_2021 <- fitted_model_temp_6_2021%>%
    pivot_wider(names_from = term, values_from = c( conf.low, conf.high))
  
  ###Organize model coefficients into a dataframe 
  library(data.table)
  # 
  Tower_T_6hour_2021_regression <- data.table(Tower_T_6hour_2021_regression)
  
  Tower_T_6hour_2021_regression <- Tower_T_6hour_2021_regression[,as.list(coef(lm(ave_soilTemp ~ tower_temp))), by=Subplot_ID]
  # 
  Tower_T_6hour_2021_regression <- as.data.frame.matrix(Tower_T_6hour_2021_regression)
  
  Tower_T_6hour_2021_regression <- merge(Tower_T_6hour_2021_regression,fitted_model_temp_6_2021, by = "Subplot_ID" )
  
  
  Tower_T_6hour_2021_modeled <- Tower_T_6hour_2021%>%
    mutate(A01E = Tower_T_6hour_2021_regression[1,3]*tower_temp + Tower_T_6hour_2021_regression[1,2])%>%
    mutate(A01W = Tower_T_6hour_2021_regression[2,3]*tower_temp + Tower_T_6hour_2021_regression[2,2])%>%
    mutate(A02E = Tower_T_6hour_2021_regression[3,3]*tower_temp + Tower_T_6hour_2021_regression[3,2])%>%
    mutate(A02W = Tower_T_6hour_2021_regression[4,3]*tower_temp + Tower_T_6hour_2021_regression[4,2])%>%
    mutate(A03E = Tower_T_6hour_2021_regression[5,3]*tower_temp + Tower_T_6hour_2021_regression[5,2])%>%
    mutate(A03W = Tower_T_6hour_2021_regression[6,3]*tower_temp + Tower_T_6hour_2021_regression[6,2])%>%
    mutate(A04E = Tower_T_6hour_2021_regression[7,3]*tower_temp + Tower_T_6hour_2021_regression[7,2])%>%
    mutate(A04W = Tower_T_6hour_2021_regression[8,3]*tower_temp + Tower_T_6hour_2021_regression[8,2])%>%  
    mutate(B01E = Tower_T_6hour_2021_regression[9,3]*tower_temp + Tower_T_6hour_2021_regression[9,2])%>%
    mutate(B01W = Tower_T_6hour_2021_regression[10,3]*tower_temp + Tower_T_6hour_2021_regression[10,2])%>%
    mutate(B02E = Tower_T_6hour_2021_regression[11,3]*tower_temp + Tower_T_6hour_2021_regression[11,2])%>%
    mutate(B02W = Tower_T_6hour_2021_regression[12,3]*tower_temp + Tower_T_6hour_2021_regression[12,2])%>%
    mutate(B03E = Tower_T_6hour_2021_regression[13,3]*tower_temp + Tower_T_6hour_2021_regression[13,2])%>%
    mutate(B03W = Tower_T_6hour_2021_regression[14,3]*tower_temp + Tower_T_6hour_2021_regression[14,2])%>%
    mutate(B04E = Tower_T_6hour_2021_regression[15,3]*tower_temp + Tower_T_6hour_2021_regression[15,2])%>%
    mutate(B04W = Tower_T_6hour_2021_regression[16,3]*tower_temp + Tower_T_6hour_2021_regression[16,2])%>%
    mutate(C01E = Tower_T_6hour_2021_regression[17,3]*tower_temp + Tower_T_6hour_2021_regression[17,2])%>%
    mutate(C01W = Tower_T_6hour_2021_regression[18,3]*tower_temp + Tower_T_6hour_2021_regression[18,2])%>%
    mutate(C02E = Tower_T_6hour_2021_regression[19,3]*tower_temp + Tower_T_6hour_2021_regression[19,2])%>%
    mutate(C02W = Tower_T_6hour_2021_regression[20,3]*tower_temp + Tower_T_6hour_2021_regression[20,2])%>%
    mutate(C03E = Tower_T_6hour_2021_regression[21,3]*tower_temp + Tower_T_6hour_2021_regression[21,2])%>%
    mutate(C03W = Tower_T_6hour_2021_regression[22,3]*tower_temp + Tower_T_6hour_2021_regression[22,2])%>%
    mutate(C04E = Tower_T_6hour_2021_regression[23,3]*tower_temp + Tower_T_6hour_2021_regression[23,2])%>%
    mutate(C04W = Tower_T_6hour_2021_regression[24,3]*tower_temp + Tower_T_6hour_2021_regression[24,2])%>%
    mutate(D01E = Tower_T_6hour_2021_regression[25,3]*tower_temp + Tower_T_6hour_2021_regression[25,2])%>%
    mutate(D01W = Tower_T_6hour_2021_regression[26,3]*tower_temp + Tower_T_6hour_2021_regression[26,2])%>%
    mutate(D02E = Tower_T_6hour_2021_regression[27,3]*tower_temp + Tower_T_6hour_2021_regression[27,2])%>%
    mutate(D02W = Tower_T_6hour_2021_regression[28,3]*tower_temp + Tower_T_6hour_2021_regression[28,2])%>%
    mutate(D03E = Tower_T_6hour_2021_regression[29,3]*tower_temp + Tower_T_6hour_2021_regression[29,2])%>%
    mutate(D03W = Tower_T_6hour_2021_regression[30,3]*tower_temp + Tower_T_6hour_2021_regression[30,2])%>%
    mutate(D04E = Tower_T_6hour_2021_regression[31,3]*tower_temp + Tower_T_6hour_2021_regression[31,2])%>%
    mutate(D04W = Tower_T_6hour_2021_regression[32,3]*tower_temp + Tower_T_6hour_2021_regression[32,2])
  
  ##low
  Tower_T_6hour_2021_modeled_low <- Tower_T_6hour_2021%>%
    mutate(A01E = Tower_T_6hour_2021_regression[1,5]*tower_temp + Tower_T_6hour_2021_regression[1,4])%>%
    mutate(A01W = Tower_T_6hour_2021_regression[2,5]*tower_temp + Tower_T_6hour_2021_regression[2,4])%>%
    mutate(A02E = Tower_T_6hour_2021_regression[3,5]*tower_temp + Tower_T_6hour_2021_regression[3,4])%>%
    mutate(A02W = Tower_T_6hour_2021_regression[4,5]*tower_temp + Tower_T_6hour_2021_regression[4,4])%>%
    mutate(A03E = Tower_T_6hour_2021_regression[5,5]*tower_temp + Tower_T_6hour_2021_regression[5,4])%>%
    mutate(A03W = Tower_T_6hour_2021_regression[6,5]*tower_temp + Tower_T_6hour_2021_regression[6,4])%>%
    mutate(A04E = Tower_T_6hour_2021_regression[7,5]*tower_temp + Tower_T_6hour_2021_regression[7,4])%>%
    mutate(A04W = Tower_T_6hour_2021_regression[8,5]*tower_temp + Tower_T_6hour_2021_regression[8,4])%>%  
    mutate(B01E = Tower_T_6hour_2021_regression[9,5]*tower_temp + Tower_T_6hour_2021_regression[9,4])%>%
    mutate(B01W = Tower_T_6hour_2021_regression[10,5]*tower_temp + Tower_T_6hour_2021_regression[10,4])%>%
    mutate(B02E = Tower_T_6hour_2021_regression[11,5]*tower_temp + Tower_T_6hour_2021_regression[11,4])%>%
    mutate(B02W = Tower_T_6hour_2021_regression[12,5]*tower_temp + Tower_T_6hour_2021_regression[12,4])%>%
    mutate(B03E = Tower_T_6hour_2021_regression[13,5]*tower_temp + Tower_T_6hour_2021_regression[13,4])%>%
    mutate(B03W = Tower_T_6hour_2021_regression[14,5]*tower_temp + Tower_T_6hour_2021_regression[14,4])%>%
    mutate(B04E = Tower_T_6hour_2021_regression[15,5]*tower_temp + Tower_T_6hour_2021_regression[15,4])%>%
    mutate(B04W = Tower_T_6hour_2021_regression[16,5]*tower_temp + Tower_T_6hour_2021_regression[16,4])%>%
    mutate(C01E = Tower_T_6hour_2021_regression[17,5]*tower_temp + Tower_T_6hour_2021_regression[17,4])%>%
    mutate(C01W = Tower_T_6hour_2021_regression[18,5]*tower_temp + Tower_T_6hour_2021_regression[18,4])%>%
    mutate(C02E = Tower_T_6hour_2021_regression[19,5]*tower_temp + Tower_T_6hour_2021_regression[19,4])%>%
    mutate(C02W = Tower_T_6hour_2021_regression[20,5]*tower_temp + Tower_T_6hour_2021_regression[20,4])%>%
    mutate(C03E = Tower_T_6hour_2021_regression[21,5]*tower_temp + Tower_T_6hour_2021_regression[21,4])%>%
    mutate(C03W = Tower_T_6hour_2021_regression[22,5]*tower_temp + Tower_T_6hour_2021_regression[22,4])%>%
    mutate(C04E = Tower_T_6hour_2021_regression[23,5]*tower_temp + Tower_T_6hour_2021_regression[23,4])%>%
    mutate(C04W = Tower_T_6hour_2021_regression[24,5]*tower_temp + Tower_T_6hour_2021_regression[24,4])%>%
    mutate(D01E = Tower_T_6hour_2021_regression[25,5]*tower_temp + Tower_T_6hour_2021_regression[25,4])%>%
    mutate(D01W = Tower_T_6hour_2021_regression[26,5]*tower_temp + Tower_T_6hour_2021_regression[26,4])%>%
    mutate(D02E = Tower_T_6hour_2021_regression[27,5]*tower_temp + Tower_T_6hour_2021_regression[27,4])%>%
    mutate(D02W = Tower_T_6hour_2021_regression[28,5]*tower_temp + Tower_T_6hour_2021_regression[28,4])%>%
    mutate(D03E = Tower_T_6hour_2021_regression[29,5]*tower_temp + Tower_T_6hour_2021_regression[29,4])%>%
    mutate(D03W = Tower_T_6hour_2021_regression[30,5]*tower_temp + Tower_T_6hour_2021_regression[30,4])%>%
    mutate(D04E = Tower_T_6hour_2021_regression[31,5]*tower_temp + Tower_T_6hour_2021_regression[31,4])%>%
    mutate(D04W = Tower_T_6hour_2021_regression[32,5]*tower_temp + Tower_T_6hour_2021_regression[32,4])
  
  
  ##high
  Tower_T_6hour_2021_modeled_high <- Tower_T_6hour_2021%>%
    mutate(A01E = Tower_T_6hour_2021_regression[1,7]*tower_temp + Tower_T_6hour_2021_regression[1,6])%>%
    mutate(A01W = Tower_T_6hour_2021_regression[2,7]*tower_temp + Tower_T_6hour_2021_regression[2,6])%>%
    mutate(A02E = Tower_T_6hour_2021_regression[3,7]*tower_temp + Tower_T_6hour_2021_regression[3,6])%>%
    mutate(A02W = Tower_T_6hour_2021_regression[4,7]*tower_temp + Tower_T_6hour_2021_regression[4,6])%>%
    mutate(A03E = Tower_T_6hour_2021_regression[5,7]*tower_temp + Tower_T_6hour_2021_regression[5,6])%>%
    mutate(A03W = Tower_T_6hour_2021_regression[6,7]*tower_temp + Tower_T_6hour_2021_regression[6,6])%>%
    mutate(A04E = Tower_T_6hour_2021_regression[7,7]*tower_temp + Tower_T_6hour_2021_regression[7,6])%>%
    mutate(A04W = Tower_T_6hour_2021_regression[8,7]*tower_temp + Tower_T_6hour_2021_regression[8,6])%>%  
    mutate(B01E = Tower_T_6hour_2021_regression[9,7]*tower_temp + Tower_T_6hour_2021_regression[9,6])%>%
    mutate(B01W = Tower_T_6hour_2021_regression[10,7]*tower_temp + Tower_T_6hour_2021_regression[10,6])%>%
    mutate(B02E = Tower_T_6hour_2021_regression[11,7]*tower_temp + Tower_T_6hour_2021_regression[11,6])%>%
    mutate(B02W = Tower_T_6hour_2021_regression[12,7]*tower_temp + Tower_T_6hour_2021_regression[12,6])%>%
    mutate(B03E = Tower_T_6hour_2021_regression[13,7]*tower_temp + Tower_T_6hour_2021_regression[13,6])%>%
    mutate(B03W = Tower_T_6hour_2021_regression[14,7]*tower_temp + Tower_T_6hour_2021_regression[14,6])%>%
    mutate(B04E = Tower_T_6hour_2021_regression[15,7]*tower_temp + Tower_T_6hour_2021_regression[15,6])%>%
    mutate(B04W = Tower_T_6hour_2021_regression[16,7]*tower_temp + Tower_T_6hour_2021_regression[16,6])%>%
    mutate(C01E = Tower_T_6hour_2021_regression[17,7]*tower_temp + Tower_T_6hour_2021_regression[17,6])%>%
    mutate(C01W = Tower_T_6hour_2021_regression[18,7]*tower_temp + Tower_T_6hour_2021_regression[18,6])%>%
    mutate(C02E = Tower_T_6hour_2021_regression[19,7]*tower_temp + Tower_T_6hour_2021_regression[19,6])%>%
    mutate(C02W = Tower_T_6hour_2021_regression[20,7]*tower_temp + Tower_T_6hour_2021_regression[20,6])%>%
    mutate(C03E = Tower_T_6hour_2021_regression[21,7]*tower_temp + Tower_T_6hour_2021_regression[21,6])%>%
    mutate(C03W = Tower_T_6hour_2021_regression[22,7]*tower_temp + Tower_T_6hour_2021_regression[22,6])%>%
    mutate(C04E = Tower_T_6hour_2021_regression[23,7]*tower_temp + Tower_T_6hour_2021_regression[23,6])%>%
    mutate(C04W = Tower_T_6hour_2021_regression[24,7]*tower_temp + Tower_T_6hour_2021_regression[24,6])%>%
    mutate(D01E = Tower_T_6hour_2021_regression[25,7]*tower_temp + Tower_T_6hour_2021_regression[25,6])%>%
    mutate(D01W = Tower_T_6hour_2021_regression[26,7]*tower_temp + Tower_T_6hour_2021_regression[26,6])%>%
    mutate(D02E = Tower_T_6hour_2021_regression[27,7]*tower_temp + Tower_T_6hour_2021_regression[27,6])%>%
    mutate(D02W = Tower_T_6hour_2021_regression[28,7]*tower_temp + Tower_T_6hour_2021_regression[28,6])%>%
    mutate(D03E = Tower_T_6hour_2021_regression[29,7]*tower_temp + Tower_T_6hour_2021_regression[29,6])%>%
    mutate(D03W = Tower_T_6hour_2021_regression[30,7]*tower_temp + Tower_T_6hour_2021_regression[30,6])%>%
    mutate(D04E = Tower_T_6hour_2021_regression[31,7]*tower_temp + Tower_T_6hour_2021_regression[31,6])%>%
    mutate(D04W = Tower_T_6hour_2021_regression[32,7]*tower_temp + Tower_T_6hour_2021_regression[32,6])
  
  Tower_T_6hour_2021_modeled <- gather(Tower_T_6hour_2021_modeled, Subplot_ID, modeled_temp, A01E:D04W, factor_key = TRUE)
  
  Tower_T_6hour_2021_modeled_low <- gather(Tower_T_6hour_2021_modeled_low, Subplot_ID, modeled_temp_low, A01E:D04W, factor_key = TRUE)
  
  Tower_T_6hour_2021_modeled_high <- gather(Tower_T_6hour_2021_modeled_high, Subplot_ID, modeled_temp_high, A01E:D04W, factor_key = TRUE)
  
  Tower_T_6hour_2021_modeled <- merge(Tower_T_6hour_2021_modeled,Tower_T_6hour_2021_modeled_low, by = c("Timestamp_6", "tower_temp", "Subplot_ID") )
  
  Tower_T_6hour_2021_modeled <- merge(Tower_T_6hour_2021_modeled,Tower_T_6hour_2021_modeled_high, by = c("Timestamp_6", "tower_temp", "Subplot_ID") )
  
  write.csv(Tower_T_6hour_2021_modeled, "modeled_6hr_Ts_2021.csv", row.names=FALSE)
  
 
##plot modeled temperature data
  
##Create a dataframe with average modeled temp per day 
  
  ##Average across day 
  Tower_T_day_2021_modeled_6 <- Tower_T_6hour_2021_modeled%>%
    group_by(Subplot_ID, Timestamp_6 = floor_date(Timestamp_6, "1 day"))%>%
    summarize(tower_temp_day = mean(tower_temp), modeled_temp_day = mean(modeled_temp))
  
##Figure of modeled temperature average per day for all subplots and include the tower data as well. 
  ggplot(Tower_T_day_2021_modeled_6, aes(x = Timestamp_6)) +
    geom_line(aes(y = modeled_temp_day,  group = Subplot_ID, color = Subplot_ID), linewidth = 1) +
    geom_point(aes(y = tower_temp_day), color = "black", size = 3) +
    theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
    guides(color=guide_legend(ncol =1))
  ggsave(path = "Figures_Output", filename = "Modeled_T_2021.png", height = 20, width =30, units = "in")
  
  
 ######################## Model SWC from tower measurements (same as temperature pipeline) #################################
  
  
  ##Add tower data to match the point measurements 
  
  ##Rename the tower variables and select only the 2021 Timestamps 
 
  Tower_SWC_6hour_2021 <- Tower_SWC_6hour%>%
    filter(between(Timestamp_6, as.POSIXct('2021-01-01 04:00:00'), as.POSIXct('2021-12-31 23:00:00')))
  
  ###Join Tower and point data for 2021 
  
  Tower_SWC_6hour_2021_combined <- Tower_SWC_6hour_2021 %>% 
    left_join(point_2021_mean_VWC_6, by = "Timestamp_6")
  
  ###Run regression analyses for the relationship between tower and point measurements (SWC)
  
  Tower_SWC_6hour_2021_regression <- Tower_SWC_6hour_2021_combined%>%
    filter(!is.na(ave_VWC))
  
  ###Run Regression to look at summary statistics (All models are highly significant)
  fitted_model_SWC_6_2021 <-  Tower_SWC_6hour_2021_regression%>%
    nest_by(Subplot_ID)%>%
    mutate(model = list(lm(ave_VWC ~ tower_SWC, data = data)))%>%
    summarize(tidy(model))
  
  ###Organize model coefficients into a dataframe 
  
  Tower_SWC_6hour_2021_regression <- data.table(Tower_SWC_6hour_2021_regression)
  
  Tower_SWC_6hour_2021_regression <- Tower_SWC_6hour_2021_regression[,as.list(coef(lm(ave_VWC ~ tower_SWC))), by=Subplot_ID]
  
  Tower_SWC_6hour_2021_regression  <- as.data.frame.matrix(Tower_SWC_6hour_2021_regression)
  
  ###Apply subplot specific regression models to model hourly SWC in each subplot for 2021 (SWC(modeled) = m(tower_SWC) + b): LINEAR MODEL!!!!!!
  
  Tower_SWC_6hour_2021_modeled <- Tower_SWC_6hour_2021%>%
    mutate(D01E = Tower_SWC_6hour_2021_regression[1,3]*tower_SWC +Tower_SWC_6hour_2021_regression[1,2])%>%
    mutate(D01W = Tower_SWC_6hour_2021_regression[2,3]*tower_SWC + Tower_SWC_6hour_2021_regression[2,2])%>%
    mutate(D02E = Tower_SWC_6hour_2021_regression[3,3]*tower_SWC + Tower_SWC_6hour_2021_regression[3,2])%>%
    mutate(D02W = Tower_SWC_6hour_2021_regression[4,3]*tower_SWC + Tower_SWC_6hour_2021_regression[4,2])%>%
    mutate(D03E = Tower_SWC_6hour_2021_regression[5,3]*tower_SWC + Tower_SWC_6hour_2021_regression[5,2])%>%
    mutate(D03W = Tower_SWC_6hour_2021_regression[6,3]*tower_SWC + Tower_SWC_6hour_2021_regression[6,2])%>%
    mutate(D04E = Tower_SWC_6hour_2021_regression[7,3]*tower_SWC + Tower_SWC_6hour_2021_regression[7,2])%>%
    mutate(D04W = Tower_SWC_6hour_2021_regression[8,3]*tower_SWC + Tower_SWC_6hour_2021_regression[8,2])%>%
    mutate(C01E = Tower_SWC_6hour_2021_regression[9,3]*tower_SWC + Tower_SWC_6hour_2021_regression[9,2])%>%
    mutate(C01W = Tower_SWC_6hour_2021_regression[10,3]*tower_SWC + Tower_SWC_6hour_2021_regression[10,2])%>%
    mutate(C02E = Tower_SWC_6hour_2021_regression[11,3]*tower_SWC + Tower_SWC_6hour_2021_regression[11,2])%>%
    mutate(C02W = Tower_SWC_6hour_2021_regression[12,3]*tower_SWC + Tower_SWC_6hour_2021_regression[12,2])%>%
    mutate(C03E = Tower_SWC_6hour_2021_regression[13,3]*tower_SWC + Tower_SWC_6hour_2021_regression[13,2])%>%
    mutate(C03W = Tower_SWC_6hour_2021_regression[14,3]*tower_SWC + Tower_SWC_6hour_2021_regression[14,2])%>%
    mutate(C04E = Tower_SWC_6hour_2021_regression[15,3]*tower_SWC + Tower_SWC_6hour_2021_regression[15,2])%>%
    mutate(C04W = Tower_SWC_6hour_2021_regression[16,3]*tower_SWC + Tower_SWC_6hour_2021_regression[16,2])%>%
    mutate(B01E = Tower_SWC_6hour_2021_regression[17,3]*tower_SWC + Tower_SWC_6hour_2021_regression[17,2])%>%
    mutate(B01W = Tower_SWC_6hour_2021_regression[18,3]*tower_SWC + Tower_SWC_6hour_2021_regression[18,2])%>%
    mutate(B02E = Tower_SWC_6hour_2021_regression[19,3]*tower_SWC + Tower_SWC_6hour_2021_regression[19,2])%>%
    mutate(B02W = Tower_SWC_6hour_2021_regression[20,3]*tower_SWC + Tower_SWC_6hour_2021_regression[20,2])%>%
    mutate(B03E = Tower_SWC_6hour_2021_regression[21,3]*tower_SWC + Tower_SWC_6hour_2021_regression[21,2])%>%
    mutate(B03W = Tower_SWC_6hour_2021_regression[22,3]*tower_SWC + Tower_SWC_6hour_2021_regression[22,2])%>%
    mutate(B04E = Tower_SWC_6hour_2021_regression[23,3]*tower_SWC + Tower_SWC_6hour_2021_regression[23,2])%>%
    mutate(B04W = Tower_SWC_6hour_2021_regression[24,3]*tower_SWC + Tower_SWC_6hour_2021_regression[24,2])%>%
    mutate(A01E = Tower_SWC_6hour_2021_regression[25,3]*tower_SWC + Tower_SWC_6hour_2021_regression[25,2])%>%
    mutate(A01W = Tower_SWC_6hour_2021_regression[26,3]*tower_SWC + Tower_SWC_6hour_2021_regression[26,2])%>%
    mutate(A02E = Tower_SWC_6hour_2021_regression[27,3]*tower_SWC + Tower_SWC_6hour_2021_regression[27,2])%>%
    mutate(A02W = Tower_SWC_6hour_2021_regression[28,3]*tower_SWC + Tower_SWC_6hour_2021_regression[28,2])%>%
    mutate(A03E = Tower_SWC_6hour_2021_regression[29,3]*tower_SWC + Tower_SWC_6hour_2021_regression[29,2])%>%
    mutate(A03W = Tower_SWC_6hour_2021_regression[30,3]*tower_SWC + Tower_SWC_6hour_2021_regression[30,2])%>%
    mutate(A04E = Tower_SWC_6hour_2021_regression[31,3]*tower_SWC + Tower_SWC_6hour_2021_regression[31,2])%>%
    mutate(A04W = Tower_SWC_6hour_2021_regression[32,3]*tower_SWC + Tower_SWC_6hour_2021_regression[32,2])
  
  
  Tower_SWC_6hour_2021_modeled <- gather(Tower_SWC_6hour_2021_modeled, Subplot_ID, modeled_SWC, D01E:A04W, factor_key = TRUE)
  
  ##plot modeled SWC data
  
  ##Create a dataframe with average modeled SWC per day 
  
  ##Average across day 
  Tower_SWC_day_2021_modeled_6 <- Tower_SWC_6hour_2021_modeled%>%
    group_by(Subplot_ID, Timestamp_6 = floor_date(Timestamp_6, "1 day"))%>%
    summarize(tower_SWC_day = mean(tower_SWC), modeled_SWC_day = mean(modeled_SWC))
  
  ##Figure of modeled SWC average per day for all subplots and include the tower data as well. 
  ggplot(Tower_SWC_day_2021_modeled_6, aes(x = Timestamp_6)) +
    geom_line(aes(y = modeled_SWC_day,  group = Subplot_ID, color = Subplot_ID), linewidth = 1) +
    geom_point(aes(y = tower_SWC_day), color = "black", size = 3) +
    theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
    guides(color=guide_legend(ncol =1))
  
  ggsave(path = "Figures_Output", filename = "Modeled_VWC_2021.png", height = 20, width =30, units = "in")
  
  
  
  ################################################## 2022 Data ############################################################################
  
  ##Read in Rs point measurement files 
  raw_2022 <- read.csv("googledrive_data/Rs_2022.csv", na.strings = c("NA","na", ""))
  
  ###Combine plot level info into one subplot column
  raw_2022$Subplot_ID <- str_c(raw_2022$Rep_ID, '',raw_2022$Plot_ID,'', raw_2022$Subplot)
  
  ##Keep the columns that we need 
  point_2022 <- raw_2022%>%
    select(Subplot_ID, soilTemp, VWC, date, time)  
  ##Convert date and time columns to datetime and combine date and time column into standard format for tower data  
  
  point_2022$date <- as.POSIXct(point_2022$date)
  
  
  point_2022$time <- as.POSIXct(point_2022$time, tz = "", format = "%H:%M:%S")
  
  point_2022$time <- format(as.POSIXct(point_2022$time), format = "%H")
  
  point_2022 <- point_2022%>%
    mutate(Timestamp_6 = as.POSIXct(paste(date, time), format="%Y-%m-%d %H"))
  
  point_2022 <-   point_2022%>%
    mutate(Timestamp_6 = as.POSIXct(paste(date, time), format="%Y-%m-%d %H"))
  
  point_2022$year <- format(as.POSIXct(point_2022$Timestamp), format = "%Y")
  
  point_2022$Timestamp_6 <- as.POSIXct(format(point_2022$Timestamp_6), tz = "")
  point_2022$Timestamp <- as.POSIXct(format(point_2022$Timestamp), tz = "")
  
  ### Average temperature and moisture over the subplot per timestamp 
  
  point_2022_mean_temp_6 <- point_2022%>%
    group_by(date, Subplot_ID, Timestamp_6)%>%
    filter(!is.na(soilTemp))%>%
    summarize(ave_soilTemp = mean(soilTemp))
  
  
  
  point_2022_mean_VWC_6 <- point_2022%>%
    group_by(date, Subplot_ID, Timestamp_6)%>%
    filter(!is.na(VWC))%>%
    summarize(ave_VWC = mean(VWC))
  
####Bring in 2022 tower data and wrangle dates urg
  
tower_raw_2022 <- read.csv("Data_Tower/2022_US_UMB.csv",na.strings = c("NA","na", ""))

library(stringr)
  
tower_raw_2022$hour <-  str_pad(tower_raw_2022$hour, 2, pad = "0")
tower_raw_2022$minute <-  str_pad(tower_raw_2022$minute, 2, pad = "0")

tower_raw_2022 <- tower_raw_2022%>%
  mutate(year = "2022")

tower_raw_2022$Day.of.Year <- as.character(tower_raw_2022$Day.of.Year)

tower_raw_2022$Day.of.Year <- as.Date(tower_raw_2022$Day.of.Year, format = "%j", origin = '2022-01-01')

tower_raw_2022$Day.of.Year <- format(tower_raw_2022$Day.of.Year, "%m-%d")
 tower_raw_2022$Day.of.Year <- as.POSIXct(tower_raw_2022$Day.of.Year, format = "%m-%d")
 
 tower_raw_2022$Day.of.Year <-  tower_raw_2022$Day.of.Year - lubridate::years(1)
 
 tower_raw_2022$hour <- as.POSIXct(tower_raw_2022$hour, tz = "", format = "%H")
 tower_raw_2022$hour <- format(as.POSIXct( tower_raw_2022$hour), format = "%H")

 tower_raw_2022$minute <- as.POSIXct(tower_raw_2022$minute, tz = "", format = "%M")
 tower_raw_2022$minute <- format(as.POSIXct( tower_raw_2022$minute), format = "%M")
  
 tower_raw_2022 <-  tower_raw_2022%>%
   mutate(Timestamp = as.POSIXct(paste(Day.of.Year, hour), format="%Y-%m-%d %H"))
 
 
 #####Create 6 hour Temperature and Moisture Dataframes 
 
 tower_raw_2022_T <- tower_raw_2022%>%
   select(Timestamp,Station.1.7.5.cm.Soil.Temp...C.)%>%
   filter(!is.na(Station.1.7.5.cm.Soil.Temp...C.))
 
 tower_T_6hour_2022 <- tower_raw_2022_T%>%
   group_by(Timestamp_6 = floor_date(Timestamp, "6 hours"))%>%
   summarize(tower_temp = mean(Station.1.7.5.cm.Soil.Temp...C.))
 
 tower_raw_2022_SWC <- tower_raw_2022%>%
   select(Timestamp,X0...30.cm.Soil.Moisture_1....vol.vol.)%>%
   filter(!is.na(X0...30.cm.Soil.Moisture_1....vol.vol.))
 
 tower_SWC_6hour_2022 <- tower_raw_2022_SWC%>%
   group_by(Timestamp_6 = floor_date(Timestamp, "6 hours"))%>%
   summarize(tower_SWC = mean(X0...30.cm.Soil.Moisture_1....vol.vol.))
  
  
  ###Join Tower and point data for 2022 
  
  Tower_T_6hour_2022_combined <- tower_T_6hour_2022%>% 
    left_join(point_2022_mean_temp_6, by = "Timestamp_6")
  
  
  ###Run regression analyses for the relationship between tower and point measurements
  Tower_T_6hour_2022_regression <- Tower_T_6hour_2022_combined%>%
    filter(!is.na(ave_soilTemp))
  
  ###Run Regression to look at summary statistics (All models are highly significant)
  library(broom)
  
  fitted_model_temp_6_2022 <-  Tower_T_6hour_2022_regression%>%
    nest_by(Subplot_ID)%>%
    mutate(model = list(lm(ave_soilTemp ~ tower_temp, data = data)))%>%
    summarize(tidy(model))
  
  ###Organize model coefficients into a dataframe 
  library(data.table)
  
  Tower_T_6hour_2022_regression <- data.table(Tower_T_6hour_2022_regression)
  
  Tower_T_6hour_2022_regression <- Tower_T_6hour_2022_regression[,as.list(coef(lm(ave_soilTemp ~ tower_temp))), by=Subplot_ID]
  
  Tower_T_6hour_2022_regression <- as.data.frame.matrix(Tower_T_6hour_2022_regression)
  

    
  
  
  ###Apply subplot specific regression models to model hourly temperature in each subplot for 2022 (temp(modeled) = m(tower_temp) + b)
  Tower_T_6hour_2022_modeled <- tower_T_6hour_2022%>%
    mutate(C01E = Tower_T_6hour_2022_regression[1,3]*tower_temp + Tower_T_6hour_2022_regression[1,2])%>%
    mutate(C01W = Tower_T_6hour_2022_regression[2,3]*tower_temp + Tower_T_6hour_2022_regression[2,2])%>%
    mutate(C02E = Tower_T_6hour_2022_regression[3,3]*tower_temp + Tower_T_6hour_2022_regression[3,2])%>%
    mutate(C02W = Tower_T_6hour_2022_regression[4,3]*tower_temp + Tower_T_6hour_2022_regression[4,2])%>%
    mutate(C03E = Tower_T_6hour_2022_regression[5,3]*tower_temp + Tower_T_6hour_2022_regression[5,2])%>%
    mutate(C03W = Tower_T_6hour_2022_regression[6,3]*tower_temp + Tower_T_6hour_2022_regression[6,2])%>%
    mutate(C04E = Tower_T_6hour_2022_regression[7,3]*tower_temp + Tower_T_6hour_2022_regression[7,2])%>%
    mutate(C04W = Tower_T_6hour_2022_regression[8,3]*tower_temp + Tower_T_6hour_2022_regression[8,2])%>%
    mutate(D01E = Tower_T_6hour_2022_regression[9,3]*tower_temp + Tower_T_6hour_2022_regression[9,2])%>%
    mutate(D01W = Tower_T_6hour_2022_regression[10,3]*tower_temp + Tower_T_6hour_2022_regression[10,2])%>%
    mutate(D02E = Tower_T_6hour_2022_regression[11,3]*tower_temp + Tower_T_6hour_2022_regression[11,2])%>%
    mutate(D02W = Tower_T_6hour_2022_regression[12,3]*tower_temp + Tower_T_6hour_2022_regression[12,2])%>%
    mutate(D03E = Tower_T_6hour_2022_regression[13,3]*tower_temp + Tower_T_6hour_2022_regression[13,2])%>%
    mutate(D03W = Tower_T_6hour_2022_regression[14,3]*tower_temp + Tower_T_6hour_2022_regression[14,2])%>%
    mutate(D04E = Tower_T_6hour_2022_regression[15,3]*tower_temp + Tower_T_6hour_2022_regression[15,2])%>%
    mutate(D04W = Tower_T_6hour_2022_regression[16,3]*tower_temp + Tower_T_6hour_2022_regression[16,2])%>%
    mutate(A01E = Tower_T_6hour_2022_regression[17,3]*tower_temp + Tower_T_6hour_2022_regression[17,2])%>%
    mutate(A01W = Tower_T_6hour_2022_regression[18,3]*tower_temp + Tower_T_6hour_2022_regression[18,2])%>%
    mutate(A02E = Tower_T_6hour_2022_regression[19,3]*tower_temp + Tower_T_6hour_2022_regression[19,2])%>%
    mutate(A02W = Tower_T_6hour_2022_regression[20,3]*tower_temp + Tower_T_6hour_2022_regression[20,2])%>%
    mutate(A03E = Tower_T_6hour_2022_regression[21,3]*tower_temp + Tower_T_6hour_2022_regression[21,2])%>%
    mutate(A03W = Tower_T_6hour_2022_regression[22,3]*tower_temp + Tower_T_6hour_2022_regression[22,2])%>%
    mutate(A04E = Tower_T_6hour_2022_regression[23,3]*tower_temp + Tower_T_6hour_2022_regression[23,2])%>%
    mutate(A04W = Tower_T_6hour_2022_regression[24,3]*tower_temp + Tower_T_6hour_2022_regression[24,2])%>%
    mutate(B01E = Tower_T_6hour_2022_regression[25,3]*tower_temp + Tower_T_6hour_2022_regression[25,2])%>%
    mutate(B01W = Tower_T_6hour_2022_regression[26,3]*tower_temp + Tower_T_6hour_2022_regression[26,2])%>%
    mutate(B02E = Tower_T_6hour_2022_regression[27,3]*tower_temp + Tower_T_6hour_2022_regression[27,2])%>%
    mutate(B02W = Tower_T_6hour_2022_regression[28,3]*tower_temp + Tower_T_6hour_2022_regression[28,2])%>%
    mutate(B03E = Tower_T_6hour_2022_regression[29,3]*tower_temp + Tower_T_6hour_2022_regression[29,2])%>%
    mutate(B03W = Tower_T_6hour_2022_regression[30,3]*tower_temp + Tower_T_6hour_2022_regression[30,2])%>%
    mutate(B04E = Tower_T_6hour_2022_regression[31,3]*tower_temp + Tower_T_6hour_2022_regression[31,2])%>%
    mutate(B04W = Tower_T_6hour_2022_regression[32,3]*tower_temp + Tower_T_6hour_2022_regression[32,2])
  
  
  Tower_T_6hour_2022_modeled <- gather(Tower_T_6hour_2022_modeled, Subplot_ID, modeled_temp, C01E:B04W, factor_key = TRUE)
  
  write.csv(Tower_T_6hour_2022_modeled, "modeled_6hr_Ts_2022.csv", row.names=FALSE)
  
  ##plot modeled temperature data
  
  ##Create a dataframe with average modeled temp per day 
  
  ##Average across day 
  Tower_T_day_2022_modeled_6 <- Tower_T_6hour_2022_modeled%>%
    group_by(Subplot_ID, Timestamp_6 = floor_date(Timestamp_6, "1 day"))%>%
    summarize(tower_temp_day = mean(tower_temp), modeled_temp_day = mean(modeled_temp))
  
  ##Figure of modeled temperature average per day for all subplots and include the tower data as well. 
  ggplot(Tower_T_day_2022_modeled_6, aes(x = Timestamp_6)) +
    geom_line(aes(y = modeled_temp_day,  group = Subplot_ID, color = Subplot_ID), linewidth = 1) +
    geom_point(aes(y = tower_temp_day), color = "black", size = 3) +
    theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
    guides(color=guide_legend(ncol =1))
  ggsave(path = "Figures_Output", filename = "Modeled_T_2022.png", height = 20, width =30, units = "in")
  
  
  ######################## Model SWC from tower measurements (same as temperature pipeline) #################################
  
  
  
  # ##Add tower data to match the point measurements 
  # 
  # ##Rename the tower variables and select only the 2022 Timestamps 
  # 
  # Tower_SWC_6hour_2022 <- Tower_SWC_6hour%>%
  #   filter(between(Timestamp_6, as.POSIXct('2022-01-01 04:00:00'), as.POSIXct('2022-12-31 23:00:00')))
  # 
  # ###Join Tower and point data for 2022 
  # 
  # Tower_SWC_6hour_2022_combined <- Tower_SWC_6hour_2022 %>% 
  #   left_join(point_2022_mean_VWC_6, by = "Timestamp_6")
  # 
  # ###Run regression analyses for the relationship between tower and point measurements (SWC)
  # 
  # Tower_SWC_6hour_2022_regression <- Tower_SWC_6hour_2022_combined%>%
  #   filter(!is.na(ave_VWC))
  # 
  # ###Run Regression to look at summary statistics (All models are highly significant)
  # fitted_model_SWC_6_2022 <-  Tower_SWC_6hour_2022_regression%>%
  #   nest_by(Subplot_ID)%>%
  #   mutate(model = list(lm(ave_VWC ~ tower_SWC, data = data)))%>%
  #   summarize(tidy(model))
  # 
  # ###Organize model coefficients into a dataframe 
  # 
  # Tower_SWC_6hour_2022_regression <- data.table(Tower_SWC_6hour_2022_regression)
  # 
  # Tower_SWC_6hour_2022_regression <- Tower_SWC_6hour_2022_regression[,as.list(coef(lm(ave_VWC ~ tower_SWC))), by=Subplot_ID]
  # 
  # Tower_SWC_6hour_2022_regression  <- as.data.frame.matrix(Tower_SWC_6hour_2022_regression)
  # 
  # ###Apply subplot specific regression models to model hourly SWC in each subplot for 2022 (SWC(modeled) = m(tower_SWC) + b): LINEAR MODEL!!!!!!
  # 
  # Tower_SWC_6hour_2022_modeled <- Tower_SWC_6hour_2022%>%
  #   mutate(D1e_modeled = Tower_SWC_6hour_2022_regression[1,3]*tower_SWC +Tower_SWC_6hour_2022_regression[1,2])%>%
  #   mutate(D1w_modeled = Tower_SWC_6hour_2022_regression[2,3]*tower_SWC + Tower_SWC_6hour_2022_regression[2,2])%>%
  #   mutate(D2e_modeled = Tower_SWC_6hour_2022_regression[3,3]*tower_SWC + Tower_SWC_6hour_2022_regression[3,2])%>%
  #   mutate(D2w_modeled = Tower_SWC_6hour_2022_regression[4,3]*tower_SWC + Tower_SWC_6hour_2022_regression[4,2])%>%
  #   mutate(D3e_modeled = Tower_SWC_6hour_2022_regression[5,3]*tower_SWC + Tower_SWC_6hour_2022_regression[5,2])%>%
  #   mutate(D3w_modeled = Tower_SWC_6hour_2022_regression[6,3]*tower_SWC + Tower_SWC_6hour_2022_regression[6,2])%>%
  #   mutate(D4e_modeled = Tower_SWC_6hour_2022_regression[7,3]*tower_SWC + Tower_SWC_6hour_2022_regression[7,2])%>%
  #   mutate(D4w_modeled = Tower_SWC_6hour_2022_regression[8,3]*tower_SWC + Tower_SWC_6hour_2022_regression[8,2])%>%
  #   mutate(C1e_modeled = Tower_SWC_6hour_2022_regression[9,3]*tower_SWC + Tower_SWC_6hour_2022_regression[9,2])%>%
  #   mutate(C1w_modeled = Tower_SWC_6hour_2022_regression[10,3]*tower_SWC + Tower_SWC_6hour_2022_regression[10,2])%>%
  #   mutate(C2e_modeled = Tower_SWC_6hour_2022_regression[11,3]*tower_SWC + Tower_SWC_6hour_2022_regression[11,2])%>%
  #   mutate(C2w_modeled = Tower_SWC_6hour_2022_regression[12,3]*tower_SWC + Tower_SWC_6hour_2022_regression[12,2])%>%
  #   mutate(C3e_modeled = Tower_SWC_6hour_2022_regression[13,3]*tower_SWC + Tower_SWC_6hour_2022_regression[13,2])%>%
  #   mutate(C3w_modeled = Tower_SWC_6hour_2022_regression[14,3]*tower_SWC + Tower_SWC_6hour_2022_regression[14,2])%>%
  #   mutate(C4e_modeled = Tower_SWC_6hour_2022_regression[15,3]*tower_SWC + Tower_SWC_6hour_2022_regression[15,2])%>%
  #   mutate(C4w_modeled = Tower_SWC_6hour_2022_regression[16,3]*tower_SWC + Tower_SWC_6hour_2022_regression[16,2])%>%
  #   mutate(B1e_modeled = Tower_SWC_6hour_2022_regression[17,3]*tower_SWC + Tower_SWC_6hour_2022_regression[17,2])%>%
  #   mutate(B1w_modeled = Tower_SWC_6hour_2022_regression[18,3]*tower_SWC + Tower_SWC_6hour_2022_regression[18,2])%>%
  #   mutate(B2e_modeled = Tower_SWC_6hour_2022_regression[19,3]*tower_SWC + Tower_SWC_6hour_2022_regression[19,2])%>%
  #   mutate(B2w_modeled = Tower_SWC_6hour_2022_regression[20,3]*tower_SWC + Tower_SWC_6hour_2022_regression[20,2])%>%
  #   mutate(B3e_modeled = Tower_SWC_6hour_2022_regression[21,3]*tower_SWC + Tower_SWC_6hour_2022_regression[21,2])%>%
  #   mutate(B3w_modeled = Tower_SWC_6hour_2022_regression[22,3]*tower_SWC + Tower_SWC_6hour_2022_regression[22,2])%>%
  #   mutate(B4e_modeled = Tower_SWC_6hour_2022_regression[23,3]*tower_SWC + Tower_SWC_6hour_2022_regression[23,2])%>%
  #   mutate(B4w_modeled = Tower_SWC_6hour_2022_regression[24,3]*tower_SWC + Tower_SWC_6hour_2022_regression[24,2])%>%
  #   mutate(A1e_modeled = Tower_SWC_6hour_2022_regression[25,3]*tower_SWC + Tower_SWC_6hour_2022_regression[25,2])%>%
  #   mutate(A1w_modeled = Tower_SWC_6hour_2022_regression[26,3]*tower_SWC + Tower_SWC_6hour_2022_regression[26,2])%>%
  #   mutate(A2e_modeled = Tower_SWC_6hour_2022_regression[27,3]*tower_SWC + Tower_SWC_6hour_2022_regression[27,2])%>%
  #   mutate(A2w_modeled = Tower_SWC_6hour_2022_regression[28,3]*tower_SWC + Tower_SWC_6hour_2022_regression[28,2])%>%
  #   mutate(A3e_modeled = Tower_SWC_6hour_2022_regression[29,3]*tower_SWC + Tower_SWC_6hour_2022_regression[29,2])%>%
  #   mutate(A3w_modeled = Tower_SWC_6hour_2022_regression[30,3]*tower_SWC + Tower_SWC_6hour_2022_regression[30,2])%>%
  #   mutate(A4e_modeled = Tower_SWC_6hour_2022_regression[31,3]*tower_SWC + Tower_SWC_6hour_2022_regression[31,2])%>%
  #   mutate(A4w_modeled = Tower_SWC_6hour_2022_regression[32,3]*tower_SWC + Tower_SWC_6hour_2022_regression[32,2])
  # 
  # 
  # 
  # Tower_SWC_6hour_2022_modeled <- gather(Tower_SWC_6hour_2022_modeled, Subplot_ID, modeled_SWC, D1e_modeled:A4w_modeled, factor_key = TRUE)
  # 
  # ##plot modeled SWC data
  # 
  # ##Create a dataframe with average modeled SWC per day 
  # 
  # ##Average across day 
  # Tower_SWC_day_2022_modeled_6 <- Tower_SWC_6hour_2022_modeled%>%
  #   group_by(Subplot_ID, Timestamp_6 = floor_date(Timestamp_6, "1 day"))%>%
  #   summarize(tower_SWC_day = mean(tower_SWC), modeled_SWC_day = mean(modeled_SWC))
  # 
  # ##Figure of modeled SWC average per day for all subplots and include the tower data as well. 
  # ggplot(Tower_SWC_day_2022_modeled_6, aes(x = Timestamp_6)) +
  #   geom_line(aes(y = modeled_SWC_day,  group = Subplot_ID, color = Subplot_ID), linewidth = 1) +
  #   geom_point(aes(y = tower_SWC_day), color = "black", size = 3) +
  #   theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  #   guides(color=guide_legend(ncol =1))
  # 
  # ggsave(path = "Figures_Output", filename = "Modeled_VWC_2022.png", height = 20, width =30, units = "in")
  
  
 ##################################################################### Estimate CWD Flux (Temperature + Moisture model method) ###################################################

########2021  
###Merge the modelled temperature and moisture dataframes 
  
#Modeled_T_SWC_2019 <- merge(Tower_SWC_6hour_2019_modeled,   Tower_T_6hour_2019_modeled, by = c("Timestamp_6", "Subplot_ID"))

##Assume the modeled SWC below 0 is = to 0   
#Modeled_T_SWC_2019 <- Modeled_T_SWC_2019%>%
  #mutate(modeled_SWC_adjusted = case_when(modeled_SWC < 0 ~ 0, 
                                          #modeled_SWC > 0 ~ modeled_SWC))

##Apply the regression for CWD -1.2773*exp(-0.0339*modeled_temp) + 0.2493*log(fixed GWC), if temp <0, R CWD = 0. R_CWD (umol C kg s ). fixed gravemetric water content from the Gough et al. 2007 based on decay classes 
##Decay class one: 50%
##Decay class two: 85%
##Decay class three: 72%
##Decay class four: 260%
##Decay class five: 200%

Modeled_R_T_SWC_2019 <- Tower_T_6hour_2019_modeled%>%
  mutate(R_CWD_umol_kg_s_1 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(50), 
                                     modeled_temp < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_2 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(85), 
                                       modeled_temp < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_3 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(72), 
                                       modeled_temp < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_4 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(260), 
                                       modeled_temp < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_5 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(200), 
                                       modeled_temp < 0 ~ 0))

Modeled_R_T_SWC_2019_low <- Tower_T_6hour_2019_modeled%>%
mutate(R_CWD_umol_kg_s_1_low = case_when(modeled_temp_low > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_low)) + 0.2493*log(50), 
                                     modeled_temp_low < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_2_low = case_when(modeled_temp_low > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_low)) + 0.2493*log(85), 
                                           modeled_temp_low < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_3_low = case_when(modeled_temp_low > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_low)) + 0.2493*log(72), 
                                           modeled_temp_low < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_4_low = case_when(modeled_temp_low > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_low)) + 0.2493*log(260), 
                                           modeled_temp_low < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_5_low = case_when(modeled_temp_low > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_low)) + 0.2493*log(200), 
                                           modeled_temp_low < 0 ~ 0))


Modeled_R_T_SWC_2019_high <- Tower_T_6hour_2019_modeled%>%
  mutate(R_CWD_umol_kg_s_1_high = case_when(modeled_temp_high > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_high)) + 0.2493*log(50), 
                                           modeled_temp_high < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_2_high = case_when(modeled_temp_high > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_high)) + 0.2493*log(85), 
                                           modeled_temp_high < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_3_high = case_when(modeled_temp_high > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_high)) + 0.2493*log(72), 
                                           modeled_temp_high < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_4_high = case_when(modeled_temp_high > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_high)) + 0.2493*log(260), 
                                           modeled_temp_high < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_5_high = case_when(modeled_temp_high > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_high)) + 0.2493*log(200), 
                                           modeled_temp_high < 0 ~ 0))


# #####Convert negative values to zero 
# Modeled_R_T_SWC_2019 <- Modeled_R_T_SWC_2019%>%
#   mutate(R_CWD_umol_kg_s_1_adj = case_when(R_CWD_umol_kg_s_1 >= 0 ~ Modeled_R_T_SWC_2019$R_CWD_umol_kg_s_1, 
#                                            R_CWD_umol_kg_s_1 < 0 ~ 0))%>%
#   mutate(R_CWD_umol_kg_s_2_adj = case_when(R_CWD_umol_kg_s_2 >= 0 ~ Modeled_R_T_SWC_2019$R_CWD_umol_kg_s_2, 
#                                            R_CWD_umol_kg_s_2 < 0 ~ 0))%>%
#   mutate(R_CWD_umol_kg_s_3_adj = case_when(R_CWD_umol_kg_s_3 >= 0 ~ Modeled_R_T_SWC_2019$R_CWD_umol_kg_s_3, 
#                                            R_CWD_umol_kg_s_3 < 0 ~ 0))%>%
#   mutate(R_CWD_umol_kg_s_4_adj = case_when(R_CWD_umol_kg_s_4 >= 0 ~ Modeled_R_T_SWC_2019$R_CWD_umol_kg_s_4, 
#                                            R_CWD_umol_kg_s_4 < 0 ~ 0))%>%
#   mutate(R_CWD_umol_kg_s_5_adj = case_when(R_CWD_umol_kg_s_5 >= 0 ~ Modeled_R_T_SWC_2019$R_CWD_umol_kg_s_5, 
#                                            R_CWD_umol_kg_s_5 < 0 ~ 0))
                                              
  
###Scale Rcwd from (umol C kg s) -> umol C kg 6hour

Modeled_6hour_R_T_SWC_2019 <- Modeled_R_T_SWC_2019%>%
  mutate(R_CWD_umol_kg_6hour_1 = R_CWD_umol_kg_s_1*21600)%>%
  mutate(R_CWD_umol_kg_6hour_2 = R_CWD_umol_kg_s_2*21600)%>%
  mutate(R_CWD_umol_kg_6hour_3 = R_CWD_umol_kg_s_3*21600)%>%
  mutate(R_CWD_umol_kg_6hour_4 = R_CWD_umol_kg_s_4*21600)%>%
  mutate(R_CWD_umol_kg_6hour_5 = R_CWD_umol_kg_s_5*21600)

###Low
Modeled_6hour_R_T_SWC_2019_low <- Modeled_R_T_SWC_2019_low%>%
  mutate(R_CWD_umol_kg_6hour_1_low = R_CWD_umol_kg_s_1_low*21600)%>%
  mutate(R_CWD_umol_kg_6hour_2_low = R_CWD_umol_kg_s_2_low*21600)%>%
  mutate(R_CWD_umol_kg_6hour_3_low = R_CWD_umol_kg_s_3_low*21600)%>%
  mutate(R_CWD_umol_kg_6hour_4_low = R_CWD_umol_kg_s_4_low*21600)%>%
  mutate(R_CWD_umol_kg_6hour_5_low = R_CWD_umol_kg_s_5_low*21600)

#high 
Modeled_6hour_R_T_SWC_2019_high <- Modeled_R_T_SWC_2019_high%>%
  mutate(R_CWD_umol_kg_6hour_1_high = R_CWD_umol_kg_s_1_high*21600)%>%
  mutate(R_CWD_umol_kg_6hour_2_high = R_CWD_umol_kg_s_2_high*21600)%>%
  mutate(R_CWD_umol_kg_6hour_3_high = R_CWD_umol_kg_s_3_high*21600)%>%
  mutate(R_CWD_umol_kg_6hour_4_high = R_CWD_umol_kg_s_4_high*21600)%>%
  mutate(R_CWD_umol_kg_6hour_5_high = R_CWD_umol_kg_s_5_high*21600)

###Scale from umol C kg 6hour -> umol C kg yr
Modeled_annual_R_T_SWC_2019 <- Modeled_6hour_R_T_SWC_2019%>%
  group_by(Subplot_ID)%>%
  summarize(R_CWD_umol_kg_year_1 = sum(R_CWD_umol_kg_6hour_1),R_CWD_umol_kg_year_2 = sum(R_CWD_umol_kg_6hour_2),R_CWD_umol_kg_year_3 = sum(R_CWD_umol_kg_6hour_3),R_CWD_umol_kg_year_4 = sum(R_CWD_umol_kg_6hour_4),R_CWD_umol_kg_year_5 = sum(R_CWD_umol_kg_6hour_5))

##Low 
Modeled_annual_R_T_SWC_2019_low <- Modeled_6hour_R_T_SWC_2019_low%>%
  group_by(Subplot_ID)%>%
  summarize(R_CWD_umol_kg_year_1_low = sum(R_CWD_umol_kg_6hour_1_low),R_CWD_umol_kg_year_2_low = sum(R_CWD_umol_kg_6hour_2_low),R_CWD_umol_kg_year_3_low = sum(R_CWD_umol_kg_6hour_3_low),R_CWD_umol_kg_year_4_low = sum(R_CWD_umol_kg_6hour_4_low),R_CWD_umol_kg_year_5_low = sum(R_CWD_umol_kg_6hour_5_low))

##High 
Modeled_annual_R_T_SWC_2019_high <- Modeled_6hour_R_T_SWC_2019_high%>%
  group_by(Subplot_ID)%>%
  summarize(R_CWD_umol_kg_year_1_high = sum(R_CWD_umol_kg_6hour_1_high),R_CWD_umol_kg_year_2_high = sum(R_CWD_umol_kg_6hour_2_high),R_CWD_umol_kg_year_3_high = sum(R_CWD_umol_kg_6hour_3_high),R_CWD_umol_kg_year_4_high = sum(R_CWD_umol_kg_6hour_4_high),R_CWD_umol_kg_year_5_high = sum(R_CWD_umol_kg_6hour_5_high))


##Make modeled Rcwd dataframe long again

Modeled_annual_R_T_SWC_2019 <- Modeled_annual_R_T_SWC_2019%>%
  rename("1" = R_CWD_umol_kg_year_1, "2" = R_CWD_umol_kg_year_2, "3" = R_CWD_umol_kg_year_3, "4" = R_CWD_umol_kg_year_4, "5" = R_CWD_umol_kg_year_5)

##low
Modeled_annual_R_T_SWC_2019_low <- Modeled_annual_R_T_SWC_2019_low%>%
  rename("1" = R_CWD_umol_kg_year_1_low, "2" = R_CWD_umol_kg_year_2_low, "3" = R_CWD_umol_kg_year_3_low, "4" = R_CWD_umol_kg_year_4_low, "5" = R_CWD_umol_kg_year_5_low)

##high
Modeled_annual_R_T_SWC_2019_high <- Modeled_annual_R_T_SWC_2019_high%>%
  rename("1" = R_CWD_umol_kg_year_1_high, "2" = R_CWD_umol_kg_year_2_high, "3" = R_CWD_umol_kg_year_3_high, "4" = R_CWD_umol_kg_year_4_high, "5" = R_CWD_umol_kg_year_5_high)

Modeled_annual_R_T_SWC_2019 <- gather(Modeled_annual_R_T_SWC_2019, decay_class, R_CWD_umol_kg_year, "1":"5", factor_key = TRUE)

Modeled_annual_R_T_SWC_2019_low <- gather(Modeled_annual_R_T_SWC_2019_low, decay_class, R_CWD_umol_kg_year_low, "1":"5", factor_key = TRUE)

Modeled_annual_R_T_SWC_2019_high <- gather(Modeled_annual_R_T_SWC_2019_high, decay_class, R_CWD_umol_kg_year_high, "1":"5", factor_key = TRUE)


###Bring in Mass of CWD in 2019 from CWD_2019_subplot_agg and convert to kg

CWD_2019_subplot_agg_sub <- CWD_2019_subplot_agg%>%
  select(subplot_id, decay_class, C_mass_Mg_subplot_total)%>%
  rename(Subplot_ID = subplot_id)%>%
  mutate(C_mass_kg_subplot_total = C_mass_Mg_subplot_total*1000)

###Merge the CWD mass by subplot and decay class with the R CWD flux dataframe from 2019 


Modeled_annual_R_T_SWC_2019 <- merge(CWD_2019_subplot_agg_sub, Modeled_annual_R_T_SWC_2019, by = c("decay_class", "Subplot_ID"))

###Convert from Rcwd (umol C kg yr) -> Mg C subplot yr: use the subplot level mass (kg) of CWD within each decay class 
Modeled_annual_R_T_SWC_2019 <- Modeled_annual_R_T_SWC_2019%>%
  mutate(R_CWD_Mg_subplot_yr = R_CWD_umol_kg_year*C_mass_kg_subplot_total/1000000*12.0107/1000000)%>%
  mutate(Remaining_C_mass_Mg_subplot_yr = C_mass_Mg_subplot_total -R_CWD_Mg_subplot_yr )


###Low 
###Merge the CWD mass by subplot and decay class with the R CWD flux dataframe from 2019 

Modeled_annual_R_T_SWC_2019_low <- merge(CWD_2019_subplot_agg_sub, Modeled_annual_R_T_SWC_2019_low, by = c("decay_class", "Subplot_ID"))

###Convert from Rcwd (umol C kg yr) -> Mg C subplot yr: use the subplot level mass (kg) of CWD within each decay class 
Modeled_annual_R_T_SWC_2019_low <- Modeled_annual_R_T_SWC_2019_low%>%
  mutate(R_CWD_Mg_subplot_yr_low = R_CWD_umol_kg_year_low*C_mass_kg_subplot_total/1000000*12.0107/1000000)%>%
  mutate(Remaining_C_mass_Mg_subplot_yr_low = C_mass_Mg_subplot_total -R_CWD_Mg_subplot_yr_low )


###high
###Merge the CWD mass by subplot and decay class with the R CWD flux dataframe from 2019 

Modeled_annual_R_T_SWC_2019_high <- merge(CWD_2019_subplot_agg_sub, Modeled_annual_R_T_SWC_2019_high, by = c("decay_class", "Subplot_ID"))

###Convert from Rcwd (umol C kg yr) -> Mg C subplot yr: use the subplot level mass (kg) of CWD within each decay class 
Modeled_annual_R_T_SWC_2019_high <- Modeled_annual_R_T_SWC_2019_high%>%
  mutate(R_CWD_Mg_subplot_yr_high = R_CWD_umol_kg_year_high*C_mass_kg_subplot_total/1000000*12.0107/1000000)%>%
  mutate(Remaining_C_mass_Mg_subplot_yr_high = C_mass_Mg_subplot_total -R_CWD_Mg_subplot_yr_high)




#######2021 

#Modeled_T_SWC_2020 <- merge(Tower_SWC_6hour_2020_modeled,   Tower_T_6hour_2020_modeled, by = c("Timestamp_6", "Subplot_ID"))

##Assume the modeled SWC below 0 is = to 0   
#Modeled_T_SWC_2020 <- Modeled_T_SWC_2020%>%
  #mutate(modeled_SWC_adjusted = case_when(modeled_SWC < 0 ~ 0, 
                                          #modeled_SWC > 0 ~ modeled_SWC))

##Apply the regression for CWD 1.2773*exp(-0.0339*modeled_temp) + 0.2493*log(fixed GWC), if temp <0, R CWD = 0. R_CWD (umol C kg s ). fixed gravemetric water content from the Gough et al. 2007 based on decay classes 
##Decay class one: 50%
##Decay class two: 80%
##Decay class three: 75%
##Decay class four: 260%
##Decay class five: 200%

Modeled_R_T_SWC_2020 <- Tower_T_6hour_2020_modeled%>%
  mutate(R_CWD_umol_kg_s_1 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(50), 
                                       modeled_temp < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_2 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(85), 
                                       modeled_temp < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_3 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(72), 
                                       modeled_temp < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_4 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(260), 
                                       modeled_temp < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_5 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(200), 
                                       modeled_temp < 0 ~ 0))

Modeled_R_T_SWC_2020_low <- Tower_T_6hour_2020_modeled%>%
  mutate(R_CWD_umol_kg_s_1_low = case_when(modeled_temp_low > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_low)) + 0.2493*log(50), 
                                           modeled_temp_low < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_2_low = case_when(modeled_temp_low > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_low)) + 0.2493*log(85), 
                                           modeled_temp_low < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_3_low = case_when(modeled_temp_low > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_low)) + 0.2493*log(72), 
                                           modeled_temp_low < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_4_low = case_when(modeled_temp_low > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_low)) + 0.2493*log(260), 
                                           modeled_temp_low < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_5_low = case_when(modeled_temp_low > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_low)) + 0.2493*log(200), 
                                           modeled_temp_low < 0 ~ 0))


Modeled_R_T_SWC_2020_high <- Tower_T_6hour_2020_modeled%>%
  mutate(R_CWD_umol_kg_s_1_high = case_when(modeled_temp_high > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_high)) + 0.2493*log(50), 
                                            modeled_temp_high < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_2_high = case_when(modeled_temp_high > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_high)) + 0.2493*log(85), 
                                            modeled_temp_high < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_3_high = case_when(modeled_temp_high > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_high)) + 0.2493*log(72), 
                                            modeled_temp_high < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_4_high = case_when(modeled_temp_high > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_high)) + 0.2493*log(260), 
                                            modeled_temp_high < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_5_high = case_when(modeled_temp_high > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_high)) + 0.2493*log(200), 
                                            modeled_temp_high < 0 ~ 0))


# #####Convert negative values to zero 
# Modeled_R_T_SWC_2020 <- Modeled_R_T_SWC_2020%>%
#   mutate(R_CWD_umol_kg_s_1_adj = case_when(R_CWD_umol_kg_s_1 >= 0 ~ Modeled_R_T_SWC_2020$R_CWD_umol_kg_s_1, 
#                                            R_CWD_umol_kg_s_1 < 0 ~ 0))%>%
#   mutate(R_CWD_umol_kg_s_2_adj = case_when(R_CWD_umol_kg_s_2 >= 0 ~ Modeled_R_T_SWC_2020$R_CWD_umol_kg_s_2, 
#                                            R_CWD_umol_kg_s_2 < 0 ~ 0))%>%
#   mutate(R_CWD_umol_kg_s_3_adj = case_when(R_CWD_umol_kg_s_3 >= 0 ~ Modeled_R_T_SWC_2020$R_CWD_umol_kg_s_3, 
#                                            R_CWD_umol_kg_s_3 < 0 ~ 0))%>%
#   mutate(R_CWD_umol_kg_s_4_adj = case_when(R_CWD_umol_kg_s_4 >= 0 ~ Modeled_R_T_SWC_2020$R_CWD_umol_kg_s_4, 
#                                            R_CWD_umol_kg_s_4 < 0 ~ 0))%>%
#   mutate(R_CWD_umol_kg_s_5_adj = case_when(R_CWD_umol_kg_s_5 >= 0 ~ Modeled_R_T_SWC_2020$R_CWD_umol_kg_s_5, 
#                                            R_CWD_umol_kg_s_5 < 0 ~ 0))


###Scale Rcwd from (umol C kg s) -> umol C kg 6hour

Modeled_6hour_R_T_SWC_2020 <- Modeled_R_T_SWC_2020%>%
  mutate(R_CWD_umol_kg_6hour_1 = R_CWD_umol_kg_s_1*21600)%>%
  mutate(R_CWD_umol_kg_6hour_2 = R_CWD_umol_kg_s_2*21600)%>%
  mutate(R_CWD_umol_kg_6hour_3 = R_CWD_umol_kg_s_3*21600)%>%
  mutate(R_CWD_umol_kg_6hour_4 = R_CWD_umol_kg_s_4*21600)%>%
  mutate(R_CWD_umol_kg_6hour_5 = R_CWD_umol_kg_s_5*21600)

###Low
Modeled_6hour_R_T_SWC_2020_low <- Modeled_R_T_SWC_2020_low%>%
  mutate(R_CWD_umol_kg_6hour_1_low = R_CWD_umol_kg_s_1_low*21600)%>%
  mutate(R_CWD_umol_kg_6hour_2_low = R_CWD_umol_kg_s_2_low*21600)%>%
  mutate(R_CWD_umol_kg_6hour_3_low = R_CWD_umol_kg_s_3_low*21600)%>%
  mutate(R_CWD_umol_kg_6hour_4_low = R_CWD_umol_kg_s_4_low*21600)%>%
  mutate(R_CWD_umol_kg_6hour_5_low = R_CWD_umol_kg_s_5_low*21600)

#high 
Modeled_6hour_R_T_SWC_2020_high <- Modeled_R_T_SWC_2020_high%>%
  mutate(R_CWD_umol_kg_6hour_1_high = R_CWD_umol_kg_s_1_high*21600)%>%
  mutate(R_CWD_umol_kg_6hour_2_high = R_CWD_umol_kg_s_2_high*21600)%>%
  mutate(R_CWD_umol_kg_6hour_3_high = R_CWD_umol_kg_s_3_high*21600)%>%
  mutate(R_CWD_umol_kg_6hour_4_high = R_CWD_umol_kg_s_4_high*21600)%>%
  mutate(R_CWD_umol_kg_6hour_5_high = R_CWD_umol_kg_s_5_high*21600)


###Scale from umol C kg 6hour -> umol C kg yr
Modeled_annual_R_T_SWC_2020 <- Modeled_6hour_R_T_SWC_2020%>%
  group_by(Subplot_ID)%>%
  summarize(R_CWD_umol_kg_year_1 = sum(R_CWD_umol_kg_6hour_1),R_CWD_umol_kg_year_2 = sum(R_CWD_umol_kg_6hour_2),R_CWD_umol_kg_year_3 = sum(R_CWD_umol_kg_6hour_3),R_CWD_umol_kg_year_4 = sum(R_CWD_umol_kg_6hour_4),R_CWD_umol_kg_year_5 = sum(R_CWD_umol_kg_6hour_5))

##Low 
Modeled_annual_R_T_SWC_2020_low <- Modeled_6hour_R_T_SWC_2020_low%>%
  group_by(Subplot_ID)%>%
  summarize(R_CWD_umol_kg_year_1_low = sum(R_CWD_umol_kg_6hour_1_low),R_CWD_umol_kg_year_2_low = sum(R_CWD_umol_kg_6hour_2_low),R_CWD_umol_kg_year_3_low = sum(R_CWD_umol_kg_6hour_3_low),R_CWD_umol_kg_year_4_low = sum(R_CWD_umol_kg_6hour_4_low),R_CWD_umol_kg_year_5_low = sum(R_CWD_umol_kg_6hour_5_low))

##High 
Modeled_annual_R_T_SWC_2020_high <- Modeled_6hour_R_T_SWC_2020_high%>%
  group_by(Subplot_ID)%>%
  summarize(R_CWD_umol_kg_year_1_high = sum(R_CWD_umol_kg_6hour_1_high),R_CWD_umol_kg_year_2_high = sum(R_CWD_umol_kg_6hour_2_high),R_CWD_umol_kg_year_3_high = sum(R_CWD_umol_kg_6hour_3_high),R_CWD_umol_kg_year_4_high = sum(R_CWD_umol_kg_6hour_4_high),R_CWD_umol_kg_year_5_high = sum(R_CWD_umol_kg_6hour_5_high))


##Make modeled Rcwd dataframe long again

Modeled_annual_R_T_SWC_2020 <- Modeled_annual_R_T_SWC_2020%>%
  rename("1" = R_CWD_umol_kg_year_1, "2" = R_CWD_umol_kg_year_2, "3" = R_CWD_umol_kg_year_3, "4" = R_CWD_umol_kg_year_4, "5" = R_CWD_umol_kg_year_5)

Modeled_annual_R_T_SWC_2020_low <- Modeled_annual_R_T_SWC_2020_low%>%
  rename("1" = R_CWD_umol_kg_year_1_low, "2" = R_CWD_umol_kg_year_2_low, "3" = R_CWD_umol_kg_year_3_low, "4" = R_CWD_umol_kg_year_4_low, "5" = R_CWD_umol_kg_year_5_low)

Modeled_annual_R_T_SWC_2020_high <- Modeled_annual_R_T_SWC_2020_high%>%
  rename("1" = R_CWD_umol_kg_year_1_high, "2" = R_CWD_umol_kg_year_2_high, "3" = R_CWD_umol_kg_year_3_high, "4" = R_CWD_umol_kg_year_4_high, "5" = R_CWD_umol_kg_year_5_high)

Modeled_annual_R_T_SWC_2020 <- gather(Modeled_annual_R_T_SWC_2020, decay_class, R_CWD_umol_kg_year, "1":"5", factor_key = TRUE)

Modeled_annual_R_T_SWC_2020_low <- gather(Modeled_annual_R_T_SWC_2020_low, decay_class, R_CWD_umol_kg_year_low, "1":"5", factor_key = TRUE)

Modeled_annual_R_T_SWC_2020_high <- gather(Modeled_annual_R_T_SWC_2020_high, decay_class, R_CWD_umol_kg_year_high, "1":"5", factor_key = TRUE)

###Bring in Mass of CWD in 2020 from Modeled_annual_R_T_SWC_2019,add CWD additions, and convert to kg 

CWD_2020_T_VWC_remaining <- Modeled_annual_R_T_SWC_2019%>%
  select(Subplot_ID, decay_class, Remaining_C_mass_Mg_subplot_yr)

dendro_data_CWD_subplot_2020 <- dendro_data_CWD_subplot_2020%>%
  rename(Subplot_ID = subplot_id)

CWD_2020_T_VWC_remaining_plus<- merge( CWD_2020_T_VWC_remaining ,dendro_data_CWD_subplot_2020, by = c("Subplot_ID", "decay_class"), all = TRUE)%>%
  rename(Mass_additions_Mg_subplot_2020 = C_mass_Mg_subplot)%>%
  select(!year)%>%
  select(!rep_id)


##Make the NAs = zero for addition 

CWD_2020_T_VWC_remaining_plus <- replace(CWD_2020_T_VWC_remaining_plus, is.na(CWD_2020_T_VWC_remaining_plus), 0)

##Add the remaining C mass to the additional CWD mass that entered the pool and convert to kg 

CWD_2020_T_VWC_remaining_plus <- CWD_2020_T_VWC_remaining_plus%>%
  mutate(C_mass_Mg_subplot_total = Mass_additions_Mg_subplot_2020 + Remaining_C_mass_Mg_subplot_yr)%>%
  mutate(C_mass_kg_subplot_total = C_mass_Mg_subplot_total*1000)
  


###Merge the CWD mass by subplot and decay class with the R CWD flux dataframe from 2020 

Modeled_annual_R_T_SWC_2020 <- merge(CWD_2020_T_VWC_remaining_plus, Modeled_annual_R_T_SWC_2020, by = c("decay_class", "Subplot_ID"))

###Convert from Rcwd (umol C kg yr) -> Mg C subplot yr: use the subplot level mass (kg) of CWD within each decay class. Subtract the subplot level CWD mass from the flux rate to determine the remaining Cmass for 2020 
Modeled_annual_R_T_SWC_2020 <- Modeled_annual_R_T_SWC_2020%>%
  mutate(R_CWD_Mg_subplot_yr = R_CWD_umol_kg_year*C_mass_kg_subplot_total/1000000*12.0107/1000000)%>%
  mutate(Remaining_C_mass_Mg_subplot_yr = C_mass_Mg_subplot_total - R_CWD_Mg_subplot_yr)

#########low
###Bring in Mass of CWD in 2020 from Modeled_annual_R_T_SWC_2019,add CWD additions, and convert to kg 

CWD_2020_T_VWC_remaining_low <- Modeled_annual_R_T_SWC_2019_low%>%
  select(Subplot_ID, decay_class, Remaining_C_mass_Mg_subplot_yr_low)

CWD_2020_T_VWC_remaining_plus_low<- merge( CWD_2020_T_VWC_remaining_low ,dendro_data_CWD_subplot_2020, by = c("Subplot_ID", "decay_class"), all = TRUE)%>%
  rename(Mass_additions_Mg_subplot_2020 = C_mass_Mg_subplot)%>%
  select(!year)%>%
  select(!rep_id)


##Make the NAs = zero for addition 

CWD_2020_T_VWC_remaining_plus_low <- replace(CWD_2020_T_VWC_remaining_plus_low, is.na(CWD_2020_T_VWC_remaining_plus_low), 0)

##Add the remaining C mass to the additional CWD mass that entered the pool and convert to kg 

CWD_2020_T_VWC_remaining_plus_low <- CWD_2020_T_VWC_remaining_plus_low%>%
  mutate(C_mass_Mg_subplot_total_low = Mass_additions_Mg_subplot_2020 + Remaining_C_mass_Mg_subplot_yr_low)%>%
  mutate(C_mass_kg_subplot_total_low = C_mass_Mg_subplot_total_low*1000)



###Merge the CWD mass by subplot and decay class with the R CWD flux dataframe from 2020 

Modeled_annual_R_T_SWC_2020_low <- merge(CWD_2020_T_VWC_remaining_plus_low, Modeled_annual_R_T_SWC_2020_low, by = c("decay_class", "Subplot_ID"))

###Convert from Rcwd (umol C kg yr) -> Mg C subplot yr: use the subplot level mass (kg) of CWD within each decay class. Subtract the subplot level CWD mass from the flux rate to determine the remaining Cmass for 2020 
Modeled_annual_R_T_SWC_2020_low <- Modeled_annual_R_T_SWC_2020_low%>%
  mutate(R_CWD_Mg_subplot_yr_low = R_CWD_umol_kg_year_low*C_mass_kg_subplot_total_low/1000000*12.0107/1000000)%>%
  mutate(Remaining_C_mass_Mg_subplot_yr_low = C_mass_Mg_subplot_total_low - R_CWD_Mg_subplot_yr_low)

#########High
###Bring in Mass of CWD in 2020 from Modeled_annual_R_T_SWC_2019,add CWD additions, and convert to kg 

CWD_2020_T_VWC_remaining_high <- Modeled_annual_R_T_SWC_2019_high%>%
  select(Subplot_ID, decay_class, Remaining_C_mass_Mg_subplot_yr_high)

CWD_2020_T_VWC_remaining_plus_high<- merge( CWD_2020_T_VWC_remaining_high ,dendro_data_CWD_subplot_2020, by = c("Subplot_ID", "decay_class"), all = TRUE)%>%
  rename(Mass_additions_Mg_subplot_2020 = C_mass_Mg_subplot)%>%
  select(!year)%>%
  select(!rep_id)


##Make the NAs = zero for addition 

CWD_2020_T_VWC_remaining_plus_high <- replace(CWD_2020_T_VWC_remaining_plus_high, is.na(CWD_2020_T_VWC_remaining_plus_high), 0)

##Add the remaining C mass to the additional CWD mass that entered the pool and convert to kg 

CWD_2020_T_VWC_remaining_plus_high <- CWD_2020_T_VWC_remaining_plus_high%>%
  mutate(C_mass_Mg_subplot_total_high = Mass_additions_Mg_subplot_2020 + Remaining_C_mass_Mg_subplot_yr_high)%>%
  mutate(C_mass_kg_subplot_total_high = C_mass_Mg_subplot_total_high*1000)



###Merge the CWD mass by subplot and decay class with the R CWD flux dataframe from 2020 

Modeled_annual_R_T_SWC_2020_high <- merge(CWD_2020_T_VWC_remaining_plus_high, Modeled_annual_R_T_SWC_2020_high, by = c("decay_class", "Subplot_ID"))

###Convert from Rcwd (umol C kg yr) -> Mg C subplot yr: use the subplot level mass (kg) of CWD within each decay class. Subtract the subplot level CWD mass from the flux rate to determine the remaining Cmass for 2020 
Modeled_annual_R_T_SWC_2020_high <- Modeled_annual_R_T_SWC_2020_high%>%
  mutate(R_CWD_Mg_subplot_yr_high = R_CWD_umol_kg_year_high*C_mass_kg_subplot_total_high/1000000*12.0107/1000000)%>%
  mutate(Remaining_C_mass_Mg_subplot_yr_high = C_mass_Mg_subplot_total_high - R_CWD_Mg_subplot_yr_high)


#######2021

##Apply the regression for CWD 1.2773*exp(-0.0339*modeled_temp) + 0.2493*log(fixed GWC), if temp <0, R CWD = 0. R_CWD (umol C kg s ). fixed gravemetric water content from the Gough et al. 2007 based on decay classes 
##Decay class one: 50%
##Decay class two: 80%
##Decay class three: 75%
##Decay class four: 260%
##Decay class five: 200%

Modeled_R_T_SWC_2021 <- Tower_T_6hour_2021_modeled%>%
  mutate(R_CWD_umol_kg_s_1 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(50), 
                                       modeled_temp < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_2 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(85), 
                                       modeled_temp < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_3 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(72), 
                                       modeled_temp < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_4 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(260), 
                                       modeled_temp < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_5 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(200), 
                                       modeled_temp < 0 ~ 0))

Modeled_R_T_SWC_2021_low <- Tower_T_6hour_2021_modeled%>%
  mutate(R_CWD_umol_kg_s_1_low = case_when(modeled_temp_low > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_low)) + 0.2493*log(50), 
                                           modeled_temp_low < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_2_low = case_when(modeled_temp_low > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_low)) + 0.2493*log(85), 
                                           modeled_temp_low < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_3_low = case_when(modeled_temp_low > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_low)) + 0.2493*log(72), 
                                           modeled_temp_low < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_4_low = case_when(modeled_temp_low > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_low)) + 0.2493*log(260), 
                                           modeled_temp_low < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_5_low = case_when(modeled_temp_low > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_low)) + 0.2493*log(200), 
                                           modeled_temp_low < 0 ~ 0))


Modeled_R_T_SWC_2021_high <- Tower_T_6hour_2021_modeled%>%
  mutate(R_CWD_umol_kg_s_1_high = case_when(modeled_temp_high > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_high)) + 0.2493*log(50), 
                                            modeled_temp_high < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_2_high = case_when(modeled_temp_high > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_high)) + 0.2493*log(85), 
                                            modeled_temp_high < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_3_high = case_when(modeled_temp_high > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_high)) + 0.2493*log(72), 
                                            modeled_temp_high < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_4_high = case_when(modeled_temp_high > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_high)) + 0.2493*log(260), 
                                            modeled_temp_high < 0 ~ 0))%>%
  mutate(R_CWD_umol_kg_s_5_high = case_when(modeled_temp_high > 0 ~ (-1.2773*exp(-0.0339*modeled_temp_high)) + 0.2493*log(200), 
                                            modeled_temp_high < 0 ~ 0))




###Scale Rcwd from (umol C kg s) -> umol C kg 6hour

Modeled_6hour_R_T_SWC_2021 <- Modeled_R_T_SWC_2021%>%
  mutate(R_CWD_umol_kg_6hour_1 = R_CWD_umol_kg_s_1*21600)%>%
  mutate(R_CWD_umol_kg_6hour_2 = R_CWD_umol_kg_s_2*21600)%>%
  mutate(R_CWD_umol_kg_6hour_3 = R_CWD_umol_kg_s_3*21600)%>%
  mutate(R_CWD_umol_kg_6hour_4 = R_CWD_umol_kg_s_4*21600)%>%
  mutate(R_CWD_umol_kg_6hour_5 = R_CWD_umol_kg_s_5*21600)

###Low
Modeled_6hour_R_T_SWC_2021_low <- Modeled_R_T_SWC_2021_low%>%
  mutate(R_CWD_umol_kg_6hour_1_low = R_CWD_umol_kg_s_1_low*21600)%>%
  mutate(R_CWD_umol_kg_6hour_2_low = R_CWD_umol_kg_s_2_low*21600)%>%
  mutate(R_CWD_umol_kg_6hour_3_low = R_CWD_umol_kg_s_3_low*21600)%>%
  mutate(R_CWD_umol_kg_6hour_4_low = R_CWD_umol_kg_s_4_low*21600)%>%
  mutate(R_CWD_umol_kg_6hour_5_low = R_CWD_umol_kg_s_5_low*21600)

#high 
Modeled_6hour_R_T_SWC_2021_high <- Modeled_R_T_SWC_2021_high%>%
  mutate(R_CWD_umol_kg_6hour_1_high = R_CWD_umol_kg_s_1_high*21600)%>%
  mutate(R_CWD_umol_kg_6hour_2_high = R_CWD_umol_kg_s_2_high*21600)%>%
  mutate(R_CWD_umol_kg_6hour_3_high = R_CWD_umol_kg_s_3_high*21600)%>%
  mutate(R_CWD_umol_kg_6hour_4_high = R_CWD_umol_kg_s_4_high*21600)%>%
  mutate(R_CWD_umol_kg_6hour_5_high = R_CWD_umol_kg_s_5_high*21600)


###Scale from umol C kg 6hour -> umol C kg yr
Modeled_annual_R_T_SWC_2021 <- Modeled_6hour_R_T_SWC_2021%>%
  group_by(Subplot_ID)%>%
  summarize(R_CWD_umol_kg_year_1 = sum(R_CWD_umol_kg_6hour_1),R_CWD_umol_kg_year_2 = sum(R_CWD_umol_kg_6hour_2),R_CWD_umol_kg_year_3 = sum(R_CWD_umol_kg_6hour_3),R_CWD_umol_kg_year_4 = sum(R_CWD_umol_kg_6hour_4),R_CWD_umol_kg_year_5 = sum(R_CWD_umol_kg_6hour_5))

##Low 
Modeled_annual_R_T_SWC_2021_low <- Modeled_6hour_R_T_SWC_2021_low%>%
  group_by(Subplot_ID)%>%
  summarize(R_CWD_umol_kg_year_1_low = sum(R_CWD_umol_kg_6hour_1_low),R_CWD_umol_kg_year_2_low = sum(R_CWD_umol_kg_6hour_2_low),R_CWD_umol_kg_year_3_low = sum(R_CWD_umol_kg_6hour_3_low),R_CWD_umol_kg_year_4_low = sum(R_CWD_umol_kg_6hour_4_low),R_CWD_umol_kg_year_5_low = sum(R_CWD_umol_kg_6hour_5_low))

##High 
Modeled_annual_R_T_SWC_2021_high <- Modeled_6hour_R_T_SWC_2021_high%>%
  group_by(Subplot_ID)%>%
  summarize(R_CWD_umol_kg_year_1_high = sum(R_CWD_umol_kg_6hour_1_high),R_CWD_umol_kg_year_2_high = sum(R_CWD_umol_kg_6hour_2_high),R_CWD_umol_kg_year_3_high = sum(R_CWD_umol_kg_6hour_3_high),R_CWD_umol_kg_year_4_high = sum(R_CWD_umol_kg_6hour_4_high),R_CWD_umol_kg_year_5_high = sum(R_CWD_umol_kg_6hour_5_high))


##Make modeled Rcwd dataframe long again

Modeled_annual_R_T_SWC_2021 <- Modeled_annual_R_T_SWC_2021%>%
  rename("1" = R_CWD_umol_kg_year_1, "2" = R_CWD_umol_kg_year_2, "3" = R_CWD_umol_kg_year_3, "4" = R_CWD_umol_kg_year_4, "5" = R_CWD_umol_kg_year_5)

Modeled_annual_R_T_SWC_2021_low <- Modeled_annual_R_T_SWC_2021_low%>%
  rename("1" = R_CWD_umol_kg_year_1_low, "2" = R_CWD_umol_kg_year_2_low, "3" = R_CWD_umol_kg_year_3_low, "4" = R_CWD_umol_kg_year_4_low, "5" = R_CWD_umol_kg_year_5_low)

Modeled_annual_R_T_SWC_2021_high <- Modeled_annual_R_T_SWC_2021_high%>%
  rename("1" = R_CWD_umol_kg_year_1_high, "2" = R_CWD_umol_kg_year_2_high, "3" = R_CWD_umol_kg_year_3_high, "4" = R_CWD_umol_kg_year_4_high, "5" = R_CWD_umol_kg_year_5_high)

Modeled_annual_R_T_SWC_2021 <- gather(Modeled_annual_R_T_SWC_2021, decay_class, R_CWD_umol_kg_year, "1":"5", factor_key = TRUE)

Modeled_annual_R_T_SWC_2021_low <- gather(Modeled_annual_R_T_SWC_2021_low, decay_class, R_CWD_umol_kg_year_low, "1":"5", factor_key = TRUE)

Modeled_annual_R_T_SWC_2021_high <- gather(Modeled_annual_R_T_SWC_2021_high, decay_class, R_CWD_umol_kg_year_high, "1":"5", factor_key = TRUE)

###Bring in Mass of CWD in 2020 from Modeled_annual_R_T_SWC_2019,add CWD additions, and convert to kg 

CWD_2021_T_VWC_remaining <- Modeled_annual_R_T_SWC_2020%>%
  select(Subplot_ID, decay_class, Remaining_C_mass_Mg_subplot_yr)

dendro_data_CWD_subplot_2021 <- dendro_data_CWD_subplot_2021%>%
  rename(Subplot_ID = subplot_id)

CWD_2021_T_VWC_remaining_plus<- merge( CWD_2021_T_VWC_remaining ,dendro_data_CWD_subplot_2021, by = c("Subplot_ID", "decay_class"), all = TRUE)%>%
  rename(Mass_additions_Mg_subplot_2021 = C_mass_Mg_subplot)%>%
  select(!year)%>%
  select(!rep_id)


##Make the NAs = zero for addition 

CWD_2021_T_VWC_remaining_plus <- replace(CWD_2021_T_VWC_remaining_plus, is.na(CWD_2021_T_VWC_remaining_plus), 0)

##Add the remaining C mass to the additional CWD mass that entered the pool and convert to kg 

CWD_2021_T_VWC_remaining_plus <- CWD_2021_T_VWC_remaining_plus%>%
  mutate(C_mass_Mg_subplot_total = Mass_additions_Mg_subplot_2021 + Remaining_C_mass_Mg_subplot_yr)%>%
  mutate(C_mass_kg_subplot_total = C_mass_Mg_subplot_total*1000)



###Merge the CWD mass by subplot and decay class with the R CWD flux dataframe from 2020 

Modeled_annual_R_T_SWC_2021 <- merge(CWD_2021_T_VWC_remaining_plus, Modeled_annual_R_T_SWC_2021, by = c("decay_class", "Subplot_ID"))

###Convert from Rcwd (umol C kg yr) -> Mg C subplot yr: use the subplot level mass (kg) of CWD within each decay class. Subtract the subplot level CWD mass from the flux rate to determine the remaining Cmass for 2020 
Modeled_annual_R_T_SWC_2021 <- Modeled_annual_R_T_SWC_2021%>%
  mutate(R_CWD_Mg_subplot_yr = R_CWD_umol_kg_year*C_mass_kg_subplot_total/1000000*12.0107/1000000)%>%
  mutate(Remaining_C_mass_Mg_subplot_yr = C_mass_Mg_subplot_total - R_CWD_Mg_subplot_yr)

#########low
###Bring in Mass of CWD in 2020 from Modeled_annual_R_T_SWC_2019,add CWD additions, and convert to kg 

CWD_2021_T_VWC_remaining_low <- Modeled_annual_R_T_SWC_2020_low%>%
  select(Subplot_ID, decay_class, Remaining_C_mass_Mg_subplot_yr_low)

CWD_2021_T_VWC_remaining_plus_low<- merge( CWD_2021_T_VWC_remaining_low ,dendro_data_CWD_subplot_2021, by = c("Subplot_ID", "decay_class"), all = TRUE)%>%
  rename(Mass_additions_Mg_subplot_2021 = C_mass_Mg_subplot)%>%
  select(!year)%>%
  select(!rep_id)


##Make the NAs = zero for addition 

CWD_2021_T_VWC_remaining_plus_low <- replace(CWD_2021_T_VWC_remaining_plus_low, is.na(CWD_2021_T_VWC_remaining_plus_low), 0)

##Add the remaining C mass to the additional CWD mass that entered the pool and convert to kg 

CWD_2021_T_VWC_remaining_plus_low <- CWD_2021_T_VWC_remaining_plus_low%>%
  mutate(C_mass_Mg_subplot_total_low = Mass_additions_Mg_subplot_2021 + Remaining_C_mass_Mg_subplot_yr_low)%>%
  mutate(C_mass_kg_subplot_total_low = C_mass_Mg_subplot_total_low*1000)



###Merge the CWD mass by subplot and decay class with the R CWD flux dataframe from 2020 

Modeled_annual_R_T_SWC_2021_low <- merge(CWD_2021_T_VWC_remaining_plus_low, Modeled_annual_R_T_SWC_2021_low, by = c("decay_class", "Subplot_ID"))

###Convert from Rcwd (umol C kg yr) -> Mg C subplot yr: use the subplot level mass (kg) of CWD within each decay class. Subtract the subplot level CWD mass from the flux rate to determine the remaining Cmass for 2020 
Modeled_annual_R_T_SWC_2021_low <- Modeled_annual_R_T_SWC_2021_low%>%
  mutate(R_CWD_Mg_subplot_yr_low = R_CWD_umol_kg_year_low*C_mass_kg_subplot_total_low/1000000*12.0107/1000000)%>%
  mutate(Remaining_C_mass_Mg_subplot_yr_low = C_mass_Mg_subplot_total_low - R_CWD_Mg_subplot_yr_low)

#########High
###Bring in Mass of CWD in 2020 from Modeled_annual_R_T_SWC_2019,add CWD additions, and convert to kg 

CWD_2021_T_VWC_remaining_high <- Modeled_annual_R_T_SWC_2020_high%>%
  select(Subplot_ID, decay_class, Remaining_C_mass_Mg_subplot_yr_high)

CWD_2021_T_VWC_remaining_plus_high<- merge( CWD_2021_T_VWC_remaining_high ,dendro_data_CWD_subplot_2021, by = c("Subplot_ID", "decay_class"), all = TRUE)%>%
  rename(Mass_additions_Mg_subplot_2021 = C_mass_Mg_subplot)%>%
  select(!year)%>%
  select(!rep_id)


##Make the NAs = zero for addition 

CWD_2021_T_VWC_remaining_plus_high <- replace(CWD_2021_T_VWC_remaining_plus_high, is.na(CWD_2021_T_VWC_remaining_plus_high), 0)

##Add the remaining C mass to the additional CWD mass that entered the pool and convert to kg 

CWD_2021_T_VWC_remaining_plus_high <- CWD_2021_T_VWC_remaining_plus_high%>%
  mutate(C_mass_Mg_subplot_total_high = Mass_additions_Mg_subplot_2021 + Remaining_C_mass_Mg_subplot_yr_high)%>%
  mutate(C_mass_kg_subplot_total_high = C_mass_Mg_subplot_total_high*1000)



###Merge the CWD mass by subplot and decay class with the R CWD flux dataframe from 2020 

Modeled_annual_R_T_SWC_2021_high <- merge(CWD_2021_T_VWC_remaining_plus_high, Modeled_annual_R_T_SWC_2021_high, by = c("decay_class", "Subplot_ID"))

###Convert from Rcwd (umol C kg yr) -> Mg C subplot yr: use the subplot level mass (kg) of CWD within each decay class. Subtract the subplot level CWD mass from the flux rate to determine the remaining Cmass for 2020 
Modeled_annual_R_T_SWC_2021_high <- Modeled_annual_R_T_SWC_2021_high%>%
  mutate(R_CWD_Mg_subplot_yr_high = R_CWD_umol_kg_year_high*C_mass_kg_subplot_total_high/1000000*12.0107/1000000)%>%
  mutate(Remaining_C_mass_Mg_subplot_yr_high = C_mass_Mg_subplot_total_high - R_CWD_Mg_subplot_yr_high)
  

# 
# #######2022
# 
# 
# 
# ##Apply the regression for CWD 1.2773*exp(-0.0339*modeled_temp) + 0.2493*log(fixed GWC), if temp <0, R CWD = 0. R_CWD (umol C kg s ). fixed gravemetric water content from the Gough et al. 2007 based on decay classes 
# ##Decay class one: 50%
# ##Decay class two: 80%
# ##Decay class three: 75%
# ##Decay class four: 260%
# ##Decay class five: 200%
# 
# Tower_R_T_6hour_2022_modeled <- Tower_T_6hour_2022_modeled%>%
#   mutate(R_CWD_umol_kg_s_1 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(50), 
#                                        modeled_temp < 0 ~ 0))%>%
#   mutate(R_CWD_umol_kg_s_2 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(85), 
#                                        modeled_temp < 0 ~ 0))%>%
#   mutate(R_CWD_umol_kg_s_3 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(72), 
#                                        modeled_temp < 0 ~ 0))%>%
#   mutate(R_CWD_umol_kg_s_4 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(260), 
#                                        modeled_temp < 0 ~ 0))%>%
#   mutate(R_CWD_umol_kg_s_5 = case_when(modeled_temp > 0 ~ (-1.2773*exp(-0.0339*modeled_temp)) + 0.2493*log(200), 
#                                        modeled_temp < 0 ~ 0))
# 
# # #####Convert negative values to zero 
# # Tower_R_T_6hour_2022_modeled<- Tower_T_6hour_2022_modeled%>%
# #   mutate(R_CWD_umol_kg_s_1_adj = case_when(R_CWD_umol_kg_s_1 >= 0 ~ Tower_T_6hour_2022_modeled$R_CWD_umol_kg_s_1, 
# #                                            R_CWD_umol_kg_s_1 < 0 ~ 0))%>%
# #   mutate(R_CWD_umol_kg_s_2_adj = case_when(R_CWD_umol_kg_s_2 >= 0 ~ Tower_T_6hour_2022_modeled$R_CWD_umol_kg_s_2, 
# #                                            R_CWD_umol_kg_s_2 < 0 ~ 0))%>%
# #   mutate(R_CWD_umol_kg_s_3_adj = case_when(R_CWD_umol_kg_s_3 >= 0 ~ Tower_T_6hour_2022_modeled$R_CWD_umol_kg_s_3, 
# #                                            R_CWD_umol_kg_s_3 < 0 ~ 0))%>%
# #   mutate(R_CWD_umol_kg_s_4_adj = case_when(R_CWD_umol_kg_s_4 >= 0 ~ Tower_T_6hour_2022_modeled$R_CWD_umol_kg_s_4, 
# #                                            R_CWD_umol_kg_s_4 < 0 ~ 0))%>%
# #   mutate(R_CWD_umol_kg_s_5_adj = case_when(R_CWD_umol_kg_s_5 >= 0 ~ Tower_T_6hour_2022_modeled$R_CWD_umol_kg_s_5, 
# #                                            R_CWD_umol_kg_s_5 < 0 ~ 0))
# 
# 
# ###Scale Rcwd from (umol C kg s) -> umol C kg 6hour
# 
# Tower_R_T_6hour_2022_modeled <- Tower_R_T_6hour_2022_modeled%>%
#   mutate(R_CWD_umol_kg_6hour_1 = R_CWD_umol_kg_s_1*21600)%>%
#   mutate(R_CWD_umol_kg_6hour_2 = R_CWD_umol_kg_s_2*21600)%>%
#   mutate(R_CWD_umol_kg_6hour_3 = R_CWD_umol_kg_s_3*21600)%>%
#   mutate(R_CWD_umol_kg_6hour_4 = R_CWD_umol_kg_s_4*21600)%>%
#   mutate(R_CWD_umol_kg_6hour_5 = R_CWD_umol_kg_s_5*21600)
# 
# ###Scale from umol C kg 6hour -> umol C kg yr
# Tower_R_T_annual_2022_modeled <- Tower_R_T_6hour_2022_modeled%>%
#   group_by(Subplot_ID)%>%
#   summarize(R_CWD_umol_kg_year_1 = sum(R_CWD_umol_kg_6hour_1),R_CWD_umol_kg_year_2 = sum(R_CWD_umol_kg_6hour_2),R_CWD_umol_kg_year_3 = sum(R_CWD_umol_kg_6hour_3),R_CWD_umol_kg_year_4 = sum(R_CWD_umol_kg_6hour_4),R_CWD_umol_kg_year_5 = sum(R_CWD_umol_kg_6hour_5))
# 
# 
# ##Make modeled Rcwd dataframe long again
# 
# Tower_R_T_annual_2022_modeled  <- Tower_R_T_annual_2022_modeled %>%
#   rename("1" = R_CWD_umol_kg_year_1, "2" = R_CWD_umol_kg_year_2, "3" = R_CWD_umol_kg_year_3, "4" = R_CWD_umol_kg_year_4, "5" = R_CWD_umol_kg_year_5)
# 
# Tower_R_T_annual_2022_modeled  <- gather(Tower_R_T_annual_2022_modeled , decay_class, R_CWD_umol_kg_year, "1":"5", factor_key = TRUE)
# 
# ###Bring in Mass of CWD in 2022 from Modeled_annual_R_T_SWC_2021,add CWD additions, and convert to kg 
# 
# CWD_2022_T_VWC_remaining <- Modeled_annual_R_T_SWC_2021%>%
#   select(Subplot_ID, decay_class, Remaining_C_mass_Mg_subplot_yr)
# 
# dendro_data_CWD_subplot_2022 <- dendro_data_CWD_subplot_2022%>%
#   rename(Subplot_ID = subplot_id)
# 
# CWD_2022_T_VWC_remaining_plus<- merge( CWD_2022_T_VWC_remaining ,dendro_data_CWD_subplot_2022, by = c("Subplot_ID", "decay_class"), all = TRUE)%>%
#   rename(Mass_additions_Mg_subplot_2022 = C_mass_Mg_subplot)%>%
#   select(!year)%>%
#   select(!rep_id)
# 
# 
# ##Make the NAs = zero for addition 
# 
# CWD_2022_T_VWC_remaining_plus <- replace(CWD_2022_T_VWC_remaining_plus, is.na(CWD_2022_T_VWC_remaining_plus), 0)
# 
# ##Add the remaining C mass to the additional CWD mass that entered the pool and convert to kg 
# 
# CWD_2022_T_VWC_remaining_plus <- CWD_2022_T_VWC_remaining_plus%>%
#   mutate(C_mass_Mg_subplot_total = Mass_additions_Mg_subplot_2022 + Remaining_C_mass_Mg_subplot_yr)%>%
#   mutate(C_mass_kg_subplot_total = C_mass_Mg_subplot_total*1000)
# 
# 
# 
# ###Merge the CWD mass by subplot and decay class with the R CWD flux dataframe from 2020 
# 
# Tower_R_T_annual_2022_modeled <- merge(CWD_2022_T_VWC_remaining_plus, Tower_R_T_annual_2022_modeled, by = c("decay_class", "Subplot_ID"))
# 
# ###Convert from Rcwd (umol C kg yr) -> Mg C subplot yr: use the subplot level mass (kg) of CWD within each decay class. Subtract the subplot level CWD mass from the flux rate to determine the remaining Cmass for 2021
# Tower_R_T_annual_2022_modeled <- Tower_R_T_annual_2022_modeled%>%
#   mutate(R_CWD_Mg_subplot_yr = R_CWD_umol_kg_year*C_mass_kg_subplot_total/1000000*12.0107/1000000)%>%
#   mutate(Remaining_C_mass_Mg_subplot_yr = C_mass_Mg_subplot_total - R_CWD_Mg_subplot_yr)



####Merge all years together 
Modeled_annual_R_T_SWC_2019 <- merge(Modeled_annual_R_T_SWC_2019,Modeled_annual_R_T_SWC_2019_low, by = c("decay_class", "Subplot_ID"))
Modeled_annual_R_T_SWC_2019 <- merge(Modeled_annual_R_T_SWC_2019,Modeled_annual_R_T_SWC_2019_high, by = c("decay_class", "Subplot_ID"))

Modeled_annual_R_T_SWC_2020 <- merge(Modeled_annual_R_T_SWC_2020,Modeled_annual_R_T_SWC_2020_low, by = c("decay_class", "Subplot_ID"))
Modeled_annual_R_T_SWC_2020 <- merge(Modeled_annual_R_T_SWC_2020,Modeled_annual_R_T_SWC_2020_high, by = c("decay_class", "Subplot_ID"))

Modeled_annual_R_T_SWC_2021 <- merge(Modeled_annual_R_T_SWC_2021,Modeled_annual_R_T_SWC_2021_low, by = c("decay_class", "Subplot_ID"))
Modeled_annual_R_T_SWC_2021 <- merge(Modeled_annual_R_T_SWC_2021,Modeled_annual_R_T_SWC_2021_high, by = c("decay_class", "Subplot_ID"))


Modeled_annual_R_T_SWC_2019_sub <- Modeled_annual_R_T_SWC_2019%>%
  select(decay_class, Subplot_ID,R_CWD_Mg_subplot_yr,R_CWD_Mg_subplot_yr_low,R_CWD_Mg_subplot_yr_high)%>%
  mutate(year = "2019")
Modeled_annual_R_T_SWC_2020_sub <- Modeled_annual_R_T_SWC_2020%>%
  select(decay_class, Subplot_ID,R_CWD_Mg_subplot_yr,R_CWD_Mg_subplot_yr,R_CWD_Mg_subplot_yr_low,R_CWD_Mg_subplot_yr_high)%>%
  mutate(year = "2020")
Modeled_annual_R_T_SWC_2021_sub <- Modeled_annual_R_T_SWC_2021%>%
  select(decay_class, Subplot_ID,R_CWD_Mg_subplot_yr,R_CWD_Mg_subplot_yr,R_CWD_Mg_subplot_yr_low,R_CWD_Mg_subplot_yr_high)%>%
  mutate(year = "2021")



#Tower_R_T_annual_2022_modeled_sub <-Tower_R_T_annual_2022_modeled%>%
  #select(decay_class, Subplot_ID,R_CWD_Mg_subplot_yr)%>%
 # mutate(year = "2022")

Modeled_annual_R_T_SWC_all_years <- rbind(Modeled_annual_R_T_SWC_2019_sub, Modeled_annual_R_T_SWC_2020_sub, Modeled_annual_R_T_SWC_2021_sub)

##Scale from subplot to ha 

Modeled_annual_R_T_SWC_all_years <- Modeled_annual_R_T_SWC_all_years%>%
  mutate(R_CWD_Mg_ha_yr = R_CWD_Mg_subplot_yr*10)%>%
  mutate(R_CWD_Mg_ha_yr_low = R_CWD_Mg_subplot_yr_low*10)%>%
  mutate(R_CWD_Mg_ha_yr_high = R_CWD_Mg_subplot_yr_high*10)
  


 ##Add up the decay classes together 
Modeled_annual_R_T_SWC_all_years_total <- Modeled_annual_R_T_SWC_all_years%>%
  group_by(Subplot_ID, year)%>%
 summarize(R_CWD_Mg_ha_yr_total = sum(R_CWD_Mg_ha_yr),R_CWD_Mg_ha_yr_total_low = sum(R_CWD_Mg_ha_yr_low),R_CWD_Mg_ha_yr_total_high = sum(R_CWD_Mg_ha_yr_high) )%>%
  rename(subplot_id = Subplot_ID)

 
# ###Add Severity, treatment and rep to CWD Dataframe 
Modeled_annual_R_T_SWC_all_years_total <-  Modeled_annual_R_T_SWC_all_years_total%>%
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
   ))%>%
  mutate(rep_id = case_when(subplot_id == "A01E" |subplot_id == "A02E" |subplot_id == "A03E" |subplot_id == "A04E" |subplot_id == "A01W" |subplot_id == "A02W" |subplot_id == "A03W"|subplot_id == "A04W" ~ "A",
                            subplot_id == "B01E" |subplot_id == "B02E" |subplot_id == "B03E" |subplot_id == "B04E" |subplot_id == "B01W" |subplot_id == "B02W" |subplot_id == "B03W" |subplot_id == "B04W" ~ "B", 
                            subplot_id == "C01E" |subplot_id == "C02E" |subplot_id == "C03E" |subplot_id == "C04E" |subplot_id == "C01W" |subplot_id == "C02W" |subplot_id == "C03W"|subplot_id == "C04W" ~ "C", 
                            subplot_id == "D01E" |subplot_id == "D02E" |subplot_id == "D03E" |subplot_id == "D04E" |subplot_id == "D01W" |subplot_id == "D02W" |subplot_id == "D03W"|subplot_id == "D04W" ~ "D"))



########################### summary Dataframes for table ####################### 
# ##Summarize by severity 
 Modeled_annual_R_T_SWC_all_years_severity <- Modeled_annual_R_T_SWC_all_years_total%>%
   group_by(severity, year)%>%
   summarize(R_CWD_Mg_ha_yr_mean = mean(R_CWD_Mg_ha_yr_total), R_CWD_Mg_ha_yr_se = std.error(R_CWD_Mg_ha_yr_total))

Modeled_annual_R_T_SWC_all_years_treatment <- Modeled_annual_R_T_SWC_all_years_total%>%
  group_by(treatment, year)%>%
  summarize(R_CWD_Mg_ha_yr_mean = mean(R_CWD_Mg_ha_yr_total), R_CWD_Mg_ha_yr_se = std.error(R_CWD_Mg_ha_yr_total))



####################################### Average CWD flux by method ######################

CWD_all_years_total <- CWD_all_years_total%>%
  rename(R_CWD_Mghayr_1 = Mass_loss_Mg_ha_total)

CWD_all_years_species_total <- CWD_all_years_species_total%>%
  rename(R_CWD_Mghayr_2 = Mass_loss_Mg_ha_total)

Modeled_annual_R_T_SWC_all_years_total <- Modeled_annual_R_T_SWC_all_years_total%>%
  rename(R_CWD_Mghayr_3 = R_CWD_Mg_ha_yr_total,R_CWD_Mghayr_3_low = R_CWD_Mg_ha_yr_total_low,R_CWD_Mghayr_3_high = R_CWD_Mg_ha_yr_total_high)


CWD_all_methods <- merge(CWD_all_years_total, CWD_all_years_species_total, by = c("rep_id", "subplot_id", "treatment", "severity", "year"))

CWD_all_methods <- merge(CWD_all_methods, Modeled_annual_R_T_SWC_all_years_total, by = c("rep_id", "subplot_id", "treatment", "severity", "year"))


####Create an average CWD by subplot per year 
CWD_all_methods <- CWD_all_methods%>%
  mutate(R_CWD_Mghayr_ave = ((R_CWD_Mghayr_1 + R_CWD_Mghayr_2 +R_CWD_Mghayr_3))/3)%>%
  rename(rep = rep_id, type = treatment)

write.csv(CWD_all_methods, "Figures_Output/R_CWD.csv", row.names=FALSE)

################################## STATS ###################################

##Transform variables into factors for model 
CWD_all_methods$severity <- as.factor(CWD_all_methods$severity)
CWD_all_methods$type <- as.factor(CWD_all_methods$type)
CWD_all_methods$year <- as.factor(CWD_all_methods$year)
CWD_all_methods$rep <- as.factor(CWD_all_methods$rep)

####Testing Assumptions 
##Test for outliers test: no extreme outliers
outliers <- CWD_all_methods%>% 
  group_by(severity, type, year) %>%
  identify_outliers(R_CWD_Mghayr_ave)

##Equality of variance test for severity and treatment 
leveneTest(R_CWD_Mghayr_ave ~ year*type*severity, data = CWD_all_methods)

##Normality 
# Build the linear model
normality_test  <- lm(R_CWD_Mghayr_ave ~ severity*type*year,
                      data = CWD_all_methods)

# Shapiro test of normality: Data not normal  
shapiro_test(residuals(normality_test))

CWD_all_methods <- CWD_all_methods%>%
  mutate(R_CWD_Mghayr_ave_log = log(R_CWD_Mghayr_ave))

# Build the linear model
normality_test  <- lm(R_CWD_Mghayr_ave_log ~ severity*type*year,
                      data = CWD_all_methods)

shapiro_test(residuals(normality_test))


####WORKING SPLIT-SPLIT MODEL: Using aov(). Same results as the agricolae package. Ran an ANCOVA with VWC as a covariate.(However significance does not change with or without VWC). 

CWD_model <- aov(R_CWD_Mghayr_ave_log  ~ severity*type*year +rep +Error(rep:severity/type/year), data = CWD_all_methods)

summary(CWD_model)

library(agricolae)

out_year_severity_CWD<- with(CWD_all_methods, LSD.test(R_CWD_Mghayr_ave_log , severity:year,72, 0.1315, console = TRUE))




################################################################## METHOD 4 ###########################################################
############################################################# ED MODEL OUTPUT FOR COARSE STRUCTURAL HETEROTROPHIC RESPIRATION ##################
#############################################################################################################################

# #########read in the data #########################
x_0_2016 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-2016-constant.rds")
x_45_2016 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-2016-constant.rds")
x_65_2016 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-2016-constant.rds")
x_85_2016 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-2016-constant.rds")

x_0_1979 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-1979-constant.rds")
x_45_1979 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-1979-constant.rds")
x_65_1979 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-1979-constant.rds")
x_85_1979 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-1979-constant.rds")

x_0_1981 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-1981-constant.rds")
x_45_1981 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-1981-constant.rds")
x_65_1981 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-1981-constant.rds")
x_85_1981 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-1981-constant.rds")

x_0_1983 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-1983-constant.rds")
x_45_1983 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-1983-constant.rds")
x_65_1983 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-1983-constant.rds")
x_85_1983 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-1983-constant.rds")

x_0_1985 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-1985-constant.rds")
x_45_1985 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-1985-constant.rds")
x_65_1985 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-1985-constant.rds")
x_85_1985 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-1985-constant.rds")

x_0_1985 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-1985-constant.rds")
x_45_1985 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-1985-constant.rds")
x_65_1985 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-1985-constant.rds")
x_85_1985 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-1985-constant.rds")

x_0_1987 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-1987-constant.rds")
x_45_1987 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-1987-constant.rds")
x_65_1987 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-1987-constant.rds")
x_85_1987 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-1987-constant.rds")

x_0_1989 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-1989-constant.rds")
x_45_1989 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-1989-constant.rds")
x_65_1989 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-1989-constant.rds")
x_85_1989 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-1989-constant.rds")

x_0_1991 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-1991-constant.rds")
x_45_1991 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-1991-constant.rds")
x_65_1991 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-1991-constant.rds")
x_85_1991 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-1991-constant.rds")

x_0_1993 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-1993-constant.rds")
x_45_1993 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-1993-constant.rds")
x_65_1993 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-1993-constant.rds")
x_85_1993 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-1993-constant.rds")

x_0_1995 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-1995-constant.rds")
x_45_1995 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-1995-constant.rds")
x_65_1995 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-1995-constant.rds")
x_85_1995 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-1995-constant.rds")

x_0_1997 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-1997-constant.rds")
x_45_1997 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-1997-constant.rds")
x_65_1997 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-1997-constant.rds")
x_85_1997 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-1997-constant.rds")

x_0_2000 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-2000-constant.rds")
x_45_2000 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-2000-constant.rds")
x_65_2000 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-2000-constant.rds")
x_85_2000 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-2000-constant.rds")

x_0_2002 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-2002-constant.rds")
x_45_2002 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-2002-constant.rds")
x_65_2002 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-2002-constant.rds")
x_85_2002 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-2002-constant.rds")

x_0_2004 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-2004-constant.rds")
x_45_2004 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-2004-constant.rds")
x_65_2004 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-2004-constant.rds")
x_85_2004 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-2004-constant.rds")

x_0_2006 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-2006-constant.rds")
x_45_2006 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-2006-constant.rds")
x_65_2006 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-2006-constant.rds")
x_85_2006 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-2006-constant.rds")

x_0_2008 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-2008-constant.rds")
x_45_2008 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-2008-constant.rds")
x_65_2008 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-2008-constant.rds")
x_85_2008 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-2008-constant.rds")

x_0_2010 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-2010-constant.rds")
x_45_2010 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-2010-constant.rds")
x_65_2010 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-2010-constant.rds")
x_85_2010 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-2010-constant.rds")

x_0_2012 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-2012-constant.rds")
x_45_2012 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-2012-constant.rds")
x_65_2012 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-2012-constant.rds")
x_85_2012 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-2012-constant.rds")

x_0_2014 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-2014-constant.rds")
x_45_2014 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-2014-constant.rds")
x_65_2014 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-2014-constant.rds")
x_85_2014 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-2014-constant.rds")

x_0_2019 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_0_1day_above_met-2019-constant.rds")
x_45_2019 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_45_1day_above_met-2019-constant.rds")
x_65_2019 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_65_1day_above_met-2019-constant.rds")
x_85_2019 <- readRDS("Data_CWD/ED-outputs/exp-constant/harvest_85_1day_above_met-2019-constant.rds")


####From Kalyn ########################
# Take a look at the object
dim(x_85_2016)

# tells you the names of the the different elements object, anything with the "df" tag is a data frame
names(x_85_2016)

# the tag following the df tells you a bit about the resolution of the data 
# scalar - is for the full patch 
# cohort - reports results by the cohort code and plant functional type I think 
# pft - reports results by the plant funcitonal type 
# soil - reports soil characteristics 

# the table of variable names https://github.com/FoRTExperiment/ed4forte/blob/master/inst/ed2-state-variables.csv
# is helpful note that we only have the monthly data saved! 

# can be used to give you an idea of what the object looks like 
str(x_85_2016)
#######################################


##################Turn the scalar element into dataframe (This element includes the Rh variables) for all 0 Severity objects ######################
scalar_0_2016 <- as.data.frame(x_0_2016$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "2016")
scalar_0_1979 <- as.data.frame(x_0_1979$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "1979")
scalar_0_1981 <- as.data.frame(x_0_1981$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "1981")
scalar_0_1983 <- as.data.frame(x_0_1983$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "1983")
scalar_0_1985 <- as.data.frame(x_0_1985$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "1985")
scalar_0_1987 <- as.data.frame(x_0_1987$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "1987")
scalar_0_1989 <- as.data.frame(x_0_1989$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "1989")
scalar_0_1991 <- as.data.frame(x_0_1991$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "1991")
scalar_0_1985 <- as.data.frame(x_0_1985$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "1985")
scalar_0_1987 <- as.data.frame(x_0_1987$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "1987")
scalar_0_1989 <- as.data.frame(x_0_1989$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "1989")
scalar_0_1991 <- as.data.frame(x_0_1991$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "1991")
scalar_0_1993 <- as.data.frame(x_0_1993$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "1993")
scalar_0_1995 <- as.data.frame(x_0_1995$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "1995")
scalar_0_1997 <- as.data.frame(x_0_1997$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "1997")
scalar_0_2000 <- as.data.frame(x_0_2000$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "2000")
scalar_0_2002 <- as.data.frame(x_0_2002$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "2002")
scalar_0_2004 <- as.data.frame(x_0_2004$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "2004")
scalar_0_2006 <- as.data.frame(x_0_2006$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "2006")
scalar_0_2008 <- as.data.frame(x_0_2008$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "2008")
scalar_0_2010 <- as.data.frame(x_0_2010$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "2010")
scalar_0_2012 <- as.data.frame(x_0_2012$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "2012")
scalar_0_2014 <- as.data.frame(x_0_2014$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "2014")
scalar_0_2019 <- as.data.frame(x_0_2019$df_scalar)%>%
  mutate(Severity = "0")%>%
  mutate(Scenario = "2019")


##################Turn the scalar element into dataframe (This element includes the Rh variables) for all 45 Severity objects ######################
scalar_45_2016 <- as.data.frame(x_45_2016$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "2016")
scalar_45_1979 <- as.data.frame(x_45_1979$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "1979")
scalar_45_1981 <- as.data.frame(x_45_1981$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "1981")
scalar_45_1983 <- as.data.frame(x_45_1983$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "1983")
scalar_45_1985 <- as.data.frame(x_45_1985$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "1985")
scalar_45_1987 <- as.data.frame(x_45_1987$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "1987")
scalar_45_1989 <- as.data.frame(x_45_1989$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "1989")
scalar_45_1991 <- as.data.frame(x_45_1991$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "1991")
scalar_45_1985 <- as.data.frame(x_45_1985$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "1985")
scalar_45_1987 <- as.data.frame(x_45_1987$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "1987")
scalar_45_1989 <- as.data.frame(x_45_1989$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "1989")
scalar_45_1991 <- as.data.frame(x_45_1991$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "1991")
scalar_45_1993 <- as.data.frame(x_45_1993$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "1993")
scalar_45_1995 <- as.data.frame(x_45_1995$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "1995")
scalar_45_1997 <- as.data.frame(x_45_1997$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "1997")
scalar_45_2000 <- as.data.frame(x_45_2000$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "2000")
scalar_45_2002 <- as.data.frame(x_45_2002$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "2002")
scalar_45_2004 <- as.data.frame(x_45_2004$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "2004")
scalar_45_2006 <- as.data.frame(x_45_2006$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "2006")
scalar_45_2008 <- as.data.frame(x_45_2008$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "2008")
scalar_45_2010 <- as.data.frame(x_45_2010$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "2010")
scalar_45_2012 <- as.data.frame(x_45_2012$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "2012")
scalar_45_2014 <- as.data.frame(x_45_2014$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "2014")
scalar_45_2019 <- as.data.frame(x_45_2019$df_scalar)%>%
  mutate(Severity = "45")%>%
  mutate(Scenario = "2019")

##################Turn the scalar element into dataframe (This element includes the Rh variables) for all 65 Severity objects ######################
scalar_65_2016 <- as.data.frame(x_65_2016$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "2016")
scalar_65_1979 <- as.data.frame(x_65_1979$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "1979")
scalar_65_1981 <- as.data.frame(x_65_1981$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "1981")
scalar_65_1983 <- as.data.frame(x_65_1983$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "1983")
scalar_65_1985 <- as.data.frame(x_65_1985$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "1985")
scalar_65_1987 <- as.data.frame(x_65_1987$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "1987")
scalar_65_1989 <- as.data.frame(x_65_1989$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "1989")
scalar_65_1991 <- as.data.frame(x_65_1991$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "1991")
scalar_65_1985 <- as.data.frame(x_65_1985$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "1985")
scalar_65_1987 <- as.data.frame(x_65_1987$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "1987")
scalar_65_1989 <- as.data.frame(x_65_1989$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "1989")
scalar_65_1991 <- as.data.frame(x_65_1991$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "1991")
scalar_65_1993 <- as.data.frame(x_65_1993$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "1993")
scalar_65_1995 <- as.data.frame(x_65_1995$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "1995")
scalar_65_1997 <- as.data.frame(x_65_1997$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "1997")
scalar_65_2000 <- as.data.frame(x_65_2000$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "2000")
scalar_65_2002 <- as.data.frame(x_65_2002$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "2002")
scalar_65_2004 <- as.data.frame(x_65_2004$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "2004")
scalar_65_2006 <- as.data.frame(x_65_2006$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "2006")
scalar_65_2008 <- as.data.frame(x_65_2008$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "2008")
scalar_65_2010 <- as.data.frame(x_65_2010$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "2010")
scalar_65_2012 <- as.data.frame(x_65_2012$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "2012")
scalar_65_2014 <- as.data.frame(x_65_2014$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "2014")
scalar_65_2019 <- as.data.frame(x_65_2019$df_scalar)%>%
  mutate(Severity = "65")%>%
  mutate(Scenario = "2019")

##################Turn the scalar element into dataframe (This element includes the Rh variables) for all 85 Severity objects ######################
scalar_85_2016 <- as.data.frame(x_85_2016$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "2016")
scalar_85_1979 <- as.data.frame(x_85_1979$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "1979")
scalar_85_1981 <- as.data.frame(x_85_1981$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "1981")
scalar_85_1983 <- as.data.frame(x_85_1983$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "1983")
scalar_85_1985 <- as.data.frame(x_85_1985$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "1985")
scalar_85_1987 <- as.data.frame(x_85_1987$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "1987")
scalar_85_1989 <- as.data.frame(x_85_1989$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "1989")
scalar_85_1991 <- as.data.frame(x_85_1991$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "1991")
scalar_85_1985 <- as.data.frame(x_85_1985$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "1985")
scalar_85_1987 <- as.data.frame(x_85_1987$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "1987")
scalar_85_1989 <- as.data.frame(x_85_1989$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "1989")
scalar_85_1991 <- as.data.frame(x_85_1991$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "1991")
scalar_85_1993 <- as.data.frame(x_85_1993$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "1993")
scalar_85_1995 <- as.data.frame(x_85_1995$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "1995")
scalar_85_1997 <- as.data.frame(x_85_1997$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "1997")
scalar_85_2000 <- as.data.frame(x_85_2000$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "2000")
scalar_85_2002 <- as.data.frame(x_85_2002$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "2002")
scalar_85_2004 <- as.data.frame(x_85_2004$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "2004")
scalar_85_2006 <- as.data.frame(x_85_2006$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "2006")
scalar_85_2008 <- as.data.frame(x_85_2008$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "2008")
scalar_85_2010 <- as.data.frame(x_85_2010$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "2010")
scalar_85_2012 <- as.data.frame(x_85_2012$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "2012")
scalar_85_2014 <- as.data.frame(x_85_2014$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "2014")
scalar_85_2019 <- as.data.frame(x_85_2019$df_scalar)%>%
  mutate(Severity = "85")%>%
  mutate(Scenario = "2019")


##################Merge all the dataframes together (yikes!!) ########
scalar_all <- rbind(scalar_0_2016,scalar_45_2016,scalar_65_2016,scalar_85_2016,
                    scalar_0_1979,scalar_45_1979,scalar_65_1979,scalar_85_1979,
                    scalar_0_1981,scalar_45_1981,scalar_65_1981,scalar_85_1981,
                    scalar_0_1983,scalar_45_1983,scalar_65_1983,scalar_85_1983,
                    scalar_0_1985,scalar_45_1985,scalar_65_1985,scalar_85_1985,
                    scalar_0_1987,scalar_45_1987,scalar_65_1987,scalar_85_1987,
                    scalar_0_1989,scalar_45_1989,scalar_65_1989,scalar_85_1989,
                    scalar_0_1991,scalar_45_1991,scalar_65_1991,scalar_85_1991,
                    scalar_0_1993,scalar_45_1993,scalar_65_1993,scalar_85_1993,
                    scalar_0_1995,scalar_45_1995,scalar_65_1995,scalar_85_1995,
                    scalar_0_1997,scalar_45_1997,scalar_65_1997,scalar_85_1997,
                    scalar_0_2000,scalar_45_2000,scalar_65_2000,scalar_85_2000,
                    scalar_0_2002,scalar_45_2002,scalar_65_2002,scalar_85_2002,
                    scalar_0_2004,scalar_45_2004,scalar_65_2004,scalar_85_2004,
                    scalar_0_2006,scalar_45_2006,scalar_65_2006,scalar_85_2006,
                    scalar_0_2008,scalar_45_2008,scalar_65_2008,scalar_85_2008,
                    scalar_0_2010,scalar_45_2010,scalar_65_2010,scalar_85_2010,
                    scalar_0_2012,scalar_45_2012,scalar_65_2012,scalar_85_2012,
                    scalar_0_2014,scalar_45_2014,scalar_65_2014,scalar_85_2014,
                    scalar_0_2019,scalar_45_2019,scalar_65_2019,scalar_85_2019
)

#########Select only the date, location, Rh variables and severity columns #####
scalar_all <- scalar_all%>%
  select(datetime, LONGITUDE, LATITUDE, MMEAN_RH_PY,MMEAN_FGC_RH_PY,MMEAN_FSC_RH_PY,MMEAN_STGC_RH_PY,MMEAN_STSC_RH_PY,MMEAN_MSC_RH_PY,MMEAN_SSC_RH_PY,MMEAN_PSC_RH_PY, Severity, Scenario)


####Subset the datetime variable to only include 2019-2022

scalar_all_4yr <- scalar_all%>%
  filter(datetime >= "2018-12-01", datetime <= "2022-12-12")

scalar_all_4yr$year <- format(as.POSIXct(scalar_all_4yr$datetime), format = "%Y")
scalar_all$year <- format(as.POSIXct(scalar_all$datetime), format = "%Y")


####Calculate annual CWD in each severity
scalar_all_4yr_CWD <- scalar_all_4yr%>%
  select(datetime, Severity, Scenario, MMEAN_STGC_RH_PY, MMEAN_STSC_RH_PY, year)%>% ###select just the two structural respiration values = BOTH SOIL AND GROUND woody debris
  mutate(CWD_sum = MMEAN_STGC_RH_PY + MMEAN_STSC_RH_PY)%>% ###Add those two values together 
  mutate(CWD_MgChayr = CWD_sum*0.001/0.0001)###scale from kg C m2 yr -> Mg C ha yr 



###Summarize CWD values across the monthly values and across climate scenarios and create a standard error 
scalar_all_4yr_CWD_summary <- scalar_all_4yr_CWD%>%
  group_by(Severity, year)%>%
  summarize(CWD_MgChayr_mean = mean(CWD_MgChayr), std_errorCWD = std.error(CWD_MgChayr))




##plots the CWD values 
ggplot(scalar_all_4yr_CWD, aes(x = year, y = CWD_MgChayr, fill = Severity)) +
  geom_boxplot()+
  facet_wrap(~Severity)

  
