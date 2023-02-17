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


##Importing the new DBH readings directly from the pipline df increment dataframe from the NPP_pipline.R file

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


###Add canopy height data from fortedata 
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

##Scale up to subplot 
dendro_data_CWD_subplot <- dendro_data_dead_unique%>%
  group_by(subplot_id, rep_id, year)%>%
  summarize(C_mass_Mg_subplot = sum(C_mass_Mg))%>%##Scale to subplot
  mutate(decay_class = 1)




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

#######CWD Flux Figures 
forte_pal <- forte_colors()

####By Severity and decay_class 


ggplot(CWD_all_years, aes(x = year, y = Mass_loss_Mg_ha, fill = severity)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = forte_pal)+
  facet_wrap(~decay_class) +
  ylab("Carbon Mass Loss (Mg ha-1)")+
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30))

ggsave(path = "Figures_Output", filename = "Mass_Loss_severity.png", height = 20, width =30, units = "in")

##Figure of just decay class one
CWD_all_years_1 <-  CWD_all_years%>%
  filter(decay_class == "1")

ggplot(CWD_all_years_1, aes(x = year, y = Mass_loss_Mg_ha, fill = severity)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = forte_pal)+
  ylab("Carbon Mass Loss (Mg ha-1)")+
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30))

####By Replicate 
ggplot(CWD_all_years, aes(x = year, y = Mass_loss_Mg_ha, fill = rep_id)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("#99E2E1", "#332752", "#CB3074", "#F7C667"))+
  facet_wrap(~decay_class)+
  ylab("Carbon Mass Loss (Mg ha-1)") +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30))

ggsave(path = "Figures_Output", filename = "Mass_Loss_replicate.png", height = 20, width =30, units = "in")



############################################### Mass Loss, species-specific decay constant method #######################################################
#########################################################################################################################################################

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
 
 ##Select neccessary columns from the previous year 
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
 

 ###Figures 
 
 ####By Severity and decay_class 
 
 ggplot(CWD_all_years_species, aes(x = year, y = Mass_loss_Mg_ha, fill = severity)) +
   geom_boxplot() +
   theme_bw() +
   scale_fill_manual(values = forte_pal)+
   facet_wrap(~decay_class) +
   ylab("Carbon Mass Loss (Mg ha-1)")+
   theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30))
 
 ggsave(path = "Figures_Output", filename = "Mass_Loss_severity.png", height = 20, width =30, units = "in")
 
 ##Figure of just decay class one
 CWD_all_years_species_1 <-  CWD_all_years_species%>%
   filter(decay_class == "1")
 
 ggplot(CWD_all_years_species_1, aes(x = year, y = Mass_loss_Mg_ha, fill = severity)) +
   geom_boxplot() +
   theme_bw() +
   scale_fill_manual(values = forte_pal)+
   ylab("Carbon Mass Loss (Mg ha-1)")+
   theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30))
 
 
 ggsave(path = "Figures_Output", filename = "Mass_Loss_severity.png", height = 20, width =30, units = "in")
 
 
 ####By Replicate 
 ggplot(CWD_all_years, aes(x = year, y = Mass_loss_Mg_ha, fill = rep_id)) +
   geom_boxplot() +
   theme_bw() +
   scale_fill_manual(values = c("#99E2E1", "#332752", "#CB3074", "#F7C667"))+
   facet_wrap(~decay_class)+
   ylab("Carbon Mass Loss (Mg ha-1)") +
   theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30))
   
 
 

#############################Tower Temperature and Moisture CWD flux estimate method ####################################################################
#########################################################################################################################################################

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

Tower_T_6hour_2019 <- Tower_T_6hour_2019 %>% 
  left_join(point_2019_mean_temp_6, by = "Timestamp_6")


###Run regression analyses for the relationship between tower and point measurements
Tower_T_6hour_2019_regression <- Tower_T_6hour_2019%>%
  filter(!is.na(ave_point_temp))

###Run Regression to look at summary statistics (All models are highly significant)
library(broom)

fitted_model_temp_6 <-  Tower_T_6hour_2019_regression%>%
  nest_by(Subplot_ID)%>%
  mutate(model = list(lm(ave_point_temp ~ tower_temp, data = data)))%>%
  summarize(tidy(model))

###Organize model coefficients into a dataframe 
library(data.table)

Tower_T_6hour_2019_regression <- data.table(Tower_T_6hour_2019_regression)

Tower_T_6hour_2019_regression <- Tower_T_6hour_2019_regression[,as.list(coef(lm(ave_point_temp ~ tower_temp))), by=Subplot_ID]

Tower_T_6hour_2019_regression <- as.data.frame.matrix(Tower_T_6hour_2019_regression)


###Apply subplot specific regression models to model hourly temperature in each subplot for 2019 (temp(modeled) = m(tower_temp) + b)
Tower_T_6hour_2019_modeled <- Tower_T_6hour_2019%>%
  mutate(D2e_modeled = Tower_T_6hour_2019_regression[1,3]*tower_temp + Tower_T_6hour_2019_regression[1,2])%>%
  mutate(D2w_modeled = Tower_T_6hour_2019_regression[2,3]*tower_temp + Tower_T_6hour_2019_regression[2,2])%>%
  mutate(D3e_modeled = Tower_T_6hour_2019_regression[3,3]*tower_temp + Tower_T_6hour_2019_regression[3,2])%>%
  mutate(D3w_modeled = Tower_T_6hour_2019_regression[4,3]*tower_temp + Tower_T_6hour_2019_regression[4,2])%>%
  mutate(D4e_modeled = Tower_T_6hour_2019_regression[5,3]*tower_temp + Tower_T_6hour_2019_regression[5,2])%>%
  mutate(D4w_modeled = Tower_T_6hour_2019_regression[6,3]*tower_temp + Tower_T_6hour_2019_regression[6,2])%>%
  mutate(C1e_modeled = Tower_T_6hour_2019_regression[7,3]*tower_temp + Tower_T_6hour_2019_regression[7,2])%>%
  mutate(C1w_modeled = Tower_T_6hour_2019_regression[8,3]*tower_temp + Tower_T_6hour_2019_regression[8,2])%>%
  mutate(C2e_modeled = Tower_T_6hour_2019_regression[9,3]*tower_temp + Tower_T_6hour_2019_regression[9,2])%>%
  mutate(C2w_modeled = Tower_T_6hour_2019_regression[10,3]*tower_temp + Tower_T_6hour_2019_regression[10,2])%>%
  mutate(C3e_modeled = Tower_T_6hour_2019_regression[11,3]*tower_temp + Tower_T_6hour_2019_regression[11,2])%>%
  mutate(C3w_modeled = Tower_T_6hour_2019_regression[12,3]*tower_temp + Tower_T_6hour_2019_regression[12,2])%>%
  mutate(C4e_modeled = Tower_T_6hour_2019_regression[13,3]*tower_temp + Tower_T_6hour_2019_regression[13,2])%>%
  mutate(C4w_modeled = Tower_T_6hour_2019_regression[14,3]*tower_temp + Tower_T_6hour_2019_regression[14,2])%>%
  mutate(D1e_modeled = Tower_T_6hour_2019_regression[15,3]*tower_temp + Tower_T_6hour_2019_regression[15,2])%>%
  mutate(D1w_modeled = Tower_T_6hour_2019_regression[16,3]*tower_temp + Tower_T_6hour_2019_regression[16,2])%>%
  mutate(B1e_modeled = Tower_T_6hour_2019_regression[17,3]*tower_temp + Tower_T_6hour_2019_regression[17,2])%>%
  mutate(B1w_modeled = Tower_T_6hour_2019_regression[18,3]*tower_temp + Tower_T_6hour_2019_regression[18,2])%>%
  mutate(B2e_modeled = Tower_T_6hour_2019_regression[19,3]*tower_temp + Tower_T_6hour_2019_regression[19,2])%>%
  mutate(B2w_modeled = Tower_T_6hour_2019_regression[20,3]*tower_temp + Tower_T_6hour_2019_regression[20,2])%>%
  mutate(B3e_modeled = Tower_T_6hour_2019_regression[21,3]*tower_temp + Tower_T_6hour_2019_regression[21,2])%>%
  mutate(B3w_modeled = Tower_T_6hour_2019_regression[22,3]*tower_temp + Tower_T_6hour_2019_regression[22,2])%>%
  mutate(B4e_modeled = Tower_T_6hour_2019_regression[23,3]*tower_temp + Tower_T_6hour_2019_regression[23,2])%>%
  mutate(B4w_modeled = Tower_T_6hour_2019_regression[24,3]*tower_temp + Tower_T_6hour_2019_regression[24,2])%>%
  mutate(A1e_modeled = Tower_T_6hour_2019_regression[25,3]*tower_temp + Tower_T_6hour_2019_regression[25,2])%>%
  mutate(A1w_modeled = Tower_T_6hour_2019_regression[26,3]*tower_temp + Tower_T_6hour_2019_regression[26,2])%>%
  mutate(A2e_modeled = Tower_T_6hour_2019_regression[27,3]*tower_temp + Tower_T_6hour_2019_regression[27,2])%>%
  mutate(A2ww_modeled = Tower_T_6hour_2019_regression[28,3]*tower_temp + Tower_T_6hour_2019_regression[28,2])%>%
  mutate(A3e_modeled = Tower_T_6hour_2019_regression[29,3]*tower_temp + Tower_T_6hour_2019_regression[29,2])%>%
  mutate(A3w_modeled = Tower_T_6hour_2019_regression[30,3]*tower_temp + Tower_T_6hour_2019_regression[30,2])%>%
  mutate(A4e_modeled = Tower_T_6hour_2019_regression[31,3]*tower_temp + Tower_T_6hour_2019_regression[31,2])%>%
  mutate(A4w_modeled = Tower_T_6hour_2019_regression[32,3]*tower_temp + Tower_T_6hour_2019_regression[32,2])

##Convert dataframe back to long for conveience 
Tower_T_6hour_2019_modeled <- Tower_T_6hour_2019_modeled%>%
  select(!Subplot_ID)%>%
  select(!date)%>%
  select(!ave_point_temp)

Tower_T_6hour_2019_modeled <- gather(Tower_T_6hour_2019_modeled, Subplot_ID, modeled_temp, D2e_modeled:A4w_modeled, factor_key = TRUE)

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


######################## Model SWC from tower measurements (same as temperature pipeline) ####

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

Tower_SWC_6hour_2019 <- Tower_SWC_6hour_2019 %>% 
  left_join(point_2019_mean_VWC_6, by = "Timestamp_6")

###Run regression analyses for the relationship between tower and point measurements (SWC)

Tower_SWC_6hour_2019_regression <- Tower_SWC_6hour_2019%>%
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
  mutate(D2e_modeled = Tower_SWC_6hour_2019_regression[1,3]*tower_SWC +Tower_SWC_6hour_2019_regression[1,2])%>%
  mutate(D2w_modeled = Tower_SWC_6hour_2019_regression[2,3]*tower_SWC + Tower_SWC_6hour_2019_regression[2,2])%>%
  mutate(D3e_modeled = Tower_SWC_6hour_2019_regression[3,3]*tower_SWC + Tower_SWC_6hour_2019_regression[3,2])%>%
  mutate(D3w_modeled = Tower_SWC_6hour_2019_regression[4,3]*tower_SWC + Tower_SWC_6hour_2019_regression[4,2])%>%
  mutate(D4e_modeled = Tower_SWC_6hour_2019_regression[5,3]*tower_SWC + Tower_SWC_6hour_2019_regression[5,2])%>%
  mutate(D4w_modeled = Tower_SWC_6hour_2019_regression[6,3]*tower_SWC + Tower_SWC_6hour_2019_regression[6,2])%>%
  mutate(C1e_modeled = Tower_SWC_6hour_2019_regression[7,3]*tower_SWC + Tower_SWC_6hour_2019_regression[7,2])%>%
  mutate(C1w_modeled = Tower_SWC_6hour_2019_regression[8,3]*tower_SWC + Tower_SWC_6hour_2019_regression[8,2])%>%
  mutate(C2e_modeled = Tower_SWC_6hour_2019_regression[9,3]*tower_SWC + Tower_SWC_6hour_2019_regression[9,2])%>%
  mutate(C2w_modeled = Tower_SWC_6hour_2019_regression[10,3]*tower_SWC + Tower_SWC_6hour_2019_regression[10,2])%>%
  mutate(C3e_modeled = Tower_SWC_6hour_2019_regression[11,3]*tower_SWC + Tower_SWC_6hour_2019_regression[11,2])%>%
  mutate(C3w_modeled = Tower_SWC_6hour_2019_regression[12,3]*tower_SWC + Tower_SWC_6hour_2019_regression[12,2])%>%
  mutate(C4e_modeled = Tower_SWC_6hour_2019_regression[13,3]*tower_SWC + Tower_SWC_6hour_2019_regression[13,2])%>%
  mutate(C4w_modeled = Tower_SWC_6hour_2019_regression[14,3]*tower_SWC + Tower_SWC_6hour_2019_regression[14,2])%>%
  mutate(D1e_modeled = Tower_SWC_6hour_2019_regression[15,3]*tower_SWC + Tower_SWC_6hour_2019_regression[15,2])%>%
  mutate(D1w_modeled = Tower_SWC_6hour_2019_regression[16,3]*tower_SWC + Tower_SWC_6hour_2019_regression[16,2])%>%
  mutate(B1e_modeled = Tower_SWC_6hour_2019_regression[17,3]*tower_SWC + Tower_SWC_6hour_2019_regression[17,2])%>%
  mutate(B1w_modeled = Tower_SWC_6hour_2019_regression[18,3]*tower_SWC + Tower_SWC_6hour_2019_regression[18,2])%>%
  mutate(B2e_modeled = Tower_SWC_6hour_2019_regression[19,3]*tower_SWC + Tower_SWC_6hour_2019_regression[19,2])%>%
  mutate(B2w_modeled = Tower_SWC_6hour_2019_regression[20,3]*tower_SWC + Tower_SWC_6hour_2019_regression[20,2])%>%
  mutate(B3e_modeled = Tower_SWC_6hour_2019_regression[21,3]*tower_SWC + Tower_SWC_6hour_2019_regression[21,2])%>%
  mutate(B3w_modeled = Tower_SWC_6hour_2019_regression[22,3]*tower_SWC + Tower_SWC_6hour_2019_regression[22,2])%>%
  mutate(B4e_modeled = Tower_SWC_6hour_2019_regression[23,3]*tower_SWC + Tower_SWC_6hour_2019_regression[23,2])%>%
  mutate(B4w_modeled = Tower_SWC_6hour_2019_regression[24,3]*tower_SWC + Tower_SWC_6hour_2019_regression[24,2])%>%
  mutate(A1e_modeled = Tower_SWC_6hour_2019_regression[25,3]*tower_SWC + Tower_SWC_6hour_2019_regression[25,2])%>%
  mutate(A1w_modeled = Tower_SWC_6hour_2019_regression[26,3]*tower_SWC + Tower_SWC_6hour_2019_regression[26,2])%>%
  mutate(A2e_modeled = Tower_SWC_6hour_2019_regression[27,3]*tower_SWC + Tower_SWC_6hour_2019_regression[27,2])%>%
  mutate(A2w_modeled = Tower_SWC_6hour_2019_regression[28,3]*tower_SWC + Tower_SWC_6hour_2019_regression[28,2])%>%
  mutate(A3e_modeled = Tower_SWC_6hour_2019_regression[29,3]*tower_SWC + Tower_SWC_6hour_2019_regression[29,2])%>%
  mutate(A3w_modeled = Tower_SWC_6hour_2019_regression[30,3]*tower_SWC + Tower_SWC_6hour_2019_regression[30,2])%>%
  mutate(A4e_modeled = Tower_SWC_6hour_2019_regression[31,3]*tower_SWC + Tower_SWC_6hour_2019_regression[31,2])%>%
  mutate(A4w_modeled = Tower_SWC_6hour_2019_regression[32,3]*tower_SWC + Tower_SWC_6hour_2019_regression[32,2])


  ##Convert dataframe back to long for conveience 
  Tower_SWC_6hour_2019_modeled <- Tower_SWC_6hour_2019_modeled%>%
    select(!Subplot_ID)%>%
    select(!date)%>%
    select(!ave_point_SWC)
  
  Tower_SWC_6hour_2019_modeled <- gather(Tower_SWC_6hour_2019_modeled, Subplot_ID, modeled_SWC, D2e_modeled:A4w_modeled, factor_key = TRUE)
  
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
  
  ##Rename the tower variables and select only the 2019 Timestamps 
  
  
  Tower_T_6hour_2021 <- Tower_T_6hour%>%
    filter(between(Timestamp_6, as.POSIXct('2021-01-01 00:00:00'), as.POSIXct('2021-12-31 18:00:00')))
  
  
  ###Join Tower and point data for 2019 
  
  Tower_T_6hour_2021 <- Tower_T_6hour_2021 %>% 
    left_join(point_2021_mean_temp_6, by = "Timestamp_6")
  
  
  ###Run regression analyses for the relationship between tower and point measurements
  Tower_T_6hour_2021_regression <- Tower_T_6hour_2021%>%
    filter(!is.na(ave_soilTemp))
  
  ###Run Regression to look at summary statistics (All models are highly significant)
  library(broom)
  
  fitted_model_temp_6_2021 <-  Tower_T_6hour_2021_regression%>%
    nest_by(Subplot_ID)%>%
    mutate(model = list(lm(ave_soilTemp ~ tower_temp, data = data)))%>%
    summarize(tidy(model))
  
  ###Organize model coefficients into a dataframe 
  library(data.table)
  
  Tower_T_6hour_2021_regression <- data.table(Tower_T_6hour_2021_regression)
  
  Tower_T_6hour_2021_regression <- Tower_T_6hour_2021_regression[,as.list(coef(lm(ave_soilTemp ~ tower_temp))), by=Subplot_ID]
  
  Tower_T_6hour_2021_regression <- as.data.frame.matrix(Tower_T_6hour_2021_regression)
  
  
  ###Apply subplot specific regression models to model hourly temperature in each subplot for 2019 (temp(modeled) = m(tower_temp) + b)
  Tower_T_6hour_2019_modeled <- Tower_T_6hour_2019%>%
    mutate(D2e_modeled = Tower_T_6hour_2019_regression[1,3]*tower_temp + Tower_T_6hour_2019_regression[1,2])%>%
    mutate(D2w_modeled = Tower_T_6hour_2019_regression[2,3]*tower_temp + Tower_T_6hour_2019_regression[2,2])%>%
    mutate(D3e_modeled = Tower_T_6hour_2019_regression[3,3]*tower_temp + Tower_T_6hour_2019_regression[3,2])%>%
    mutate(D3w_modeled = Tower_T_6hour_2019_regression[4,3]*tower_temp + Tower_T_6hour_2019_regression[4,2])%>%
    mutate(D4e_modeled = Tower_T_6hour_2019_regression[5,3]*tower_temp + Tower_T_6hour_2019_regression[5,2])%>%
    mutate(D4w_modeled = Tower_T_6hour_2019_regression[6,3]*tower_temp + Tower_T_6hour_2019_regression[6,2])%>%
    mutate(C1e_modeled = Tower_T_6hour_2019_regression[7,3]*tower_temp + Tower_T_6hour_2019_regression[7,2])%>%
    mutate(C1w_modeled = Tower_T_6hour_2019_regression[8,3]*tower_temp + Tower_T_6hour_2019_regression[8,2])%>%
    mutate(C2e_modeled = Tower_T_6hour_2019_regression[9,3]*tower_temp + Tower_T_6hour_2019_regression[9,2])%>%
    mutate(C2w_modeled = Tower_T_6hour_2019_regression[10,3]*tower_temp + Tower_T_6hour_2019_regression[10,2])%>%
    mutate(C3e_modeled = Tower_T_6hour_2019_regression[11,3]*tower_temp + Tower_T_6hour_2019_regression[11,2])%>%
    mutate(C3w_modeled = Tower_T_6hour_2019_regression[12,3]*tower_temp + Tower_T_6hour_2019_regression[12,2])%>%
    mutate(C4e_modeled = Tower_T_6hour_2019_regression[13,3]*tower_temp + Tower_T_6hour_2019_regression[13,2])%>%
    mutate(C4w_modeled = Tower_T_6hour_2019_regression[14,3]*tower_temp + Tower_T_6hour_2019_regression[14,2])%>%
    mutate(D1e_modeled = Tower_T_6hour_2019_regression[15,3]*tower_temp + Tower_T_6hour_2019_regression[15,2])%>%
    mutate(D1w_modeled = Tower_T_6hour_2019_regression[16,3]*tower_temp + Tower_T_6hour_2019_regression[16,2])%>%
    mutate(B1e_modeled = Tower_T_6hour_2019_regression[17,3]*tower_temp + Tower_T_6hour_2019_regression[17,2])%>%
    mutate(B1w_modeled = Tower_T_6hour_2019_regression[18,3]*tower_temp + Tower_T_6hour_2019_regression[18,2])%>%
    mutate(B2e_modeled = Tower_T_6hour_2019_regression[19,3]*tower_temp + Tower_T_6hour_2019_regression[19,2])%>%
    mutate(B2w_modeled = Tower_T_6hour_2019_regression[20,3]*tower_temp + Tower_T_6hour_2019_regression[20,2])%>%
    mutate(B3e_modeled = Tower_T_6hour_2019_regression[21,3]*tower_temp + Tower_T_6hour_2019_regression[21,2])%>%
    mutate(B3w_modeled = Tower_T_6hour_2019_regression[22,3]*tower_temp + Tower_T_6hour_2019_regression[22,2])%>%
    mutate(B4e_modeled = Tower_T_6hour_2019_regression[23,3]*tower_temp + Tower_T_6hour_2019_regression[23,2])%>%
    mutate(B4w_modeled = Tower_T_6hour_2019_regression[24,3]*tower_temp + Tower_T_6hour_2019_regression[24,2])%>%
    mutate(A1e_modeled = Tower_T_6hour_2019_regression[25,3]*tower_temp + Tower_T_6hour_2019_regression[25,2])%>%
    mutate(A1w_modeled = Tower_T_6hour_2019_regression[26,3]*tower_temp + Tower_T_6hour_2019_regression[26,2])%>%
    mutate(A2e_modeled = Tower_T_6hour_2019_regression[27,3]*tower_temp + Tower_T_6hour_2019_regression[27,2])%>%
    mutate(A2ww_modeled = Tower_T_6hour_2019_regression[28,3]*tower_temp + Tower_T_6hour_2019_regression[28,2])%>%
    mutate(A3e_modeled = Tower_T_6hour_2019_regression[29,3]*tower_temp + Tower_T_6hour_2019_regression[29,2])%>%
    mutate(A3w_modeled = Tower_T_6hour_2019_regression[30,3]*tower_temp + Tower_T_6hour_2019_regression[30,2])%>%
    mutate(A4e_modeled = Tower_T_6hour_2019_regression[31,3]*tower_temp + Tower_T_6hour_2019_regression[31,2])%>%
    mutate(A4w_modeled = Tower_T_6hour_2019_regression[32,3]*tower_temp + Tower_T_6hour_2019_regression[32,2])
  
  ##Convert dataframe back to long for conveience 
  Tower_T_6hour_2019_modeled <- Tower_T_6hour_2019_modeled%>%
    select(!Subplot_ID)%>%
    select(!date)%>%
    select(!ave_point_temp)
  
  Tower_T_6hour_2019_modeled <- gather(Tower_T_6hour_2019_modeled, Subplot_ID, modeled_temp, D2e_modeled:A4w_modeled, factor_key = TRUE)
  
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
  
  
  
