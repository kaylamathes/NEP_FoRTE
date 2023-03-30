###### FoRTE Soil Rh Measurements and estimates (multiple methods) ##########################
##### 2019-2022 ########
##### Kayla C. Mathes 

############# Extract annual modeled Rh values from manuscript. These values were averaged over the 20 climate scenarios and plotted for the four years of measurement across the disturbance severity gradient. 

###Libraries
library(dplyr)
library(plotrix)
library(fortedata)
library(ggplot2)
library(tidyr)
library(reshape2)
library(googledrive)
library(stringr)
library(broom)


####### Data from manuscript 
ED_output <- read.csv("Data_CWD/ED-outputs/results/exp-constant-yr.csv", na.strings = c("", "NA")) 

ED_output_month <- read.csv("Data_CWD/ED-outputs/results/exp-constant-mon.csv", na.strings = c("", "NA")) 


##Extracting just the Rh data 
ED_Rh <- ED_output%>%
  filter(variable == "Rh")

ED_Rh_month <- ED_output_month%>%
  filter(variable == "Rh")

ED_Rh <- ED_Rh%>%
 filter(year >= 2019)%>%
  filter(year <= 2022)

ED_Rh_2019 <- ED_Rh%>%
  filter(year == "2019")

ED_Rh_month$datetime <- as.POSIXct(ED_Rh_month$datetime)

ED_Rh_month <- ED_Rh_month%>%
  filter(between(datetime, as.POSIXct("2019-01-01"), as.POSIXct("2022-12-01")))

ED_Rh_summary <- ED_Rh%>%
  group_by(scn, year)%>%
  summarize(ave_Rh = mean(value), std_err = std.error(value))

write.csv(ED_Rh_summary, "Figures_Output\\Model_Rh_means.csv", row.names=FALSE)

forte_pal <- forte_colors()

ggplot(ED_Rh, aes(x = scn, y = value, fill = scn))+
  geom_boxplot()+
  scale_fill_manual(values = forte_pal)+
  facet_wrap(~ year)


################### Calculate continous Rs #######################

Rs <- read.csv("Data_Rh/Rs_Q10_dataframe.csv")
Rs_2022 <- read.csv("googledrive_data/Rs_2022.csv")

###Combine plot level info into one subplot column for 2022
Rs_2022$Subplot_ID <- str_c(Rs_2022$Rep_ID, '',Rs_2022$Plot_ID,'', Rs_2022$Subplot)

####Create severity and treatment columns and 
Rs_2022_summary <- Rs_2022%>%
  mutate(subplot_id = case_when(Subplot_ID == "A1e" ~ "A01E", 
                                Subplot_ID == "A1w" ~ "A01W", 
                                Subplot_ID == "A2e" ~ "A02E", 
                                Subplot_ID == "A2w" ~ "A02W", 
                                Subplot_ID == "A3e" ~ "A03E", 
                                Subplot_ID == "A3w" ~ "A03W", 
                                Subplot_ID == "A4e" ~ "A04E", 
                                Subplot_ID == "A4w" ~ "A04W", 
                                Subplot_ID == "B1e" ~ "B01E", 
                                Subplot_ID == "B1w" ~ "B01W", 
                                Subplot_ID == "B2e" ~ "B02E", 
                                Subplot_ID == "B2w" ~ "B02W", 
                                Subplot_ID == "B3e" ~ "B03E", 
                                Subplot_ID == "B3w" ~ "B03W", 
                                Subplot_ID == "B4e" ~ "B04E", 
                                Subplot_ID == "B4w" ~ "B04W", 
                                Subplot_ID == "C1e" ~ "C01E", 
                                Subplot_ID == "C1w" ~ "C01W", 
                                Subplot_ID == "C2e" ~ "C02E", 
                                Subplot_ID == "C2w" ~ "C02W", 
                                Subplot_ID == "C3e" ~ "C03E", 
                                Subplot_ID == "C3w" ~ "C03W", 
                                Subplot_ID == "C4e" ~ "C04E", 
                                Subplot_ID == "C4w" ~ "C04W", 
                                Subplot_ID == "D1e" ~ "D01E", 
                                Subplot_ID == "D1w" ~ "D01W", 
                                Subplot_ID == "D2e" ~ "D02E", 
                                Subplot_ID == "D2w" ~ "D02W", 
                                Subplot_ID == "D3e" ~ "D03E", 
                                Subplot_ID == "D3w" ~ "D03W", 
                                Subplot_ID == "D4e" ~ "D04E", 
                                Subplot_ID == "D4w" ~ "D04W"))%>%
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
  group_by(severity, treatment, subplot_id, Rep_ID, date)%>%
  summarize(ave_soilCO2Efflux = mean(soilCO2Efflux), ave_soilTemp = mean(soilTemp))%>%
  rename(Severity = severity, Treatment = treatment)%>%
  mutate(year = "2022")%>%
  select(Rep_ID, Severity, Treatment, date, year, ave_soilCO2Efflux, ave_soilTemp, subplot_id)


#### Cleaning other Rs document 

Rs_summary <- Rs%>%
  rename(Subplot_ID = Subplot_code)%>%
  mutate(subplot_id = case_when(Subplot_ID == "A1e" ~ "A01E", 
                                Subplot_ID == "A1w" ~ "A01W", 
                                Subplot_ID == "A2e" ~ "A02E", 
                                Subplot_ID == "A2w" ~ "A02W", 
                                Subplot_ID == "A3e" ~ "A03E", 
                                Subplot_ID == "A3w" ~ "A03W", 
                                Subplot_ID == "A4e" ~ "A04E", 
                                Subplot_ID == "A4w" ~ "A04W", 
                                Subplot_ID == "B1e" ~ "B01E", 
                                Subplot_ID == "B1w" ~ "B01W", 
                                Subplot_ID == "B2e" ~ "B02E", 
                                Subplot_ID == "B2w" ~ "B02W", 
                                Subplot_ID == "B3e" ~ "B03E", 
                                Subplot_ID == "B3w" ~ "B03W", 
                                Subplot_ID == "B4e" ~ "B04E", 
                                Subplot_ID == "B4w" ~ "B04W", 
                                Subplot_ID == "C1e" ~ "C01E", 
                                Subplot_ID == "C1w" ~ "C01W", 
                                Subplot_ID == "C2e" ~ "C02E", 
                                Subplot_ID == "C2w" ~ "C02W", 
                                Subplot_ID == "C3e" ~ "C03E", 
                                Subplot_ID == "C3w" ~ "C03W", 
                                Subplot_ID == "C4e" ~ "C04E", 
                                Subplot_ID == "C4w" ~ "C04W", 
                                Subplot_ID == "D1e" ~ "D01E", 
                                Subplot_ID == "D1w" ~ "D01W", 
                                Subplot_ID == "D2e" ~ "D02E", 
                                Subplot_ID == "D2w" ~ "D02W", 
                                Subplot_ID == "D3e" ~ "D03E", 
                                Subplot_ID == "D3w" ~ "D03W", 
                                Subplot_ID == "D4e" ~ "D04E", 
                                Subplot_ID == "D4w" ~ "D04W"))%>%
  select(!Subplot_ID)

###Merging years together 
Rs_summary$Severity <- as.factor(Rs_summary$Severity)
Rs_2022_summary$Severity <- as.factor(Rs_2022_summary$Severity)

Rs_summary$year <- as.factor(Rs_summary$year)
Rs_2022_summary$year <- as.factor(Rs_2022_summary$year)

###Make one total Rs dataframe 
Rs_total_point <- rbind(Rs_2022_summary, Rs_summary)

Rs_total_point$date <- as.POSIXct(Rs_total_point$date)

###Separate Rs dataframe by year 

####2019
Rs_2019 <- Rs_total_point%>%
  filter(year == "2019")

fitted_Rs_temp_2019 <-  Rs_2019%>%
  group_by(Rep_ID, Severity,Treatment, subplot_id)%>%
  do(model = nls(ave_soilCO2Efflux ~ a * exp(b * ave_soilTemp), start = list(a = 0.8,b = 0.1),data = .))%>%
  ungroup()
  
param_model_Q10_2019 <-  fitted_Rs_temp_2019 %>%
  mutate(param_efflux = lapply(model, broom::tidy)) %>%
  unnest(param_efflux) %>%
  select(Rep_ID, Severity,Treatment,subplot_id, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) 


###2020
Rs_2020 <- Rs_total_point%>%
  filter(year == "2020")

fitted_Rs_temp_2020 <-  Rs_2020%>%
  group_by(Rep_ID, Severity,Treatment, subplot_id)%>%
  do(model = nls(ave_soilCO2Efflux ~ a * exp(b * ave_soilTemp), start = list(a = 0.8,b = 0.1),data = .))%>%
  ungroup()

param_model_Q10_2020 <-  fitted_Rs_temp_2020 %>%
  mutate(param_efflux = lapply(model, broom::tidy)) %>%
  unnest(param_efflux) %>%
  select(Rep_ID, Severity,Treatment,subplot_id, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) 


###2021
Rs_2021 <- Rs_total_point%>%
  filter(year == "2021")

fitted_Rs_temp_2021 <-  Rs_2021%>%
  group_by(Rep_ID, Severity,Treatment, subplot_id)%>%
  do(model = nls(ave_soilCO2Efflux ~ a * exp(b * ave_soilTemp), start = list(a = 0.8,b = 0.1),data = .,control = nls.control(maxiter = 1000)))%>%
  ungroup()

param_model_Q10_2021 <-  fitted_Rs_temp_2021 %>%
  mutate(param_efflux = lapply(model, broom::tidy)) %>%
  unnest(param_efflux) %>%
  select(Rep_ID, Severity,Treatment,subplot_id, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) 


###2022
Rs_2022 <- Rs_total_point%>%
  filter(year == "2022")

fitted_Rs_temp_2022 <-  Rs_2022%>%
  group_by(Rep_ID, Severity,Treatment, subplot_id)%>%
  do(model = nls(ave_soilCO2Efflux ~ a * exp(b * ave_soilTemp), start = list(a = 0.8,b = 0.1),data = .))%>%
  ungroup()

param_model_Q10_2022 <-  fitted_Rs_temp_2022 %>%
  mutate(param_efflux = lapply(model, broom::tidy)) %>%
  unnest(param_efflux) %>%
  select(Rep_ID, Severity,Treatment,subplot_id, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) 

########### Calculate continuous Rs #############

#### Load in tower temperature data 

Tower_temp_2019 <- read.csv("modeled_6hr_Ts_2019.csv")
Tower_temp_2020 <- read.csv("modeled_6hr_Ts_2020.csv")
Tower_temp_2021 <- read.csv("modeled_6hr_Ts_2021.csv")
Tower_temp_2022 <- read.csv("modeled_6hr_Ts_2022.csv")

####Turn modeled temperature into daily temperature 

Tower_temp_2019$Timestamp_6 <- as.POSIXct(Tower_temp_2019$Timestamp_6)
Tower_temp_2020$Timestamp_6 <- as.POSIXct(Tower_temp_2020$Timestamp_6)
Tower_temp_2021$Timestamp_6 <- as.POSIXct(Tower_temp_2021$Timestamp_6)
Tower_temp_2022$Timestamp_6 <- as.POSIXct(Tower_temp_2022$Timestamp_6)


Tower_temp_2019$date <- format(as.POSIXct(Tower_temp_2019$Timestamp_6), format = "%Y-%m-%d")

Tower_temp_2019 <- Tower_temp_2019%>%
  group_by(Subplot_ID,date)%>%
  summarize(modeled_temp_day = mean(modeled_temp))%>%
  rename(subplot_id = Subplot_ID)%>%
  na.omit()



Tower_temp_2020$date <- format(as.POSIXct(Tower_temp_2020$Timestamp_6), format = "%Y-%m-%d")

Tower_temp_2020<- Tower_temp_2020%>%
  group_by(Subplot_ID, date)%>%
  summarize(modeled_temp_day = mean(modeled_temp))%>%
 rename(subplot_id = Subplot_ID)%>%
  na.omit()


Tower_temp_2021$date <- format(as.POSIXct(Tower_temp_2021$Timestamp_6), format = "%Y-%m-%d")

Tower_temp_2021<- Tower_temp_2021%>%
  group_by(date, Subplot_ID)%>%
  summarize(modeled_temp_day = mean(modeled_temp))%>%
  rename(subplot_id = Subplot_ID)%>%
  na.omit()%>%
  filter(date != "2021-12-31")

Tower_temp_2022$date <- format(as.POSIXct(Tower_temp_2022$Timestamp_6), format = "%Y-%m-%d")

Tower_temp_2022 <- Tower_temp_2022%>%
  group_by(date, Subplot_ID)%>%
  summarize(modeled_temp_day = mean(modeled_temp))%>%
  rename(subplot_id = Subplot_ID)%>%
  na.omit()%>%
  filter(date != "2022-01-01")%>%
  filter(date != "2022-12-31")

###Join Tower and point data 

Rs_tower_temp_2019 <- Tower_temp_2019 %>% 
  left_join(param_model_Q10_2019, by = "subplot_id")%>%
  mutate(modeled_efflux_umol_m2_s =  a * exp(b * modeled_temp_day))

Rs_tower_temp_2020 <- Tower_temp_2020 %>% 
  left_join(param_model_Q10_2020, by = "subplot_id")%>%
  mutate(modeled_efflux_umol_m2_s =  a * exp(b * modeled_temp_day))

Rs_tower_temp_2021 <- Tower_temp_2021 %>% 
  left_join(param_model_Q10_2021, by = "subplot_id")%>%
  mutate(modeled_efflux_umol_m2_s =  a * exp(b * modeled_temp_day))

Rs_tower_temp_2022 <- Tower_temp_2022 %>% 
  left_join(param_model_Q10_2022, by = "subplot_id")%>%
  mutate(modeled_efflux_umol_m2_s =  a * exp(b * modeled_temp_day))
  

############Scale up to g C m2 y

Rs_tower_temp_2019_year <- Rs_tower_temp_2019%>%
  mutate(modeled_efflux_umol_m2_day = modeled_efflux_umol_m2_s*86400)%>%
  group_by(Severity, Treatment, subplot_id, Rep_ID)%>%
  summarise(modeled_efflux_umol_m2_yr = sum(modeled_efflux_umol_m2_day))%>%
  mutate(modeled_efflux_g_m2_y = modeled_efflux_umol_m2_yr/1000000*12.0107)%>%
  mutate(year = "2019")

Rs_tower_temp_2020_year <- Rs_tower_temp_2020%>%
  mutate(modeled_efflux_umol_m2_day = modeled_efflux_umol_m2_s*86400)%>%
  group_by(Severity,subplot_id, Treatment,  Rep_ID)%>%
  summarise(modeled_efflux_umol_m2_yr = sum(modeled_efflux_umol_m2_day))%>%
  mutate(modeled_efflux_g_m2_y = modeled_efflux_umol_m2_yr/1000000*12.0107)%>%
  mutate(year = "2020")

Rs_tower_temp_2021_year <- Rs_tower_temp_2021%>%
  mutate(modeled_efflux_umol_m2_day = modeled_efflux_umol_m2_s*86400)%>%
  group_by(Severity, subplot_id, Treatment, Rep_ID)%>%
  summarise(modeled_efflux_umol_m2_yr = sum(modeled_efflux_umol_m2_day))%>%
  mutate(modeled_efflux_g_m2_y = modeled_efflux_umol_m2_yr/1000000*12.0107)%>%
  mutate(year = "2021")

Rs_tower_temp_2022_year <- Rs_tower_temp_2022%>%
  mutate(modeled_efflux_umol_m2_day = modeled_efflux_umol_m2_s*86400)%>%
  group_by(Severity, Rep_ID, Treatment, subplot_id)%>%
  summarise(modeled_efflux_umol_m2_yr = sum(modeled_efflux_umol_m2_day))%>%
  mutate(modeled_efflux_g_m2_y = modeled_efflux_umol_m2_yr/1000000*12.0107)%>%
  mutate(year = "2022")

Rs_tower_temp_year_all <- rbind(Rs_tower_temp_2019_year, Rs_tower_temp_2020_year,Rs_tower_temp_2021_year,Rs_tower_temp_2022_year)%>%
  ungroup()



################Calculate control Rh from Bond-Lamberty 2004 relationship 

Rs_tower_temp_year_all_0 <- Rs_tower_temp_year_all%>%
  filter(Severity == "0")%>%
  mutate(log_modeled_Rh_g_m2_y_BBL = (1.22 + 0.73* log(modeled_efflux_g_m2_y)))%>%
  mutate(modeled_Rh_g_m2_y_BBL = exp(log_modeled_Rh_g_m2_y_BBL))%>%
  mutate(modeled_Rh_Mg_ha_y_BBL = modeled_Rh_g_m2_y_BBL/0.0001/1000000)%>%
  mutate(modeled_Rs_Mg_ha_y = modeled_efflux_g_m2_y/0.0001/1000000)%>%
  mutate(modeled_Ra_Mg_ha_y_BBL = modeled_Rs_Mg_ha_y - modeled_Rh_Mg_ha_y_BBL)
  
################Calculate control Rh from Subke 2006 relationship 

Rs_tower_temp_year_all_0 <- Rs_tower_temp_year_all_0%>%
  mutate(Rh_Rs_ratio_gC_m2_y_subke = -0.138*log(modeled_efflux_g_m2_y) + 1.482)%>%
  mutate(modeled_Rh_gC_m2_y_subke = Rh_Rs_ratio_gC_m2_y_subke*modeled_efflux_g_m2_y)%>%
  mutate(modeled_Rh_Mg_ha_y_subke = modeled_Rh_gC_m2_y_subke/0.0001/1000000)%>%
  mutate(modeled_Ra_Mg_ha_y_subke = modeled_Rs_Mg_ha_y - modeled_Rh_Mg_ha_y_subke)

######Calculate the slope for 0 severity to 100 severity Ra = 0

####BBL method
modeled_Ra_relationship_BBL <- Rs_tower_temp_year_all_0%>%
  ungroup()%>%
  select(!Severity)%>%
  mutate(slope_BBL = modeled_Ra_Mg_ha_y_BBL/-100)%>%
  mutate("45" = slope_BBL*45 + modeled_Ra_Mg_ha_y_BBL)%>%
  mutate("65" = slope_BBL*65 + modeled_Ra_Mg_ha_y_BBL)%>%
  mutate("85" = slope_BBL*85 + modeled_Ra_Mg_ha_y_BBL)

modeled_Ra_relationship_BBL_severity <-modeled_Ra_relationship_BBL%>%
  select(Rep_ID, Treatment, subplot_id, year,  modeled_Ra_Mg_ha_y_BBL, "45", "65", "85")
  
modeled_Ra_relationship_BBL_severity <-  melt(modeled_Ra_relationship_BBL_severity, id.vars=c("Rep_ID", "Treatment", "subplot_id", "year"))

modeled_Ra_relationship_BBL_severity <- modeled_Ra_relationship_BBL_severity%>%
  mutate(Severity = case_when(variable == "modeled_Ra_Mg_ha_y_BBL" ~ "0", 
                              variable == "45" ~ "45",
                              variable == "65" ~ "65",
                              variable == "85" ~ "85"))%>%
  rename(Ra_MgChayr = value)%>%
  select(!variable)

modeled_Ra_relationship_BBL_severity$Severity <- as.factor(modeled_Ra_relationship_BBL_severity$Severity)

Rs_tower_temp_year_all$Severity <- as.factor(Rs_tower_temp_year_all$Severity )

####Merge the Rs with the modeled Ra values 


modeled_Ra_Rs_relationship_BBL_severity <- merge(modeled_Ra_relationship_BBL_severity,Rs_tower_temp_year_all, by = c("Severity", "year","Treatment","Rep_ID"))%>%
  select(!subplot_id.x)%>%
  rename(subplot_id = subplot_id.y)%>%
  mutate(Rs_MgChayr = modeled_efflux_g_m2_y/0.0001/1000000)%>%
  mutate(Rh_MgChayr =Rs_MgChayr - Ra_MgChayr)


modeled_Ra_Rs_relationship_BBL_severity_summary <- modeled_Ra_Rs_relationship_BBL_severity%>%
  group_by(Severity, year)%>%
  summarize(Rh_MgChayr_ave = mean(Rh_MgChayr), std_error = std.error(Rh_MgChayr))

modeled_Ra_Rs_relationship_BBL_Treatment_summary <- modeled_Ra_Rs_relationship_BBL_severity%>%
  group_by(year, Treatment)%>%
  summarize(Rh_MgChayr_ave = mean(Rh_MgChayr), std_error = std.error(Rh_MgChayr))


####subke method
modeled_Ra_relationship_subke <- Rs_tower_temp_year_all_0%>%
  ungroup()%>%
  select(!Severity)%>%
  mutate(slope_subke = modeled_Ra_Mg_ha_y_subke/-100)%>%
  mutate("45" = slope_subke*45 + modeled_Ra_Mg_ha_y_subke)%>%
  mutate("65" = slope_subke*65 + modeled_Ra_Mg_ha_y_subke)%>%
  mutate("85" = slope_subke*85 + modeled_Ra_Mg_ha_y_subke)

modeled_Ra_relationship_subke_severity <-modeled_Ra_relationship_subke%>%
  select(Rep_ID, Treatment, subplot_id, year,  modeled_Ra_Mg_ha_y_subke, "45", "65", "85")

modeled_Ra_relationship_subke_severity <-  melt(modeled_Ra_relationship_subke_severity, id.vars=c("Rep_ID", "Treatment", "subplot_id", "year"))

modeled_Ra_relationship_subke_severity <- modeled_Ra_relationship_subke_severity%>%
  mutate(Severity = case_when(variable == "modeled_Ra_Mg_ha_y_subke" ~ "0", 
                              variable == "45" ~ "45",
                              variable == "65" ~ "65",
                              variable == "85" ~ "85"))%>%
  rename(Ra_MgChayr = value)%>%
  select(!variable)

modeled_Ra_relationship_subke_severity$Severity <- as.factor(modeled_Ra_relationship_subke_severity$Severity)

Rs_tower_temp_year_all$Severity <- as.factor(Rs_tower_temp_year_all$Severity )

####Merge the Rs with the modeled Ra values 


modeled_Ra_Rs_relationship_subke_severity <- merge(modeled_Ra_relationship_subke_severity,Rs_tower_temp_year_all, by = c("Severity", "year","Treatment","Rep_ID"))%>%
  select(!subplot_id.x)%>%
  rename(subplot_id = subplot_id.y)%>%
  mutate(Rs_MgChayr = modeled_efflux_g_m2_y/0.0001/1000000)%>%
  mutate(Rh_MgChayr =Rs_MgChayr - Ra_MgChayr)


modeled_Ra_Rs_relationship_subke_severity_summary <- modeled_Ra_Rs_relationship_subke_severity%>%
  group_by(Severity, year)%>%
  summarize(Rh_MgChayr_ave = mean(Rh_MgChayr), std_error = std.error(Rh_MgChayr))

modeled_Ra_Rs_relationship_subke_Treatment_summary <- modeled_Ra_Rs_relationship_subke_severity%>%
  group_by(year, Treatment)%>%
  summarize(Rh_MgChayr_ave = mean(Rh_MgChayr), std_error = std.error(Rh_MgChayr))

#################### Bring in Rh jar values ############################################

jar_Rh <- read.csv("jar_Rh.csv")

########Find the ratio between control and severity level Rh levels

jar_Rh <- jar_Rh%>%
  rename(Subplot_ID = Subplot_code)%>%
  mutate(subplot_id = case_when(Subplot_ID == "A1e" ~ "A01E", 
                                Subplot_ID == "A1w" ~ "A01W", 
                                Subplot_ID == "A2e" ~ "A02E", 
                                Subplot_ID == "A2w" ~ "A02W", 
                                Subplot_ID == "A3e" ~ "A03E", 
                                Subplot_ID == "A3w" ~ "A03W", 
                                Subplot_ID == "A4e" ~ "A04E", 
                                Subplot_ID == "A4w" ~ "A04W", 
                                Subplot_ID == "B1e" ~ "B01E", 
                                Subplot_ID == "B1w" ~ "B01W", 
                                Subplot_ID == "B2e" ~ "B02E", 
                                Subplot_ID == "B2w" ~ "B02W", 
                                Subplot_ID == "B3e" ~ "B03E", 
                                Subplot_ID == "B3w" ~ "B03W", 
                                Subplot_ID == "B4e" ~ "B04E", 
                                Subplot_ID == "B4w" ~ "B04W", 
                                Subplot_ID == "C1e" ~ "C01E", 
                                Subplot_ID == "C1w" ~ "C01W", 
                                Subplot_ID == "C2e" ~ "C02E", 
                                Subplot_ID == "C2w" ~ "C02W", 
                                Subplot_ID == "C3e" ~ "C03E", 
                                Subplot_ID == "C3w" ~ "C03W", 
                                Subplot_ID == "C4e" ~ "C04E", 
                                Subplot_ID == "C4w" ~ "C04W", 
                                Subplot_ID == "D1e" ~ "D01E", 
                                Subplot_ID == "D1w" ~ "D01W", 
                                Subplot_ID == "D2e" ~ "D02E", 
                                Subplot_ID == "D2w" ~ "D02W", 
                                Subplot_ID == "D3e" ~ "D03E", 
                                Subplot_ID == "D3w" ~ "D03W", 
                                Subplot_ID == "D4e" ~ "D04E", 
                                Subplot_ID == "D4w" ~ "D04W"))

######### select only the columns we need   
  jar_Rh_wide <- jar_Rh%>%
    select( Rep_ID, year, Treatment, Severity, ave_soilCO2Efflux_umolg)%>%
    ungroup()
 
########## Make dataframe wide  
jar_Rh_wide <- pivot_wider(data = jar_Rh_wide, 
                                 names_from = Severity, values_from = ave_soilCO2Efflux_umolg)



######Creating a control to disturbance ratio across all severities for jar estimates 

jar_Rh_wide <- jar_Rh_wide%>%
  rename(severity_0 = "0", severity_45 = "45", severity_65 = "65", severity_85 = "85")%>%
  mutate(Rh_jar_45_ratio = severity_45/severity_0)%>%
  mutate(Rh_jar_65_ratio = severity_65/severity_0)%>%
  mutate(Rh_jar_85_ratio = severity_85/severity_0)
   

########Take the control Rh estimates from Ben and Subke and create Rh for severities levels based off of ratio from the incutations jar estimates 

jar_estimates <- merge(Rs_tower_temp_year_all_0, jar_Rh_wide, by = c("year", "Treatment", "Rep_ID"))%>%
  select(year, Treatment, Rep_ID, Rh_jar_45_ratio, Rh_jar_65_ratio,Rh_jar_85_ratio, modeled_Rh_Mg_ha_y_BBL,modeled_Rh_Mg_ha_y_subke)%>%
  mutate(Modeled_Rh_Mg_ha_y_BBL_45 = modeled_Rh_Mg_ha_y_BBL*Rh_jar_45_ratio)%>%
  mutate(Modeled_Rh_Mg_ha_y_BBL_65 = modeled_Rh_Mg_ha_y_BBL*Rh_jar_65_ratio)%>%
  mutate(Modeled_Rh_Mg_ha_y_BBL_85 = modeled_Rh_Mg_ha_y_BBL*Rh_jar_85_ratio)%>%
mutate(Modeled_Rh_Mg_ha_y_subke_45 = modeled_Rh_Mg_ha_y_subke*Rh_jar_45_ratio)%>%
  mutate(Modeled_Rh_Mg_ha_y_subke_65 = modeled_Rh_Mg_ha_y_subke*Rh_jar_65_ratio)%>%
  mutate(Modeled_Rh_Mg_ha_y_subke_85 = modeled_Rh_Mg_ha_y_subke*Rh_jar_85_ratio)

####Select only neessary columns and make a BBL method dataframe 
jar_estimates_summary_BBL <- jar_estimates%>%
  select(year, Rep_ID, Treatment,modeled_Rh_Mg_ha_y_BBL, Modeled_Rh_Mg_ha_y_BBL_45,Modeled_Rh_Mg_ha_y_BBL_65,Modeled_Rh_Mg_ha_y_BBL_85)%>%
  rename("0"= modeled_Rh_Mg_ha_y_BBL, "45"= Modeled_Rh_Mg_ha_y_BBL_45, "65"= Modeled_Rh_Mg_ha_y_BBL_65, "85"= Modeled_Rh_Mg_ha_y_BBL_85)

######Transition from long to wide  
jar_estimates_summary_BBL <- jar_estimates_summary_BBL%>%
                pivot_longer(cols=c("0", "45", "65", "85"),
                    names_to='Severity',
                    values_to='Modeled_Rh_Mg_ha_y_BBL')

####Select only neessary columns and make a subke method dataframe 
jar_estimates_summary_subke <- jar_estimates%>%
  select(year, Rep_ID, Treatment,modeled_Rh_Mg_ha_y_subke, Modeled_Rh_Mg_ha_y_subke_45,Modeled_Rh_Mg_ha_y_subke_65,Modeled_Rh_Mg_ha_y_subke_85)%>%
  rename("0"= modeled_Rh_Mg_ha_y_subke, "45"= Modeled_Rh_Mg_ha_y_subke_45, "65"= Modeled_Rh_Mg_ha_y_subke_65, "85"= Modeled_Rh_Mg_ha_y_subke_85)


######Transition from long to wide  
jar_estimates_summary_subke <- jar_estimates_summary_subke%>%
  pivot_longer(cols=c("0", "45", "65", "85"),
               names_to='Severity',
               values_to='Modeled_Rh_Mg_ha_y_subke')

#####Merge methods again 
Jar_estimate_Rh_summary <- merge(jar_estimates_summary_subke, jar_estimates_summary_BBL, by =c ("year", "Rep_ID", "Treatment", "Severity"))


#####Create a severity summary dataframe 
Jar_estimate_Rh_summary_severity <- Jar_estimate_Rh_summary%>%
   group_by(year, Severity)%>%
     summarize(ave_Modeled_Rh_Mg_ha_y_BBL = mean(Modeled_Rh_Mg_ha_y_BBL), se_Modeled_Rh_Mg_ha_y_BBL = std.error(Modeled_Rh_Mg_ha_y_BBL),ave_Modeled_Rh_Mg_ha_y_subke = mean(Modeled_Rh_Mg_ha_y_subke), se_Modeled_Rh_Mg_ha_y_subke = std.error(Modeled_Rh_Mg_ha_y_subke))

write.csv(Jar_estimate_Rh_summary_severity, "Jar_Rh_estimate.csv", row.names = FALSE)

#####Create a Treatment summary dataframe 
Jar_estimate_Rh_summary_treatment <- Jar_estimate_Rh_summary%>%
  group_by(year, Treatment)%>%
  summarize(ave_Modeled_Rh_Mg_ha_y_BBL = mean(Modeled_Rh_Mg_ha_y_BBL), se_Modeled_Rh_Mg_ha_y_BBL = std.error(Modeled_Rh_Mg_ha_y_BBL),ave_Modeled_Rh_Mg_ha_y_subke = mean(Modeled_Rh_Mg_ha_y_subke), se_Modeled_Rh_Mg_ha_y_subke = std.error(Modeled_Rh_Mg_ha_y_subke))



######################method using the ratio of control to severity levels for ED output #########

ED_Rh_summary_wide <- ED_Rh_summary%>%
  select(scn, ave_Rh, year)

ED_Rh_summary_wide <- pivot_wider(data = ED_Rh_summary_wide,
                                  names_from = scn, values_from = ave_Rh)

  
######Creating a control to disturbance ratio across all severities for jar estimates 
  
ED_Rh_summary_wide <- ED_Rh_summary_wide%>%
    mutate(Rh_ED_45_ratio = harvest_45/harvest_0)%>%
    mutate(Rh_ED_65_ratio = harvest_65/harvest_0)%>%
    mutate(Rh_ED_85_ratio = harvest_85/harvest_0)
  
  
########Take the control Rh estimates from Ben and Subke and create Rh for severities levels based off of ratio from the incutations jar estimates 
Rs_tower_temp_year_all_0_ED <- Rs_tower_temp_year_all_0%>%
  select(year, Rep_ID, Treatment, subplot_id, modeled_Rh_Mg_ha_y_BBL, modeled_Rh_Mg_ha_y_subke)

Rs_tower_temp_year_all_0_ED$year <- as.factor(Rs_tower_temp_year_all_0_ED$year)
ED_Rh_summary_wide$year <- as.factor(ED_Rh_summary_wide$year)

ED_Rh_estimates <- Rs_tower_temp_year_all_0_ED%>% 
  left_join(ED_Rh_summary_wide, by = "year")

  
  ED_Rh_estimates <- ED_Rh_estimates%>%
    select(year,Rep_ID, Treatment, subplot_id, Rh_ED_45_ratio, Rh_ED_65_ratio,Rh_ED_85_ratio, modeled_Rh_Mg_ha_y_BBL, modeled_Rh_Mg_ha_y_subke)%>%
    mutate(Modeled_Rh_Mg_ha_y_BBL_45 = modeled_Rh_Mg_ha_y_BBL*Rh_ED_45_ratio)%>%
    mutate(Modeled_Rh_Mg_ha_y_BBL_65 = modeled_Rh_Mg_ha_y_BBL*Rh_ED_65_ratio)%>%
    mutate(Modeled_Rh_Mg_ha_y_BBL_85 = modeled_Rh_Mg_ha_y_BBL*Rh_ED_85_ratio)%>%
    mutate(Modeled_Rh_Mg_ha_y_subke_45 = modeled_Rh_Mg_ha_y_subke*Rh_ED_45_ratio)%>%
    mutate(Modeled_Rh_Mg_ha_y_subke_65 = modeled_Rh_Mg_ha_y_subke*Rh_ED_65_ratio)%>%
    mutate(Modeled_Rh_Mg_ha_y_subke_85 = modeled_Rh_Mg_ha_y_subke*Rh_ED_85_ratio)
  
  
    
  ED_estimates_BBL<-   ED_Rh_estimates%>%
    select(year, subplot_id, Rep_ID, Treatment,modeled_Rh_Mg_ha_y_BBL, Modeled_Rh_Mg_ha_y_BBL_45,Modeled_Rh_Mg_ha_y_BBL_65,Modeled_Rh_Mg_ha_y_BBL_85)%>%
    rename("0"= modeled_Rh_Mg_ha_y_BBL, "45"= Modeled_Rh_Mg_ha_y_BBL_45, "65"= Modeled_Rh_Mg_ha_y_BBL_65, "85"= Modeled_Rh_Mg_ha_y_BBL_85)
  
ED_estimates_BBL <- ED_estimates_BBL%>%
    pivot_longer(cols=c("0", "45", "65", "85"),
                 names_to='Severity',
                 values_to='Modeled_Rh_Mg_ha_y_BBL')
  
ED_estimates_subke<-    ED_Rh_estimates%>%
  select(year, subplot_id, Rep_ID, Treatment, modeled_Rh_Mg_ha_y_BBL, Modeled_Rh_Mg_ha_y_subke_45,Modeled_Rh_Mg_ha_y_subke_65,Modeled_Rh_Mg_ha_y_subke_85)%>%
  rename("0"= modeled_Rh_Mg_ha_y_BBL, "45"= Modeled_Rh_Mg_ha_y_subke_45, "65"= Modeled_Rh_Mg_ha_y_subke_65, "85"= Modeled_Rh_Mg_ha_y_subke_85)

ED_estimates_subke <- ED_estimates_subke%>%
  pivot_longer(cols=c("0", "45", "65", "85"),
               names_to='Severity',
               values_to='Modeled_Rh_Mg_ha_y_subke')
  
  
ED_Rh_estimates <- merge(ED_estimates_BBL, ED_estimates_subke, by = c("Rep_ID","Treatment", "subplot_id", "year", "Severity"))

#####Create a severity summary dataframe 
ED_Rh_estimates_severity <- ED_Rh_estimates%>%
  group_by(year, Severity)%>%
  summarize(ave_Modeled_Rh_Mg_ha_y_BBL = mean(Modeled_Rh_Mg_ha_y_BBL), se_Modeled_Rh_Mg_ha_y_BBL = std.error(Modeled_Rh_Mg_ha_y_BBL),ave_Modeled_Rh_Mg_ha_y_subke = mean(Modeled_Rh_Mg_ha_y_subke), se_Modeled_Rh_Mg_ha_y_subke = std.error(Modeled_Rh_Mg_ha_y_subke))

write.csv(ED_Rh_estimates_severity, "ED_Rh_estimate.csv", row.names = FALSE)

#####Create a Treatment summary dataframe 
ED_Rh_estimates_treatment <- ED_Rh_estimates%>%
  group_by(year, Treatment)%>%
  summarize(ave_Modeled_Rh_Mg_ha_y_BBL = mean(Modeled_Rh_Mg_ha_y_BBL), se_Modeled_Rh_Mg_ha_y_BBL = std.error(Modeled_Rh_Mg_ha_y_BBL),ave_Modeled_Rh_Mg_ha_y_subke = mean(Modeled_Rh_Mg_ha_y_subke), se_Modeled_Rh_Mg_ha_y_subke = std.error(Modeled_Rh_Mg_ha_y_subke))


write.csv(ED_Rh_estimates, "ED_Rh_estimate.csv", row.names = FALSE)
















