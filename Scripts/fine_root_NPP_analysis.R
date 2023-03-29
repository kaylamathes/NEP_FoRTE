###########################################################
####FoRTE Fine root Data Analysis
####2019-2020
####Kayla C. Mathes  
###########################################################

#Load Libraries
library(googledrive)
library(dplyr)
library(rstatix)
library(car)
library(stringr)
library(ggplot2)
library(lubridate)



######Download fine root data for 2019, 2020, 2021 from Google Drive####

# Direct Google Drive link to "FoRTE/data/soil_respiration/2021"
as_id("https://drive.google.com/drive/folders/11qBp_RlpPqoGG4XziO9LPnwUUyyYNsI4") %>% 
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

## Import downloaded date from new data directory "googledrive_data"
root_mass_2020 <- read.csv("googledrive_data/in_growth_soil_core_2020.csv", na.strings = c("NA", "na"))
burn_mass_2020 <- read.csv("googledrive_data/Burn_mass_2020.csv", na.strings = c("NA","na"))
root_mass_2019 <- read.csv("googledrive_data/in_growth_soil_core_2019.csv", na.strings = c("NA", "na", ""))
root_mass_2021 <- read.csv("googledrive_data/in_growth_soil_core_2021.csv", na.strings = c("NA", "na", ""))


##################################### 2019 ROOT DATA ######################################################## ###(INSTALLATION Date:June 3, COLLECTION November 11)

##Create a root mass column 

root_mass_2019 <- root_mass_2019%>%
  mutate(root_mass_g = total_g - bag_g)

##T-test to determine if trimmed vs. untrimmed are different (n>30) = Yes they are statistically different 

##Equality of variance test for replicates: variances are equal with alpha = 0.1
leveneTest(root_mass_g ~ trimmed.untrimmed, data = root_mass_2019)

##Normality (Data are normal)
# Build the linear model
normality_test  <- lm(root_mass_g ~ trimmed.untrimmed,
                      data = root_mass_2019)

# Shapiro test of normality: normal with alpha of 0.01
shapiro_test(residuals(normality_test))

##Run T-test
t_test <- t.test(root_mass_g~trimmed.untrimmed, data = root_mass_2019)


# ##Make a graph visualizing this difference 
# ggplot(root_mass_2019,aes(x = trimmed.untrimmed, y = root_mass_g, fill = trimmed.untrimmed)) +
#   theme_classic()+
#   geom_boxplot()+
#   theme(axis.text.x= element_text(size = 20), axis.text.y= element_text(size=20), axis.title.x = element_text(size = 25), axis.title.y  = element_text(size=25), legend.title = element_blank(),  strip.text.x =element_text(size = 25), legend.text = element_blank(), legend.position = "none",plot.margin = margin(1,1,1,1, "cm")) +
#   scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
#   guides(col = guide_legend(nrow = 2)) +
#   labs(x = "Trim Status", y= "Root Mass (g)")

##percent difference between average trim and untrimmed roots 
##Create mean variables from t-test summary 
Mu1 <- t_test$estimate[1]
Mu2 <- t_test$estimate[2]

##Create a percent difference variable 
percent_diff <- (abs(Mu1 - Mu2) / ((Mu1 + Mu2)/2))


##Add percent difference variable to dataframe for untrimmed 
root_mass_2019$trimmed.untrimmed <- as.factor(root_mass_2019$trimmed.untrimmed)

root_mass_2019 <- root_mass_2019%>%
  mutate(root_mass_g = case_when(trimmed.untrimmed == "untrimmed" ~ (root_mass_g - (root_mass_g                                         *percent_diff)),
                                     trimmed.untrimmed == "trimmed" ~ root_mass_g))

##Create two separate datasets for burn columns and root mass columns 
root_mass_2019_only <- root_mass_2019%>%
  select(subplot_id, root_mass_g)%>%
  filter(!is.na(root_mass_g))

##Add severity and treatment to dataframe 
##add disturbance severity and treatment columns from subplot codes  
Plot_conversion <- function(subplot_id) {
  if(str_detect(subplot_id, "A01E")==TRUE) {
    Disturbance_severity = "85"
  } else if(str_detect(subplot_id, "A01W")==TRUE) {
    Disturbance_severity = "85"
  }else if(str_detect(subplot_id, "A02E")==TRUE) {
    Disturbance_severity = "45"
  }else if(str_detect(subplot_id, "A02W")==TRUE) {
    Disturbance_severity = "45"
  }else if(str_detect(subplot_id, "A03E")==TRUE) {
    Disturbance_severity = "65"
  }else if(str_detect(subplot_id, "A03W")==TRUE) {
    Disturbance_severity = "65"
  }else if(str_detect(subplot_id, "A04E")==TRUE) {
    Disturbance_severity = "0"
  }else if(str_detect(subplot_id, "A04W")==TRUE) {
    Disturbance_severity = "0"
  }else if(str_detect(subplot_id, "B01E")==TRUE) {
    Disturbance_severity = "0"
  } else if(str_detect(subplot_id, "B01W")==TRUE) {
    Disturbance_severity = "0"
  } else if(str_detect(subplot_id, "B02E")==TRUE) {
    Disturbance_severity = "45"
  } else if(str_detect(subplot_id, "B02W")==TRUE) {
    Disturbance_severity = "45"
  } else if(str_detect(subplot_id, "B03E")==TRUE) {
    Disturbance_severity = "85"
  } else if(str_detect(subplot_id, "B03W")==TRUE) {
    Disturbance_severity = "85"
  } else if(str_detect(subplot_id, "B04E")==TRUE) {
    Disturbance_severity = "65"
  } else if(str_detect(subplot_id, "B04W")==TRUE) {
    Disturbance_severity = "65"
  } else if(str_detect(subplot_id, "C01E")==TRUE) {
    Disturbance_severity = "0"
  } else if(str_detect(subplot_id, "C01W")==TRUE) {
    Disturbance_severity = "0"
  } else if(str_detect(subplot_id, "C02E")==TRUE) {
    Disturbance_severity = "65"
  } else if(str_detect(subplot_id, "C02W")==TRUE) {
    Disturbance_severity = "65"
  } else if(str_detect(subplot_id, "C03E")==TRUE) {
    Disturbance_severity = "85"
  } else if(str_detect(subplot_id, "C03W")==TRUE) {
    Disturbance_severity = "85"
  } else if(str_detect(subplot_id, "C04E")==TRUE) {
    Disturbance_severity = "45"
  } else if(str_detect(subplot_id, "C04W")==TRUE) {
    Disturbance_severity = "45"
  } else if(str_detect(subplot_id, "D01E")==TRUE) {
    Disturbance_severity = "0"
  } else if(str_detect(subplot_id, "D01W")==TRUE) {
    Disturbance_severity = "0"
  } else if(str_detect(subplot_id, "D02E")==TRUE) {
    Disturbance_severity = "85"
  } else if(str_detect(subplot_id, "D02W")==TRUE) {
    Disturbance_severity = "85"
  } else if(str_detect(subplot_id, "D03E")==TRUE) {
    Disturbance_severity = "45"
  } else if(str_detect(subplot_id, "D03W")==TRUE) {
    Disturbance_severity = "45"
  } else if(str_detect(subplot_id, "D04E")==TRUE) {
    Disturbance_severity = "65"
  } else if(str_detect(subplot_id, "D04W")==TRUE) {
    Disturbance_severity = "65"
  }
  return(Disturbance_severity)
}

##Add disturbance severity column to dataset
root_mass_2019_only <- root_mass_2019_only %>%
  mutate(Severity = sapply(subplot_id, Plot_conversion))


##Add Treatment variable 
Sub_plot_conversion <- function(subplot_id) {
  if(str_detect(subplot_id, "A01E")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(subplot_id, "A01W")==TRUE) {
    Treatment = "top"
  }else if(str_detect(subplot_id, "A02E")==TRUE) {
    Treatment = "top"
  }else if(str_detect(subplot_id, "A02W")==TRUE) {
    Treatment = "bottom"
  }else if(str_detect(subplot_id, "A03E")==TRUE) {
    Treatment = "bottom"
  }else if(str_detect(subplot_id, "A03W")==TRUE) {
    Treatment = "top"
  }else if(str_detect(subplot_id, "A04E")==TRUE) {
    Treatment = "bottom"
  }else if(str_detect(subplot_id, "A04W")==TRUE) {
    Treatment = "top"
  }else if(str_detect(subplot_id, "B01E")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(subplot_id, "B01W")==TRUE) {
    Treatment = "top"
  } else if(str_detect(subplot_id, "B02E")==TRUE) {
    Treatment = "top"
  } else if(str_detect(subplot_id, "B02W")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(subplot_id, "B03E")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(subplot_id, "B03W")==TRUE) {
    Treatment = "top"
  } else if(str_detect(subplot_id, "B04E")==TRUE) {
    Treatment = "top"
  } else if(str_detect(subplot_id, "B04W")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(subplot_id, "C01E")==TRUE) {
    Treatment = "top"
  } else if(str_detect(subplot_id, "C01W")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(subplot_id, "C02E")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(subplot_id, "C02W")==TRUE) {
    Treatment = "top"
  } else if(str_detect(subplot_id, "C03E")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(subplot_id, "C03W")==TRUE) {
    Treatment = "top"
  } else if(str_detect(subplot_id, "C04E")==TRUE) {
    Treatment = "top"
  } else if(str_detect(subplot_id, "C04W")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(subplot_id, "D01E")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(subplot_id, "D01W")==TRUE) {
    Treatment = "top"
  } else if(str_detect(subplot_id, "D02E")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(subplot_id, "D02W")==TRUE) {
    Treatment = "top"
  } else if(str_detect(subplot_id, "D03E")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(subplot_id, "D03W")==TRUE) {
    Treatment = "top"
  } else if(str_detect(subplot_id, "D04E")==TRUE) {
    Treatment = "top"
  } else if(str_detect(subplot_id, "D04W")==TRUE) {
    Treatment = "bottom"
  }
  return(Treatment)
}

##Add treatment column to datasets
root_mass_2019_only <-root_mass_2019_only %>%
  mutate(Treatment = sapply(subplot_id, Sub_plot_conversion))

##Adding a unique rep column 
rep_add <- function(subplot_id) {
  if(str_detect(subplot_id, "A01E")==TRUE) {
    rep = "A"
  } else if(str_detect(subplot_id, "A01W")==TRUE) {
    rep = "A"
  }else if(str_detect(subplot_id, "A02E")==TRUE) {
    rep = "A"
  }else if(str_detect(subplot_id, "A02W")==TRUE) {
    rep = "A"
  }else if(str_detect(subplot_id, "A03E")==TRUE) {
    rep = "A"
  }else if(str_detect(subplot_id, "A03W")==TRUE) {
    rep = "A"
  }else if(str_detect(subplot_id, "A04E")==TRUE) {
    rep = "A"
  }else if(str_detect(subplot_id, "A04W")==TRUE) {
    rep = "A"
  }else if(str_detect(subplot_id, "B01E")==TRUE) {
    rep = "B"
  } else if(str_detect(subplot_id, "B01W")==TRUE) {
    rep = "B"
  } else if(str_detect(subplot_id, "B02E")==TRUE) {
    rep = "B"
  } else if(str_detect(subplot_id, "B02W")==TRUE) {
    rep = "B"
  } else if(str_detect(subplot_id, "B03E")==TRUE) {
    rep = "B"
  } else if(str_detect(subplot_id, "B03W")==TRUE) {
    rep = "B"
  } else if(str_detect(subplot_id, "B04E")==TRUE) {
    rep = "B"
  } else if(str_detect(subplot_id, "B04W")==TRUE) {
    rep = "B"
  } else if(str_detect(subplot_id, "C01E")==TRUE) {
    rep = "C"
  } else if(str_detect(subplot_id, "C01W")==TRUE) {
    rep = "C"
  } else if(str_detect(subplot_id, "C02E")==TRUE) {
    rep = "C"
  } else if(str_detect(subplot_id, "C02W")==TRUE) {
    rep = "C"
  } else if(str_detect(subplot_id, "C03E")==TRUE) {
    rep = "C"
  } else if(str_detect(subplot_id, "C03W")==TRUE) {
    rep = "C"
  } else if(str_detect(subplot_id, "C04E")==TRUE) {
    rep = "C"
  } else if(str_detect(subplot_id, "C04W")==TRUE) {
    rep = "C"
  } else if(str_detect(subplot_id, "D01E")==TRUE) {
    rep = "D"
  } else if(str_detect(subplot_id, "D01W")==TRUE) {
    rep = "D"
  } else if(str_detect(subplot_id, "D02E")==TRUE) {
    rep = "D"
  } else if(str_detect(subplot_id, "D02W")==TRUE) {
    rep = "D"
  } else if(str_detect(subplot_id, "D03E")==TRUE) {
    rep = "D"
  } else if(str_detect(subplot_id, "D03W")==TRUE) {
    rep = "D"
  } else if(str_detect(subplot_id, "D04E")==TRUE) {
    rep = "D"
  } else if(str_detect(subplot_id, "D04W")==TRUE) {
    rep = "D"
  }
  return(rep)
}

##Add rep column to the in_growth_core dataframe 
root_mass_2019_only <-root_mass_2019_only %>%
  mutate(rep = sapply(subplot_id, rep_add))


##Adjust for Ash free weight (Calculated as percent decline from pre and post root burning for 12 hours which was 81%)

root_mass_2019_ash_adjust <- root_mass_2019_only%>%
  mutate(root_mass_g_adj = root_mass_g*0.97)



root_mass_2019_ash_adjust <- root_mass_2019_ash_adjust%>%
  mutate(year = "2019")


########################### 2020 ROOT DATA #################################################################### (INSTALLATION Date:July 3, COLLECTION November 16)

#######Adjust root mass for ash free weight#### 
###Create percent root column (measured as percent decrease from burn samples). Calculated as percent decrease, that decrease is the all roots that burned in muffle furnace, what is left is inorganic material. Then create a Rep column 
burn_mass_2020 <- burn_mass_2020%>%
  mutate(percent_root = (pre_burn_g_12hr - post_burn_g_12hr)/pre_burn_g_12hr*100)%>%
  mutate(Rep = case_when(Subplot_ID == "A01W" | Subplot_ID == "A02W" | Subplot_ID == "A04W" ~ "A", 
                        Subplot_ID == "B01W" |  Subplot_ID == "B03E" | Subplot_ID == "B03W" ~ "B", 
                         Subplot_ID == "C01W" | Subplot_ID == "C04E" | Subplot_ID == "C04W" ~ "C", 
                        Subplot_ID == "D01W" | Subplot_ID == "D02E" | Subplot_ID == "D02W" ~ "D"))

####Test whether or not there is a replicate difference in amount of inorganic material 

##Test Model assumptions 

##Test for outliers test: no outliers 
burn_mass_2020 %>% 
  group_by(Rep) %>%
  identify_outliers(percent_root)

##Equality of variance test for replicates: variances are equal 
leveneTest(percent_root ~ Rep, data = burn_mass_2020)

##Normality (Data are normal)
# Build the linear model
normality_test  <- lm(percent_root ~ Rep,
                      data = burn_mass_2020)

# Shapiro test of normality: normal with alpha of 0.01
shapiro_test(residuals(normality_test))

##No statistical difference between percent root material across replicates 
burn_anova <- aov(percent_root ~ Rep, data = burn_mass_2020)
summary(burn_anova)

###Applying Ash free adjustment to 2020 root data 

##Calculate the average "percent root" from each burn sample 

Average_burn_mass_2020 <- burn_mass_2020%>%
  mutate(average_percent_root = mean(percent_root))

##Apply that percentage to the root weight (grams)

root_mass_2020_adj <- root_mass_2020%>%
  mutate(root_mass_g_adj = Root.Weight*0.97)%>%
  rename(subplot_id = Subplot_ID, root_mass_g = Root.Weight)


####Clean 2020 root data ##### (INSTALLATION Date:July 3, COLLECTION November 16)


##Add disturbance severity column to dataset
root_mass_2020_adj <- root_mass_2020_adj %>%
  mutate(Severity = sapply(subplot_id, Plot_conversion))


##Add treatment column to datasets
root_mass_2020_adj <- root_mass_2020_adj %>%
  mutate(Treatment = sapply(subplot_id, Sub_plot_conversion))


##Add rep column to the in_growth_core dataframe 
root_mass_2020_adj <-root_mass_2020_adj %>%
  mutate(rep = sapply(subplot_id, rep_add))

root_mass_2020_adj <- root_mass_2020_adj%>%
  select(!Bag.weight)%>%
  mutate(year = "2020")


  

################################ 2021 ROOT DATA ############################################################## (Installation Date: June 18, Collection date: Novemeber 13th)

#######Adjust root mass for ash free weight#### 

##Apply that percentage to the root weight (grams): From 2020 Data: UPDATE ONCE ROOTS HAVE BEEN BURNED

root_mass_2021_adj <- root_mass_2021%>%
  mutate(root_mass_g_adj = Root.Weight.g.*0.97)


####Clean 2021 root data #####

##Change name of column 
names(root_mass_2021_adj)[1]<-paste("subplot_id")

##Add disturbance severity column to dataset
root_mass_2021_adj <- root_mass_2021_adj %>%
  mutate(Severity = sapply(subplot_id, Plot_conversion))


##Add treatment column to datasets
root_mass_2021_adj <- root_mass_2021_adj %>%
  mutate(Treatment = sapply(subplot_id, Sub_plot_conversion))


##Add rep column to the in_growth_core dataframe 
root_mass_2021_adj <-root_mass_2021_adj %>%
  mutate(rep = sapply(subplot_id, rep_add))

root_mass_2021_adj <- root_mass_2021_adj%>%
  mutate(year = "2021")%>%
  rename(root_mass_g = Root.Weight.g.)


################################################## ALL YEARS COMBINED ########################################
root_combined <- rbind(root_mass_2019_ash_adjust, root_mass_2020_adj,root_mass_2021_adj ) 

root_combined_severity <- root_combined%>%
  group_by(year,Severity)%>%
  summarize(root_mass_average = mean(root_mass_g_adj))

root_combined_treatment <- root_combined%>%
  group_by(year, rep, Treatment)%>%
  summarize(root_mass_average = mean(root_mass_g_adj))

###Plot 2019, 2020, 2021 Data 
##visualize the root mass data by severity
ggplot(root_combined,aes(x = Severity, y = root_mass_g_adj, fill = Severity)) +
 scale_fill_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00")) +
  theme_classic()+
  geom_boxplot()+
  theme(axis.text.x= element_text(size = 30), axis.text.y= element_text(size=30), axis.title.x = element_text(size = 35), axis.title.y  = element_text(size=35), legend.title = element_blank(),  strip.text.x =element_text(size = 35), legend.text = element_blank(), legend.position = "none") +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
  guides(col = guide_legend(nrow = 2)) +
  facet_grid(.~year,scales="free")+ 
  labs(x = "Severity", y= "Root Mass (g)")

ggsave(path = "Figures_Output", filename = "Root_mass_g.png", height = 20, width =30, units = "in")
                                        

########################## Scale up root weight (from g/30 cm/year => Mg C /ha/yr) for just the top 30cm of root growth  #####################

### height = 30 cm
### Diameter = 5.79cm 
### Volume of a cylinder = 787.17cm3
### 
### multiple by 4 because it is across 4 root cores = 3148.68 cm3

### calculate the root production per cm3 
### then integrate down 
### then scale out 



####### Apply root profile relationship to measure remaining fine-root growth in the vertical profile. ### equation from Gough dataset at UMBS 

###Figure out much of the roots are within the top 30 and how much of the roots are below 30, add the porportion that is missing to the FoRTE roots 

#### apply a root profile down to a meter 
x_cm <- c(0,10,20,30,40,50,60,70,80,90,100)

##### apply Gough's equation to predict root growth 
root_profile <- as.data.frame(x_cm)%>%
  mutate(y_g = 7.93724*exp(-0.03669*x_cm))

### total root mass in g
root_profile%>%
 summarize(sum = sum(y_g))

## root growth from top 30 cm 
root_profile%>%
  filter(x_cm > 0 &x_cm <40)%>%
  summarize(sum = sum(y_g))

## root growth from 40-100  
root_profile%>%
  filter(x_cm < 10  |x_cm > 30)%>%
  summarize(sum = sum(y_g))

root_profile%>%
  summarize(sum =sum(y_g))

###Calculate the percent of roots that are missing from the in-growth cores 


# 0.2765459 (27.65459% of the roots are missing from the in-growth cores) 100-27.65459 = 

###Using two different methods to scale roots. One that goes from top surface area of the core to surface area of the suplot, the other that goes from volume of core to volume of subplot 
root_combined <- root_combined%>%
  mutate(root_mass_g_1m = root_mass_g_adj/0.78)%>%
  mutate(root_mass_Mg_C_ha_yr_1 = root_mass_g_1m/0.008/0.0001/1000000*0.49)

write.csv(root_combined, "FoRTE_fine_roots.csv", row.names=FALSE)

root_combined_0 = root_combined%>%
  filter(Severity == "0")


library(plotrix)
root_combined_severity <- root_combined%>%
  group_by(year, Severity)%>%
  summarize(root_mass_Mg_C_ha_yr_summary = mean(root_mass_Mg_C_ha_yr_1), root_mass_se = std.error(root_mass_Mg_C_ha_yr_1))





##############Applying a turnover rate (daily) to estimate root production for the days the core was not in the ground #########

modeled_Ts_2019_6hr <- read.csv("modeled_6hr_Ts_2019.csv")

modeled_Ts_2019_6hr$Timestamp_6 <- as.POSIXct(modeled_Ts_2019_6hr$Timestamp_6)

modeled_Ts_2020_6hr <- read.csv("modeled_6hr_Ts_2020.csv")
modeled_Ts_2020_6hr$Timestamp_6 <- as.POSIXct(modeled_Ts_2020_6hr$Timestamp_6)

modeled_Ts_2021_6hr <- read.csv("modeled_6hr_Ts_2021.csv")
modeled_Ts_2021_6hr$Timestamp_6 <- as.POSIXct(modeled_Ts_2021_6hr$Timestamp_6)

model_Ts_2019_day <- modeled_Ts_2019_6hr%>%
  group_by(Subplot_ID, Timestamp_6 = floor_date(Timestamp_6, "1 day"))%>%
  summarize(tower_temp_day = mean(tower_temp), modeled_temp_day = mean(modeled_temp))


#####Applying a daily turnover rate equation from Gough et al. 2008


model_Ts_2019_day_k <- model_Ts_2019_day%>%
  mutate(k_day = 0.0113+0.035*modeled_temp_day)



###Plot 2019, 2020, 2021
##by treatment
ggplot(root_combined,aes(x = Severity, y = root_mass_Mg_C_ha_yr_1, fill = Severity)) +
  scale_fill_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00")) +
  theme_classic()+
  geom_boxplot()+
  theme(axis.text.x= element_text(size = 20), axis.text.y= element_text(size=20), axis.title.x = element_text(size = 25), axis.title.y  = element_text(size=25), legend.title = element_blank(),  strip.text.x =element_text(size = 25), legend.text = element_blank(), legend.position = "none") +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
  guides(col = guide_legend(nrow = 2)) +
  facet_grid(.~year,scales="free")+
  labs(x = "Treatment", y= "Root Mass (Mg C ha yr")

root_combined_2019 <- root_combined%>%
 filter(year == 2019)

######## Make the datafile for the figure ####################

root_combined <- root_combined%>%
  rename(type = Treatment, severity = Severity)

write.csv(root_combined, "Figures_Output/NPP_fr.csv", row.names=FALSE)

