
####Script for calculating leaf and fine debris mass in FoRTE

##By Kayla Mathes 

require(devtools)
require(fortedata)
library(tidyverse)
library(ggplot2)
library(plotrix)
library(rstatix)
library(car)

#Get rid of annoying Scientific notation
options(scipen=999)

##### PLOT METADATA 
meta_data <- fortedata:::fd_plot_metadata()

##Turning into dataframe
meta_data <- data.frame(meta_data)

##combining plot id info into single subplot_id column
meta_data$subplot_id <- paste(meta_data$replicate, 0, meta_data$plot, meta_data$subplot, sep = "")

meta_data$subplot_id <- as.factor(meta_data$subplot_id)

meta_data %>%
  select(subplot_id, disturbance_severity, treatment, replicate) %>%
  distinct() %>%
  na.omit() %>%
  data.frame() -> meta_data




##Aboveground leaf litter production (Mll): from FoRTE google drive
##still needs 2022 data 

litter_trap <- read.csv(file = 'Data_Litter/fd_littertrap.csv',na.strings = c("", "NA"))

names(litter_trap)[names(litter_trap) == "SubplotID"] <- "subplot_id"

##merge with metadata file
litter_trap <-  merge(litter_trap, meta_data, by = "subplot_id")

##filter out only leafs and miscelaneous for leaf mass
leaf_litter <- litter_trap%>%
  filter(fraction == "leaf" | fraction == "misc")%>%
  mutate(leafmass_g = BagMass_g - BagTare_g)


##sum all the species within a subplot to get leafmass_sum_g
leaf_litter_sum <- leaf_litter%>%
  group_by(Year, disturbance_severity, treatment, replicate)%>%
  summarize(leafmass_sum_g = sum(leafmass_g))


##Need to go from grams of dry mass in 4 leaf litter traps to Mg C ha yr (0.49 dry mass to C fraction came from Gough et al. 2009)
leaf_litter_sum <- leaf_litter_sum%>%
  mutate(leafmass_MgChayr = leafmass_sum_g*0.49/0.000104/1000000)


##visualize data
leaf_litter_sum$disturbance_severity <- as.factor(leaf_litter_sum$disturbance_severity)
forte_pal <- forte_colors()

leaf_litter_sum_after <- leaf_litter_sum%>%
  filter(Year != "2018")

plot1 <- ggplot(leaf_litter_sum_after, aes(x = disturbance_severity, y = leafmass_MgChayr, group = disturbance_severity, fill = disturbance_severity)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values = forte_pal)+
  facet_grid(~Year) +
  theme(axis.text.y = element_text(size = 25), axis.title.y = element_text(size = 30), axis.title.x = element_blank(),axis.text.x = element_blank(), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_blank()) +
  labs(x = "Severity", y=expression(paste(" ",NPP[ll]," (",MgC,"  ",ha^-2,"  ",yr^-1,")")))

ggsave(path = "Figures_Output", filename = "leaf_litter.png", height = 10, width =10, units = "in")


#Aboveground fine debris mass (Mfd): from FoRTE google drive
##still needs 2022 data 


##filter out only leafs and miscelaneous for leaf mass
fwd_litter <- litter_trap%>%
  filter(fraction == "fruit" | fraction == "fwd")%>%
  mutate(fwdmass_g = BagMass_g - BagTare_g)%>%
  filter(!is.na(fraction))


##sum all the species within a subplot to get leafmass_sum_g
fwd_litter_sum <- fwd_litter%>%
  group_by(Year, disturbance_severity, treatment, replicate)%>%
  summarize(fwdmass_sum_g = sum(fwdmass_g))


##Need to go from grams of dry mass in 4 leaf litter traps to Mg C ha yr (0.49 dry mass to C fraction came from Gough et al. 2009)
fwd_litter_sum2 <- fwd_litter_sum%>%
  mutate(fwdmass_MgChayr = fwdmass_sum_g*0.49/1.04/0.0001/1000000)


##visualize data
fwd_litter_sum2$disturbance_severity <- as.factor(fwd_litter_sum2$disturbance_severity)

fwd_litter_sum2_after <- fwd_litter_sum2%>%
  filter(Year != "2018")
 
plot2 <- ggplot(fwd_litter_sum2_figure, aes(x = disturbance_severity, y = fwdmass_MgChayr, group = disturbance_severity, fill = disturbance_severity)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values = forte_pal)+
  facet_grid(~Year)+
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_blank(), legend.text = element_text(size = 25), legend.title = element_blank()) +
  labs(x = "Severity", y=expression(paste(" ",NPP[fwd]," (",MgC,"  ",ha^-2,"  ",yr^-1,")")))


#Rh  Multipanel Figure 
NPP_1_grob <- ggplotGrob(plot1 )
NPP_2_grob <- ggplotGrob(plot2 )

layout <- rbind(c(1),
                (2))

library(gridExtra)

NPP <- grid.arrange(NPP_1_grob,NPP_2_grob, layout_matrix=layout)

ggsave(path = "Figures_Output",filename = "NPP_litter.png",height = 20, width = 10, units = "in", NPP)




#######Treatment Figures 
leaf_litter_sum_after <- leaf_litter_sum_after%>%
  mutate(type = case_when(treatment == "T" ~ "Top", 
                          treatment == "B" ~ "Bottom"))%>%
  rename(severity = disturbance_severity, year = Year, rep = replicate)

fwd_litter_sum2_after <- fwd_litter_sum2_after%>%
  mutate(type = case_when(treatment == "T" ~ "Top", 
                          treatment == "B" ~ "Bottom"))%>%
  rename(severity = disturbance_severity, year = Year, rep = replicate)

write.csv(fwd_litter_sum2_after , "Figures_Output/fwd_litter.csv", row.names=FALSE)
write.csv(leaf_litter_sum_after  , "Figures_Output/leaf_litter.csv", row.names=FALSE)

plot3 <- ggplot(leaf_litter_sum_after, aes(x = Type, y = leafmass_MgChayr, group = Type, fill = Type)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  facet_grid(~Year) +
  theme(axis.text.y = element_text(size = 25), axis.title.y = element_text(size = 30), axis.title.x = element_blank(),axis.text.x = element_blank(), strip.text = element_text(size = 30), legend.position = "none") +
  labs(x = "Disturbance Type", y=expression(paste(" ",NPP[ll]," (",MgC,"  ",ha^-2,"  ",yr^-1,")")))

plot4 <- ggplot(fwd_litter_sum2_figure, aes(x = Type, y = fwdmass_MgChayr, group = Type, fill = Type)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  facet_grid(~Year)+
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_blank(), legend.position = "none") +
  labs(x = "Disturbance Type", y=expression(paste(" ",NPP[fwd]," (",MgC,"  ",ha^-2,"  ",yr^-1,")")))


#Rh  Multipanel Figure 
NPP_3_grob <- ggplotGrob(plot3 )
NPP_4_grob <- ggplotGrob(plot4 )

layout <- rbind(c(1),
                (2))

library(gridExtra)

NPP_treatment <- grid.arrange(NPP_3_grob,NPP_4_grob, layout_matrix=layout)

ggsave(path = "Figures_Output",filename = "NPP_litter.png",height = 20, width = 10, units = "in", NPP)

################### Summary Dataframes for table#########################
leaf_litter_sum_summary_severity <- leaf_litter_sum%>%
  group_by(disturbance_severity, Year)%>%
  summarize(ll_MgChayr_ave = mean(leafmass_MgChayr), ll_se = std.error(leafmass_MgChayr))

leaf_litter_sum_summary_treatment <- leaf_litter_sum%>%
  group_by(treatment, Year)%>%
  summarize(ll_MgChayr_ave = mean(leafmass_MgChayr), ll_se = std.error(leafmass_MgChayr))

fwd_litter_sum2_summary_severity <- fwd_litter_sum2%>%
  filter(!is.na(fwdmass_MgChayr))%>%
  group_by(disturbance_severity, Year)%>%
  summarize(fwd_MgChayr_ave = mean(fwdmass_MgChayr), fwd_se = std.error(fwdmass_MgChayr))

fwd_litter_sum2_summary_treatment <- fwd_litter_sum2%>%
  filter(!is.na(fwdmass_MgChayr))%>%
  group_by(treatment, Year)%>%
  summarize(fwd_MgChayr_ave = mean(fwdmass_MgChayr), fwd_se = std.error(fwdmass_MgChayr))





###########################Stats ##################################

##Transform variables into factors for model 
leaf_litter_sum_after$disturbance_severity <- as.factor(leaf_litter_sum_after$disturbance_severity)
leaf_litter_sum_after$treatment <- as.factor(leaf_litter_sum_after$treatment)
leaf_litter_sum_after$Year <- as.factor(leaf_litter_sum_after$Year)
leaf_litter_sum_after$replicate <- as.factor(leaf_litter_sum_after$replicate)

####Testing Assumptions 
##Test for outliers test: no extreme outliers
leaf_litter_sum_after%>% 
  group_by(disturbance_severity, treatment, Year) %>%
  identify_outliers(leafmass_MgChayr)

##Equality of variance test for severity and treatment 
leveneTest(leafmass_MgChayr ~ Year*treatment*disturbance_severity, data = leaf_litter_sum_after)

##Normality 
# Build the linear model
normality_test  <- lm(leafmass_MgChayr ~ disturbance_severity*treatment*Year,
                      data = leaf_litter_sum_after)

# Shapiro test of normality: Data not normal  
shapiro_test(residuals(normality_test))

leaf_litter_sum_after <- leaf_litter_sum_after%>%
  mutate(leafmass_MgChayr_rec = sqrt(1/leafmass_MgChayr))

# Build the linear model
normality_test  <- lm(leafmass_MgChayr_rec ~ disturbance_severity*treatment*Year,
                      data = leaf_litter_sum_after)

shapiro_test(residuals(normality_test))


####WORKING SPLIT-SPLIT MODEL: Using aov(). Same results as the agricolae package. Ran an ANCOVA with VWC as a covariate.(However significance does not change with or without VWC). 

leaf_litter_sum_after_model <- aov(leafmass_MgChayr  ~ disturbance_severity*treatment*Year +replicate +Error(replicate:disturbance_severity/treatment/Year), data = leaf_litter_sum_after)

summary(leaf_litter_sum_after_model)

library(agricolae)

out_year_severity_leaf<- with(leaf_litter_sum_after, LSD.test(leafmass_MgChayr, disturbance_severity:Year,48, 0.0391, console = TRUE))


####Fine woody debris 
##Transform variables into factors for model 

fwd_litter_sum2_after <- fwd_litter_sum2%>%
  filter(Year != "2018")
fwd_litter_sum2_after[is.na(fwd_litter_sum2_after)] <- 0

fwd_litter_sum2_after$disturbance_severity <- as.factor(fwd_litter_sum2_after$disturbance_severity)
fwd_litter_sum2_after$treatment <- as.factor(fwd_litter_sum2_after$treatment)
fwd_litter_sum2_after$Year <- as.factor(fwd_litter_sum2_after$Year)
fwd_litter_sum2_after$replicate <- as.factor(fwd_litter_sum2_after$replicate)

####Testing Assumptions 
##Test for outliers test: no extreme outliers
fwd_litter_sum2_after%>% 
  group_by(disturbance_severity, treatment, Year) %>%
  identify_outliers(fwdmass_MgChayr)

##Equality of variance test for severity and treatment (Slightly unequal data using alpha = 0.05. Equal variance using alpha = 0.1)
leveneTest(fwdmass_MgChayr ~ Year*treatment*disturbance_severity, data = fwd_litter_sum2_after)

##Normality (Data are normal)
# Build the linear model
normality_test  <- lm(fwdmass_MgChayr ~ disturbance_severity*treatment*Year,
                      data = fwd_litter_sum2_after)

# Shapiro test of normality: Data not normal  
shapiro_test(residuals(normality_test))

fwd_litter_sum2_after <- fwd_litter_sum2_after%>%
  mutate(fwdmass_MgChayr_log = log(fwdmass_MgChayr))

# Build the linear model
normality_test  <- lm(fwdmass_MgChayr_log ~ disturbance_severity*treatment*Year,
                      data = fwd_litter_sum2_after)

shapiro_test(residuals(normality_test))


####WORKING SPLIT-SPLIT MODEL: Using aov(). Same results as the agricolae package. Ran an ANCOVA with VWC as a covariate.(However significance does not change with or without VWC). 

fwd_litter_sum_after_model <- aov(fwdmass_MgChayr  ~ disturbance_severity*treatment*Year + replicate +Error(replicate:disturbance_severity/treatment/Year), data = fwd_litter_sum2_after)

summary(fwd_litter_sum_after_model)















