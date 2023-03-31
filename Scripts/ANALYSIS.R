#######figures and ANOVA models for NEP FoRTE Assessment #######
library(ggplot2)
library(dplyr)
library(fortedata)
library(rstatix)
library(car)
library(agricolae)

####Load CSV files from other R scripts 

NPPw <- read.csv("Figures_Output/NPP_total.csv")
NPP_ll <- read.csv("Figures_Output/leaf_litter.csv")%>%
  select(!treatment)
NPP_fwd <- read.csv("Figures_Output/fwd_litter.csv", na.strings = c("NA", "na"))%>%
  select(!treatment)
R_CWD <- read.csv("Figures_Output/R_CWD.csv")
R_sh <- read.csv("Figures_Output/Rsh_total.csv")
NPP_fr <- read.csv("Figures_Output/FoRTE_fine_roots.csv")


forte_pal <- forte_colors()

##################################################### big dataframe ######################################################



#######################################Figures #####################################################
######################## Wood NPP ################################################################

##Convert variables 
NPP_fwd $severity <- as.factor(NPP_fwd $severity)
NPP_fwd $type <- as.factor(NPP_fwd $type)
NPP_fwd $year <- as.factor(NPP_fwd $year)
NPP_fwd $rep <- as.factor(NPP_fwd $rep)

##Convert variables 
NPP_ll$severity <- as.factor(NPP_ll$severity)
NPP_ll$type <- as.factor(NPP_ll$type)
NPP_ll$year <- as.factor(NPP_ll$year)
NPP_ll$rep <- as.factor(NPP_ll$rep)

##Convert variables 
NPPw$severity <- as.factor(NPPw$severity)
NPPw$type <- as.factor(NPPw$type)
NPPw$year <- as.factor(NPPw$year)
NPPw$rep <- as.factor(NPPw$rep)

############## total NPPw ##############################

#####Add significance (2020:85% and 2022:65%)

ggplot(NPPw, aes(x = severity, y = NPPw, fill = severity, group = severity)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("NPPw (Mg C ha yr)") 


ggplot(NPPw, aes(x = type, y = NPPw, fill = type, group = type)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("NPPw (Mg C ha yr)") 

#########Aboveground Canopy and subcanopy NPP: Severity 

ggplot(NPPw, aes(x = severity, y = NPP_can, fill = severity, group = severity)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("Canopy NPP (Mg C ha yr)") 

ggplot(NPPw, aes(x = type, y = NPP_can, fill = type, group = type)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("Canopy NPP (Mg C ha yr)") 

ggplot(NPPw, aes(x = severity, y = NPP_sc, fill = severity, group = severity)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("Subcanopy NPP (Mg C ha yr)") 

ggplot(NPPw, aes(x = type, y = NPP_sc, fill = type, group = type)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("Subcanopy NPP (Mg C ha yr)") 



######################################### Leaf litter & fine-woody debris #############

###NPP_ll severity 
###Add significance to 2021: 85%
ggplot(NPP_ll, aes(x = severity, y = leafmass_MgChayr, group = severity, fill = severity)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values = forte_pal)+
  facet_grid(~year) +
  theme(axis.text.y = element_text(size = 25), axis.title.y = element_text(size = 30), axis.title.x = element_blank(),axis.text.x = element_blank(), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_blank()) +
  labs(x = "Severity", y=expression(paste(" ",NPP[ll]," (",MgC,"  ",ha^-2,"  ",yr^-1,")")))

###NPP_fwd severity 
ggplot(NPP_fwd, aes(x = severity, y = fwdmass_MgChayr, group = severity, fill = severity)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values = forte_pal)+
  facet_grid(~year)+
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_blank(), legend.text = element_text(size = 25), legend.title = element_blank()) +
  labs(x = "Severity", y=expression(paste(" ",NPP[fwd]," (",MgC,"  ",ha^-2,"  ",yr^-1,")")))

##NPP_ll type
ggplot(NPP_ll, aes(x = type, y = leafmass_MgChayr, group = type, fill = type)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  facet_grid(~year) +
  theme(axis.text.y = element_text(size = 25), axis.title.y = element_text(size = 30), axis.title.x = element_blank(),axis.text.x = element_blank(), strip.text = element_text(size = 30), legend.position = "none") +
  labs(x = "Disturbance Type", y=expression(paste(" ",NPP[ll]," (",MgC,"  ",ha^-2,"  ",yr^-1,")")))

##NPP_fwd type 
ggplot(NPP_fwd, aes(x = type, y = fwdmass_MgChayr, group = type, fill = type)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  facet_grid(~year)+
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_blank(), legend.position = "none") +
  labs(x = "Disturbance Type", y=expression(paste(" ",NPP[fwd]," (",MgC,"  ",ha^-2,"  ",yr^-1,")")))


######################################### Fine-root NPP #####################################

NPP_fr$severity <- as.factor(NPP_fr$severity)
NPP_fr$type <- as.factor(NPP_fr$type)
NPP_fr$year <- as.factor(NPP_fr$year)
NPP_fr$rep <- as.factor(NPP_fr$rep)


ggplot(NPP_fr, aes(x = severity, y = root_mass_Mg_C_ha_yr_1, group = severity, fill = severity)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values = forte_pal)+
  facet_grid(~year) +
  theme(axis.text.y = element_text(size = 25), axis.title.y = element_text(size = 30), axis.title.x = element_blank(),axis.text.x = element_blank(), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_blank()) +
  labs(x = "Severity", y=expression(paste(" ",NPP[fr]," (",MgC,"  ",ha^-2,"  ",yr^-1,")")))

ggplot(NPP_fr, aes(x = type, y = root_mass_Mg_C_ha_yr_1, group = type, fill = type)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  facet_grid(~year) +
  theme(axis.text.y = element_text(size = 25), axis.title.y = element_text(size = 30), axis.title.x = element_blank(),axis.text.x = element_blank(), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_blank()) +
  labs(x = "type", y=expression(paste(" ",NPP[fr]," (",MgC,"  ",ha^-2,"  ",yr^-1,")")))


########################################## CWD #########################################



R_CWD$severity <- as.factor(R_CWD$severity)
R_CWD$type <- as.factor(R_CWD$type)
R_CWD$year <- as.factor(R_CWD$year)
R_CWD$rep <- as.factor(R_CWD$rep)

ggplot(R_CWD, aes(x = severity, y = R_CWD_Mghayr_ave, fill = severity, group = severity)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("R CWD (Mg C ha yr)") 

ggplot(R_CWD, aes(x = type, y = R_CWD_Mghayr_ave, fill = type, group = type)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("R CWD (Mg C ha yr)") 

ggplot(R_CWD, aes(x = rep, y = R_CWD_Mghayr_ave, fill = rep, group = rep)) +
  geom_boxplot() +
  facet_grid(~year) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("R CWD (Mg C ha yr)") 


############################################### Soil Hetertrophic Respiration #######################

R_sh$severity <- as.factor(R_sh$severity)
R_sh$type <- as.factor(R_sh$type)
R_sh$year <- as.factor(R_sh$year)
R_sh$rep <- as.factor(R_sh$rep)

ggplot(R_sh, aes(x = severity, y = Modeled_Rh_Mg_ha_y_mean, fill = severity, group = severity)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("R sh (Mg C ha yr)") 

ggplot(R_sh, aes(x = type, y = Modeled_Rh_Mg_ha_y_mean, fill = type, group = type)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("R sh (Mg C ha yr)") 

ggplot(R_sh, aes(x = rep, y = Modeled_Rh_Mg_ha_y_mean, fill = rep, group = rep)) +
  geom_boxplot() +
  facet_grid(~year) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("R sh (Mg C ha yr)") 



##################################################### STATS ######################################################
######################

####NPP canopy 
##Equality of variance test for severity, type and year: Equal 
leveneTest(NPP_can ~ year*type*severity, data = NPPw)

##Normality 
# Build the linear model
normality_test  <- lm(NPP_can ~ severity*type*year,
                      data = NPPw)

# Shapiro test of normality: Normal 
shapiro_test(residuals(normality_test))


####SPLIT-SPLIT MODEL: Using aov() ####

NPP_can_model <- aov(NPP_can ~ severity*type*year +rep + Error(rep:severity/type/year), data = NPPw)
summary(NPP_can_model)

out_year_severity_NPP_can<- with(NPPw, LSD.test(NPP_can, severity:year,72, 0.371, console = TRUE))

#######################NPP subcanopy ###################################
##Equality of variance test for severity, type and year: Equal 
leveneTest(NPP_sc ~ year*type*severity, data = NPPw)

##Normality 
# Build the linear model
normality_test  <- lm(NPP_sc ~ severity*type*year,
                      data = NPPw)

# Shapiro test of normality: Not normal 
shapiro_test(residuals(normality_test))

#####Log transformation 

NPPw <- NPPw%>%
  mutate(NPP_sc_log = log(NPP_sc))

##Normality 
# Build the linear model
normality_test  <- lm(NPP_sc_log ~ severity*type*year,
                      data = NPPw)

# Shapiro test of normality: Normal 
shapiro_test(residuals(normality_test))


####SPLIT-SPLIT MODEL: Using aov() ####

NPP_sub_model <- aov(NPP_sc_log ~ severity*type*year +rep + Error(rep:severity/type/year), data = NPPw)
summary(NPP_sub_model)

out_year_severity_NPP_sc<- with(NPPw, LSD.test(NPP_sc_log, severity:year,72, 0.486, console = TRUE))


####################### BNPP  ###################################
##Equality of variance test for severity, type and year: Equal 
leveneTest(BNPP ~ year*type*severity, data = NPPw)

##Normality 
# Build the linear model
normality_test  <- lm(BNPP ~ severity*type*year,
                      data = NPPw)

# Shapiro test of normality: normal 
shapiro_test(residuals(normality_test))


####SPLIT-SPLIT MODEL: Using aov() ###################

BNPP_model <- aov(BNPP ~ severity*type*year +rep + Error(rep:severity/type/year), data = NPPw)
summary(BNPP_model)

out_year_severity_BNPP<- with(NPPw, LSD.test(BNPP, severity:year,72, 0.02328, console = TRUE))

#######################NPP total ###################################
##Equality of variance test for severity, type and year: Equal 
leveneTest(NPPw ~ year*type*severity, data = NPPw)

##Normality 
# Build the linear model
normality_test  <- lm(NPPw ~ severity*type*year,
                      data = NPPw)

# Shapiro test of normality: normal 
shapiro_test(residuals(normality_test))



##### SPLIT-SPLIT MODEL: Using aov() #####

NPPw_model <- aov(NPPw ~ severity*type*year +rep + Error(rep:severity/type/year), data = NPPw)
summary(NPPw_model)

out_year_severity_BNPP<- with(NPPw, LSD.test(NPPw, severity:year,72, 0.763, console = TRUE))


############################### CWD ##########################

####NPP canopy 
##Equality of variance test for severity, type and year: Equal 
leveneTest(R_CWD_Mghayr_ave ~ year*type*severity, data = R_CWD)

##Normality 
# Build the linear model
normality_test  <- lm(R_CWD_Mghayr_ave ~ severity*type*year,
                      data = R_CWD)

# Shapiro test of normality: Normal 
shapiro_test(residuals(normality_test))

R_CWD <- R_CWD%>%
  mutate(R_CWD_Mghayr_ave_log = log(R_CWD_Mghayr_ave))

##Normality 
# Build the linear model
normality_test  <- lm(R_CWD_Mghayr_ave_log ~ severity*type*year,
                      data = R_CWD)

# Shapiro test of normality: Normal 
shapiro_test(residuals(normality_test))


####SPLIT-SPLIT MODEL: Using aov() ######

R_CWD_model <- aov(R_CWD_Mghayr_ave_log ~ severity*type*year +rep + Error(rep:severity/type/year), data = R_CWD)
summary(R_CWD_model)

out_year_severity_R_CWD<- with(R_CWD, LSD.test(R_CWD_Mghayr_ave_log, rep,9, 2.009, console = TRUE))


############################### Rsh ##########################

####NPP canopy 
##Equality of variance test for severity, type and year: Equal 
leveneTest(Modeled_Rh_Mg_ha_y_mean ~ year*type*severity, data = R_sh)

##Normality 
# Build the linear model
normality_test  <- lm(Modeled_Rh_Mg_ha_y_mean ~ severity*type*year,
                      data = R_sh)

# Shapiro test of normality: Normal 
shapiro_test(residuals(normality_test))


####SPLIT-SPLIT MODEL: Using aov() ######

R_sh_model <- aov(Modeled_Rh_Mg_ha_y_mean ~ severity*type*year +rep + Error(rep:severity/type/year), data = R_sh)
summary(R_sh_model)




####################################NEP Calculations ##############################

########Create a large dataframe with all variables from 2019-2021 

R_CWD_summary <- R_CWD%>%
  select(rep, year, type, severity, R_CWD_Mghayr_ave)

R_sh_summary <- R_sh%>%
  select(rep, year, type, severity, Modeled_Rh_Mg_ha_y_mean)

NPPw_summary <- NPPw%>%
  select(rep, year, type, severity, ANPP, BNPP)%>%
  mutate(type = case_when(type == "Bottom" ~ "bottom", 
                          type == "Top" ~ "top"))

NPP_ll_summary <- NPP_ll%>%
  select(rep, year, type, severity, leafmass_MgChayr)%>%
  mutate(type = case_when(type == "Bottom" ~ "bottom", 
                          type == "Top" ~ "top"))

NPP_fwd_summary <- NPP_fwd%>%
  select(rep, year, type, severity, fwdmass_MgChayr)%>%
  mutate(type = case_when(type == "Bottom" ~ "bottom", 
                          type == "Top" ~ "top"))

NPP_fr_summary <- NPP_fr%>%
  select(rep, year, type, severity, root_mass_Mg_C_ha_yr_1)
  

NEP_dataframe <- merge(R_sh_summary, R_CWD_summary, by = c("severity", "type", "rep", "year"))
NEP_dataframe <-  merge(NEP_dataframe, NPP_fr_summary, by = c("severity", "type", "rep", "year"))
NEP_dataframe <-  merge(NEP_dataframe, NPP_fwd_summary, by = c("severity", "type", "rep", "year"))
NEP_dataframe <-  merge(NEP_dataframe, NPP_ll_summary, by = c("severity", "type", "rep", "year"))
NEP_dataframe <-  merge(NEP_dataframe, NPPw_summary, by = c("severity", "type", "rep", "year"))


##Make NA zero 
NEP_dataframe[is.na(NEP_dataframe)] = 0

#########Calculate NEP 

NEP_dataframe <- NEP_dataframe%>%
  mutate(NEP_MgChayr = (ANPP + BNPP + leafmass_MgChayr + fwdmass_MgChayr + root_mass_Mg_C_ha_yr_1) - (Modeled_Rh_Mg_ha_y_mean + R_CWD_Mghayr_ave))


ggplot(NEP_dataframe, aes(x = severity, y = NEP_MgChayr, fill = severity, group = severity)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("NEP (Mg C ha yr)") 

ggplot(NEP_dataframe, aes(x = type, y = NEP_MgChayr, fill = type, group = type)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("NEP (Mg C ha yr)") 


####NEP  
##Equality of variance test for severity, type and year: Equal 
leveneTest(NEP_MgChayr ~ year*type*severity, data = NEP_dataframe)

##Normality 
# Build the linear model
normality_test  <- lm(NEP_MgChayr ~ severity*type*year,
                      data = NEP_dataframe)

# Shapiro test of normality: Normal 
shapiro_test(residuals(normality_test))


####SPLIT-SPLIT MODEL: Using aov() ######

NEP_model <- aov(NEP_MgChayr ~ severity*type*year +rep + Error(rep:severity/type/year), data = NEP_dataframe)
summary(NEP_model)



  
