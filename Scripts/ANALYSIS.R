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

forte_pal <- forte_colors()

##################################################### big dataframe ######################################################

NPP_total <- merge(NPPw, NPP_ll, by = c("year","rep", "severity", "type"))

NPP_total <- merge(NPP_total, NPP_fwd, by = c("year","rep", "severity", "type")) 

###Create a Rh total dataframe when Rhs is ready 
Rh_total <-R_CWD

#######################################Figures #####################################################
######################## Wood NPP ################################################################

##Convert variables 
NPP_total$severity <- as.factor(NPP_total$severity)
NPP_total$type <- as.factor(NPP_total$type)
NPP_total$year <- as.factor(NPP_total$year)
NPP_total$rep <- as.factor(NPP_total$rep)

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
ggplot(NPP_total, aes(x = severity, y = leafmass_MgChayr, group = severity, fill = severity)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values = forte_pal)+
  facet_grid(~year) +
  theme(axis.text.y = element_text(size = 25), axis.title.y = element_text(size = 30), axis.title.x = element_blank(),axis.text.x = element_blank(), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_blank()) +
  labs(x = "Severity", y=expression(paste(" ",NPP[ll]," (",MgC,"  ",ha^-2,"  ",yr^-1,")")))

###NPP_fwd severity 
ggplot(NPP_total, aes(x = severity, y = fwdmass_MgChayr, group = severity, fill = severity)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values = forte_pal)+
  facet_grid(~year)+
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_blank(), legend.text = element_text(size = 25), legend.title = element_blank()) +
  labs(x = "Severity", y=expression(paste(" ",NPP[fwd]," (",MgC,"  ",ha^-2,"  ",yr^-1,")")))

##NPP_ll severity 
ggplot(NPP_total, aes(x = type, y = leafmass_MgChayr, group = type, fill = type)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  facet_grid(~year) +
  theme(axis.text.y = element_text(size = 25), axis.title.y = element_text(size = 30), axis.title.x = element_blank(),axis.text.x = element_blank(), strip.text = element_text(size = 30), legend.position = "none") +
  labs(x = "Disturbance Type", y=expression(paste(" ",NPP[ll]," (",MgC,"  ",ha^-2,"  ",yr^-1,")")))

##NPP_fwd type 
ggplot(NPP_total, aes(x = type, y = fwdmass_MgChayr, group = type, fill = type)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  facet_grid(~year)+
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_blank(), legend.position = "none") +
  labs(x = "Disturbance Type", y=expression(paste(" ",NPP[fwd]," (",MgC,"  ",ha^-2,"  ",yr^-1,")")))


######################################### Fine-root NPP #####################################

########################################## CWD #########################################

###Create a Rh total dataframe when Rhs is ready 
Rh_total <-R_CWD


Rh_total$severity <- as.factor(Rh_total$severity)
Rh_total$type <- as.factor(Rh_total$type)
Rh_total$year <- as.factor(Rh_total$year)
Rh_total$rep <- as.factor(Rh_total$rep)

ggplot(Rh_total, aes(x = severity, y = R_CWD_Mghayr_ave, fill = severity, group = severity)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("R CWD (Mg C ha yr)") 

ggplot(Rh_total, aes(x = type, y = R_CWD_Mghayr_ave, fill = type, group = type)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("R CWD (Mg C ha yr)") 

ggplot(Rh_total, aes(x = rep, y = R_CWD_Mghayr_ave, fill = rep, group = rep)) +
  geom_boxplot() +
  facet_grid(~year) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("R CWD (Mg C ha yr)") 


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


