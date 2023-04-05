#######figures and ANOVA models for NEP FoRTE Assessment #######
library(ggplot2)
library(dplyr)
library(fortedata)
library(rstatix)
library(car)
library(agricolae)
library(gridExtra)

####Load CSV files from other R scripts 

NPPw <- read.csv("Figures_Output/NPP_total.csv")
NPP_ll <- read.csv("Figures_Output/leaf_litter.csv")%>%
  select(!treatment)
NPP_fwd <- read.csv("Figures_Output/fwd_litter.csv", na.strings = c("NA", "na"))%>%
  select(!treatment)
R_CWD <- read.csv("Figures_Output/R_CWD.csv")
R_sh <- read.csv("Figures_Output/Rsh_total.csv")
NPP_fr <- read.csv("Figures_Output/NPP_fr.csv")

forte_pal <- forte_colors()



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

NPP_fr$severity <- as.factor(NPP_fr$severity)
NPP_fr$type <- as.factor(NPP_fr$type)
NPP_fr$year <- as.factor(NPP_fr$year)
NPP_fr$rep <- as.factor(NPP_fr$rep)

R_sh$severity <- as.factor(R_sh$severity)
R_sh$type <- as.factor(R_sh$type)
R_sh$year <- as.factor(R_sh$year)
R_sh$rep <- as.factor(R_sh$rep)

R_CWD$severity <- as.factor(R_CWD$severity)
R_CWD$type <- as.factor(R_CWD$type)
R_CWD$year <- as.factor(R_CWD$year)
R_CWD$rep <- as.factor(R_CWD$rep)



######Getting rid of 2022 and summarize by severity 


##NPPw
NPPw_3yr <-  NPPw%>%
  filter(year != "2022")

NPPw_3yr <- NPPw_3yr%>%
  group_by(severity, rep, year)%>%
  summarize(NPP_can = mean(NPP_can), NPP_sc = mean(NPP_sc), NPP_can = mean(NPP_can), ANPP = mean(ANPP), BNPP = mean(BNPP), NPPw = mean(NPPw))

##NPPfr
NPP_fr <- NPP_fr%>%
  group_by(severity, rep, year)%>%
  summarize(NPP_fr = mean(root_mass_Mg_C_ha_yr_1_adj))

##NPPll
NPP_ll <- NPP_ll%>%
  group_by(severity, rep, year)%>%
  summarize(NPP_ll = mean(leafmass_MgChayr))

###NPPfwd
NPP_fwd <- NPP_fwd%>%
  group_by(severity, rep, year)%>%
  summarize(NPP_fwd = mean(fwdmass_MgChayr))

###CWD
R_CWD_3yr <-  R_CWD%>%
  filter(year != "2022")

R_CWD_3yr <-  R_CWD_3yr%>%
  group_by(severity, rep, year)%>%
  summarize(R_CWD_Mghayr_ave = mean(R_CWD_Mghayr_ave),R_CWD_Mghayr_1 = mean(R_CWD_Mghayr_1),R_CWD_Mghayr_2 = mean(R_CWD_Mghayr_2),R_CWD_Mghayr_3 = mean(R_CWD_Mghayr_3))

####Rsh 
R_sh_3yr <-  R_sh%>%
  group_by(severity, rep, year)%>%
  summarize(Rh_Mghayr_ave = mean(Modeled_Rh_Mg_ha_y_mean), Modeled_Rh_Mg_ha_y_BBL_ED = mean(Modeled_Rh_Mg_ha_y_BBL_ED), Modeled_Rh_Mg_ha_y_BBL_Jar = mean(Modeled_Rh_Mg_ha_y_BBL_Jar), Modeled_Rh_Mg_ha_y_subke_ED = mean(Modeled_Rh_Mg_ha_y_subke_ED), Modeled_Rh_Mg_ha_y_subke_Jar = mean(Modeled_Rh_Mg_ha_y_subke_Jar))

########################### Big NPP Figures_Severity  ##############################

NPP_can1 <- ggplot(NPPw_3yr, aes(x = severity, y = NPP_can, fill = severity, group = severity)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
 scale_y_continuous(labels = scales::number_format(accuracy = 0.1),sec.axis = dup_axis(name = NULL, labels = NULL)) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 35), axis.title.y = element_text(size = 40), strip.text = element_text(size = 40), axis.text.x = element_blank(), axis.title.x = element_blank(), legend.position = "none") +
  labs(y=expression(paste("",Canopy," ",ANPP[w],"")))

NPP_sc2 <- ggplot(NPPw_3yr, aes(x = severity, y = NPP_sc, fill = severity, group = severity)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL, labels = NULL)) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 35), axis.title.y = element_text(size = 40), axis.text.x = element_blank(), axis.title.x = element_blank(), legend.position = "none", strip.text = element_blank()) +
  labs( y=expression(paste("",Subcanopy," ",ANPP[w],"")))

BNPP3 <-ggplot(NPPw_3yr, aes(x = severity, y = BNPP, fill = severity, group = severity)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL, labels = NULL)) +
  theme_classic() +
  theme(axis.text = element_text(size = 35), axis.title = element_text(size = 40), legend.position = "none",strip.text = element_blank()) +
  labs(x = "Severity (%)", y=expression(paste(" ",BNPP[w]," ")))

NPP_ll4 <-ggplot(NPP_ll, aes(x = severity, y = NPP_ll, group = severity, fill = severity)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values = forte_pal)+
  facet_grid(~year) +
  theme(axis.text.y = element_text(size = 35), axis.title.y = element_text(size = 40), axis.text.x = element_blank(), axis.title.x = element_blank(), legend.position = "none", strip.text = element_text(size = 40) ) +
  scale_y_continuous(position = "right",sec.axis = dup_axis(name = NULL, labels = NULL)) +
  labs(x = "Severity", y=expression(paste(" ",NPP[ll],"")))

NPP_fwd5 <- ggplot(NPP_fwd, aes(x = severity, y = NPP_fwd, group = severity, fill = severity)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values = forte_pal)+
  facet_grid(~year)+
  theme(axis.text.y = element_text(size = 35), axis.title.y = element_text(size = 40), axis.text.x = element_blank(), axis.title.x = element_blank(), legend.position = "none",strip.text = element_blank()) +
  scale_y_continuous(position = "right",sec.axis = dup_axis(name = NULL, labels = NULL),breaks = seq(from = 0.5, to = 2, by = 0.5)) +
  labs(x = "Severity", y=expression(paste(" ",NPP[fwd]," ")))

NPP_fr6 <- ggplot(NPP_fr, aes(x = severity, y = NPP_fr, group = severity, fill = severity)) +
  theme_classic() +
  geom_boxplot() +
  scale_fill_manual(values = forte_pal)+
  facet_grid(~year) +
  theme(axis.text = element_text(size = 35), axis.title = element_text(size = 40), strip.text = element_blank(), legend.position = "none") +
  scale_y_continuous(position = "right",labels = scales::number_format(accuracy = 0.1),sec.axis = dup_axis(name = NULL, labels = NULL)) +
  labs(x = "Severity", y=expression(paste(" ",NPP[fr]," ")))


NPP_can_grob <- ggplotGrob(NPP_can1)
NPP_sc_grob <- ggplotGrob(NPP_sc2)
BNPP_grob <- ggplotGrob(BNPP3)
NPP_ll_grob <- ggplotGrob(NPP_ll4)
NPP_fwd_grob <- ggplotGrob(NPP_fwd5)
NPP_fr_grob <- ggplotGrob(NPP_fr6)


layout_NPP_severity <- rbind(c(1,4),
                             c(2,5),
                             c(3,6))

NPP_severity <- grid.arrange(NPP_can_grob, NPP_sc_grob,BNPP_grob,NPP_ll_grob,NPP_fwd_grob,NPP_fr_grob, layout_matrix=layout_NPP_severity)

ggsave(path = "Figures_Output", filename = "NPP_combined_severity.png", height = 20, width =20, units = "in", NPP_severity)




########################### Rh Combined Figure Severity ####################################


R_sh_severity <- ggplot(R_sh, aes(x = severity, y = Modeled_Rh_Mg_ha_y_mean, fill = severity, group = severity)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
  theme_classic() +
  theme(axis.text = element_text(size = 35), axis.title = element_text(size = 40), strip.text = element_blank(), legend.position = "none") +
  labs(x = "Severity (%)",y=expression(paste(" ",R[sh,mean]," (",Mg,"  ",C,"  ",ha^-1,"  ",yr^-1,")"))) 

R_CWD_severity <- ggplot(R_CWD_3yr, aes(x = severity, y = R_CWD_Mghayr_ave, fill = severity, group = severity)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 35), axis.title.y = element_text(size = 40), strip.text = element_text(size = 40), legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(0,0,0,-0.2)) +
  labs(x = "Severity (%)",y=expression(paste(" ",R[CWD,mean]," (",Mg,"  ",C,"  ",ha^-1,"  ",yr^-1,")"))) 




R_sh_severity_grob <- ggplotGrob(R_sh_severity)
R_CWD_severity_grob <- ggplotGrob(R_CWD_severity)

layout_Rh_severity <- rbind(c(1),
                             (2))
                   
Rh_severity <- grid.arrange(R_CWD_severity_grob, R_sh_severity_grob, layout_matrix=layout_Rh_severity)

ggsave(path = "Figures_Output", filename = "Rh_combined_severity.png", height = 20, width =15, units = "in", Rh_severity)

 #################################### Replicate ##########################
ggplot(NPPw_3yr, aes(x = rep, y = NPP_can, fill = rep, group = rep)) +
  geom_boxplot() +
  facet_grid(~year) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("R CWD (Mg C ha yr)") 

ggplot(NPP_ll, aes(x = rep, y = NPP_ll, fill = rep, group = rep)) +
  geom_boxplot() +
  facet_grid(~year) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("R CWD (Mg C ha yr)") 



ggplot(R_CWD, aes(x = rep, y = R_CWD_Mghayr_ave, fill = rep, group = rep)) +
  geom_boxplot() +
  facet_grid(~year) +
  theme_classic() +
  theme(axis.text = element_text(size = 25), axis.title = element_text(size = 30), strip.text = element_text(size = 30), legend.text = element_text(size = 25), legend.title = element_text(size = 30)) +
  ylab("R CWD (Mg C ha yr)") 

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
leveneTest(NPP_can ~ year*severity, data = NPPw_3yr)

##Normality 
# Build the linear model
normality_test  <- lm(NPP_can ~ severity*year,
                      data = NPPw_3yr)

# Shapiro test of normality: Normal 
shapiro_test(residuals(normality_test))


####SPLIT-SPLIT MODEL: Using aov() ####

NPP_can_model <- aov(NPP_can ~ severity*year +rep + Error(rep:severity/year), data = NPPw_3yr)
summary(NPP_can_model)

out_year_severity_NPP_can<- with(NPPw_3yr, LSD.test(NPP_can, severity:year,24, 0.264, console = TRUE))

#######################NPP subcanopy ###################################
##Equality of variance test for severity, type and year: Equal 
leveneTest(NPP_sc ~ year*severity, data = NPPw_3yr)

##Normality 
# Build the linear model
normality_test  <- lm(NPP_sc ~ severity*year,
                      data = NPPw_3yr)

# Shapiro test of normality: Not normal 
shapiro_test(residuals(normality_test))

#####Log transformation 

NPPw_3yr <- NPPw_3yr%>%
  mutate(NPP_sc_log = log(NPP_sc))

##Normality 
# Build the linear model
normality_test  <- lm(NPP_sc_log ~ severity*year,
                      data = NPPw_3yr)

# Shapiro test of normality: Normal 
shapiro_test(residuals(normality_test))


####SPLIT-SPLIT MODEL: Using aov() ####

NPP_sub_model <- aov(NPP_sc_log ~ severity*year +rep + Error(rep:severity/year), data = NPPw_3yr)
summary(NPP_sub_model)

out_year_severity_NPP_sc<- with(NPPw_3yr, LSD.test(NPP_sc_log, severity:year,24, 0.159, console = TRUE))


####################### BNPP  ###################################
##Equality of variance test for severity, type and year: Equal 
leveneTest(BNPP ~ year*severity, data = NPPw_3yr)

##Normality 
# Build the linear model
normality_test  <- lm(BNPP ~ severity*year,
                      data = NPPw_3yr)

# Shapiro test of normality: normal 
shapiro_test(residuals(normality_test))


####SPLIT-SPLIT MODEL: Using aov() ###################

BNPP_model <- aov(BNPP ~ severity*year +rep + Error(rep:severity/year), data = NPPw_3yr)
summary(BNPP_model)

out_year_severity_BNPP<- with(NPPw_3yr, LSD.test(BNPP, severity:year,24, 0.01651, console = TRUE))

#######################NPPw total ###################################
##Equality of variance test for severity, type and year: Equal 
leveneTest(NPPw ~ year*severity, data = NPPw_3yr)

##Normality 
# Build the linear model
normality_test  <- lm(NPPw ~ severity*year,
                      data = NPPw_3yr)

# Shapiro test of normality: normal 
shapiro_test(residuals(normality_test))


##### SPLIT-SPLIT MODEL: Using aov() #####

NPPw_model <- aov(NPPw ~ severity*year +rep + Error(rep:severity/year), data = NPPw_3yr)
summary(NPPw_model)

out_year_severity_BNPP<- with(NPPw, LSD.test(NPPw, severity:year,72, 0.763, console = TRUE))


#######################NPP_fr total ###################################
##Equality of variance test for severity, type and year: Equal 
leveneTest(NPP_fr ~ year*severity, data = NPP_fr)

##Normality 
# Build the linear model
normality_test  <- lm(NPP_fr ~ severity*year,
                      data = NPP_fr)

# Shapiro test of normality: normal 
shapiro_test(residuals(normality_test))


##### SPLIT-SPLIT MODEL: Using aov() #####

NPP_fr_model <- aov(NPP_fr ~ severity*year +rep + Error(rep:severity/year), data = NPP_fr)
summary(NPP_fr_model)

out_year_severity_fr<- with(NPP_fr, LSD.test(NPP_fr, severity:year,24, 0.292, console = TRUE))



#######################NPP_ll total ###################################
##Equality of variance test for severity, type and year: Equal 
leveneTest(NPP_ll ~ year*severity, data = NPP_ll)

##Normality 
# Build the linear model
normality_test  <- lm(NPP_ll ~ severity*year,
                      data = NPP_ll)

# Shapiro test of normality: normal 
shapiro_test(residuals(normality_test))


##### SPLIT-SPLIT MODEL: Using aov() #####

NPP_ll_model <- aov(NPP_ll ~ severity*year +rep + Error(rep:severity/year), data = NPP_ll)
summary(NPP_ll_model)

out_year_severity_ll<- with(NPP_ll, LSD.test(NPP_ll, severity:year,24, 0.0157, console = TRUE))


#######################NPP_fwd total ###################################
##Equality of variance test for severity, type and year: Equal 
leveneTest(NPP_fwd ~ year*severity, data = NPP_fwd)

##Normality 
# Build the linear model
normality_test  <- lm(NPP_fwd ~ severity*year,
                      data = NPP_fwd)

# Shapiro test of normality: normal 
shapiro_test(residuals(normality_test))

NPP_fwd <- NPP_fwd%>%
  mutate(NPP_fwd_log = log(NPP_fwd))


##### SPLIT-SPLIT MODEL: Using aov() #####

NPP_fwd_model <- aov(NPP_fwd_log ~ severity*year +rep + Error(rep:severity/year), data = NPP_fwd)
summary(NPP_fwd_model)

out_year_severity_fwd<- with(NPP_fwd, LSD.test(NPP_fwd_log, severity:year,24, 0.2752, console = TRUE))





############################### CWD ##########################


##Equality of variance test for severity, type and year: Equal 
leveneTest(R_CWD_Mghayr_ave ~ year*severity, data = R_CWD_3yr)

##Normality 
# Build the linear model
normality_test  <- lm(R_CWD_Mghayr_ave ~ severity*year,
                      data = R_CWD_3yr)

# Shapiro test of normality: Normal 
shapiro_test(residuals(normality_test))

R_CWD_3yr <- R_CWD_3yr%>%
  mutate(R_CWD_Mghayr_ave_log = log(R_CWD_Mghayr_ave))

##Normality 
# Build the linear model
normality_test  <- lm(R_CWD_Mghayr_ave_log ~ severity*year,
                      data = R_CWD_3yr)

# Shapiro test of normality: Normal 
shapiro_test(residuals(normality_test))


####SPLIT-SPLIT MODEL: Using aov() ######

R_CWD_model <- aov(R_CWD_Mghayr_ave_log ~ severity*year +rep + Error(rep:severity/year), data = R_CWD_3yr)
summary(R_CWD_model)

out_year_severity_R_CWD<- with(R_CWD_3yr, LSD.test(R_CWD_Mghayr_ave_log, year,24, 0.03084, console = TRUE))


############################### Rsh ##########################
R_sh_3yr <-  R_sh%>%
  group_by(severity, rep, year)%>%
  summarize(Rh_Mghayr_ave = mean(Modeled_Rh_Mg_ha_y_mean))

##Equality of variance test for severity, type and year: Equal 
leveneTest(Rh_Mghayr_ave ~ year*severity, data = R_sh_3yr )

##Normality 
# Build the linear model
normality_test  <- lm(Rh_Mghayr_ave ~ severity*year,
                      data = R_sh_3yr )

# Shapiro test of normality: Normal 
shapiro_test(residuals(normality_test))


####SPLIT-SPLIT MODEL: Using aov() ######

R_sh_model <- aov(Rh_Mghayr_ave ~ severity*year +rep + Error(rep:severity/year), data = R_sh_3yr)
summary(R_sh_model)


out_year_severity_rsh<- with(R_sh_3yr, LSD.test(Rh_Mghayr_ave, severity:year,24, 0.533, console = TRUE))


####################################NEP Calculations ##############################

########Create a large dataframe with all variables from 2019-2021 


NEP_dataframe <- merge(R_sh_3yr, R_CWD_3yr, by = c("severity", "rep", "year"))
NEP_dataframe <-  merge(NEP_dataframe, NPP_fr, by = c("severity",  "rep", "year"))
NEP_dataframe <-  merge(NEP_dataframe, NPP_fwd, by = c("severity", "rep", "year"))
NEP_dataframe <-  merge(NEP_dataframe, NPP_ll, by = c("severity",  "rep", "year"))
NEP_dataframe <-  merge(NEP_dataframe, NPPw_3yr, by = c("severity",  "rep", "year"))


##Make NA zero 
NEP_dataframe[is.na(NEP_dataframe)] = 0

#########Calculate NEP 

NEP_dataframe <- NEP_dataframe%>%
  mutate(NEP_MgChayr_1 = (ANPP + BNPP + NPP_ll + NPP_fwd + NPP_fr) - (Modeled_Rh_Mg_ha_y_BBL_ED + R_CWD_Mghayr_1))%>%
  mutate(NEP_MgChayr_2 = (ANPP + BNPP + NPP_ll + NPP_fwd + NPP_fr) - (Modeled_Rh_Mg_ha_y_BBL_Jar + R_CWD_Mghayr_1))%>%
  mutate(NEP_MgChayr_3 = (ANPP + BNPP + NPP_ll + NPP_fwd + NPP_fr) - (Modeled_Rh_Mg_ha_y_BBL_ED + R_CWD_Mghayr_2))%>%
  mutate(NEP_MgChayr_4 = (ANPP + BNPP + NPP_ll + NPP_fwd + NPP_fr) - (Modeled_Rh_Mg_ha_y_BBL_Jar + R_CWD_Mghayr_2))%>%
  mutate(NEP_MgChayr_5 = (ANPP + BNPP + NPP_ll + NPP_fwd + NPP_fr) - (Modeled_Rh_Mg_ha_y_BBL_ED + R_CWD_Mghayr_3))%>%
  mutate(NEP_MgChayr_6 = (ANPP + BNPP + NPP_ll + NPP_fwd + NPP_fr) - (Modeled_Rh_Mg_ha_y_BBL_Jar + R_CWD_Mghayr_3))%>%
  mutate(NEP_MgChayr_7 = (ANPP + BNPP + NPP_ll + NPP_fwd + NPP_fr) - (Modeled_Rh_Mg_ha_y_subke_ED + R_CWD_Mghayr_1))%>%
  mutate(NEP_MgChayr_8 = (ANPP + BNPP + NPP_ll + NPP_fwd + NPP_fr) - (Modeled_Rh_Mg_ha_y_subke_Jar + R_CWD_Mghayr_1))%>%
  mutate(NEP_MgChayr_9 = (ANPP + BNPP + NPP_ll + NPP_fwd + NPP_fr) - (Modeled_Rh_Mg_ha_y_subke_ED + R_CWD_Mghayr_2))%>%
  mutate(NEP_MgChayr_10 = (ANPP + BNPP + NPP_ll + NPP_fwd + NPP_fr) - (Modeled_Rh_Mg_ha_y_subke_Jar + R_CWD_Mghayr_2))%>%
  mutate(NEP_MgChayr_11 = (ANPP + BNPP + NPP_ll + NPP_fwd + NPP_fr) - (Modeled_Rh_Mg_ha_y_subke_ED + R_CWD_Mghayr_3))%>%
  mutate(NEP_MgChayr_12 = (ANPP + BNPP + NPP_ll + NPP_fwd + NPP_fr) - (Modeled_Rh_Mg_ha_y_subke_Jar + R_CWD_Mghayr_3))%>%
  mutate(NEP_MgChayr_grandmean = (NEP_MgChayr_1 +NEP_MgChayr_2 +NEP_MgChayr_3 +NEP_MgChayr_4 +NEP_MgChayr_5 + NEP_MgChayr_6 +NEP_MgChayr_7 +NEP_MgChayr_8 +NEP_MgChayr_9 +NEP_MgChayr_10 +NEP_MgChayr_11 + NEP_MgChayr_12)/12)
  
  


ggplot(NEP_dataframe, aes(x = severity, y = NEP_MgChayr_grandmean, fill = severity, group = severity)) +
  geom_boxplot() +
  facet_grid(~year) +
  scale_fill_manual(values =forte_pal) +
  theme_classic() +
  theme(axis.text = element_text(size = 35), axis.title = element_text(size = 40), strip.text = element_text(size = 30), legend.position = "none") +
    scale_y_continuous(sec.axis = dup_axis(name = NULL, labels = NULL), breaks = seq(from = -4, to = 3, by = 2)) +
  ylab("NEP (Mg C ha yr)") 

ggsave(path = "Figures_Output", filename = "NEP_severity.png", height = 10, width =15, units = "in")



NEP_dataframe$severity <- as.factor(NEP_dataframe$severity)
NEP_dataframe$year <- as.factor(NEP_dataframe$year)
NEP_dataframe$rep <- as.factor(NEP_dataframe$rep)

####NEP  
##Equality of variance test for severity, type and year: Equal 
leveneTest(NEP_MgChayr_grandmean ~ year*severity, data = NEP_dataframe)

##Normality 
# Build the linear model
normality_test  <- lm(NEP_MgChayr_grandmean ~ severity*year,
                      data = NEP_dataframe)

# Shapiro test of normality: Normal 
shapiro_test(residuals(normality_test))


####SPLIT-SPLIT MODEL: Using aov() ######

NEP_model <- aov(NEP_MgChayr_grandmean ~ severity*year +rep + Error(rep:severity/year), data = NEP_dataframe)
summary(NEP_model)

out_year_severity_NEP<- with(NEP_dataframe, LSD.test(NEP_MgChayr_grandmean, severity:year,24, 1.110, console = TRUE))


###########################Assess differences in pre-disturbance biomass to NEP 

pre_disturbance_biomass <- read.csv("Data_NPP/FoRTE_all_trees.csv")

pre_disturbance_biomass_summary <- pre_disturbance_biomass%>%
  select(SubplotID, dbh, Species)%>%
  mutate(a = case_when(
    Species == "ACPE" ~ 0.03117,
    Species == "ACRU" ~ 0.03177,
    Species == "ACSA" ~ 0.1693,
    Species == "AMEL" ~ 0.1630,
    Species == "BEPA" ~ 0.0301,
    Species == "FAGR" ~ 0.1892,
    Species == "PIRE" ~ 0.0526,
    Species == "PIST" ~ 0.0408,
    Species == "POGR" ~ 0.1387,
    Species == "POTR" ~ 0.0589,
    Species == "QURU" ~ 0.0398,
    Species == "ABBA" ~ 0.0705,
    Species == "TSCA" ~ 0.1617
  )) %>% 
  mutate(b = case_when(
    Species == "ACPE" ~ 2.7780,
    Species == "ACRU" ~ 2.7780,
    Species == "ACSA" ~ 2.3436,
    Species == "AMEL" ~ 2.4940,
    Species == "BEPA" ~ 2.8387,
    Species == "FAGR" ~ 2.3097,
    Species == "PIRE" ~ 2.5258,
    Species == "PIST" ~ 2.5735,
    Species == "POGR" ~ 2.3498,
    Species == "POTR" ~ 2.6235,
    Species == "QURU" ~ 2.7734,
    Species == "ABBA" ~ 2.4970,
    Species == "TSCA" ~ 2.1536
  ))%>%
  mutate(biomass = case_when(
    Species == "AMEL" ~ (a*(dbh*10)^b)/1000,
    Species != "AMEL" ~ a*dbh^b
  )) %>%
  group_by(SubplotID) %>% 
  summarise(subplot_biomass_kg = sum(biomass))%>%
  mutate(biomass_kg_ha = subplot_biomass_kg*10)

###Add Severity and treatment to CWD Dataframe 


pre_disturbance_biomass_summary <- pre_disturbance_biomass_summary%>%
  rename(subplot_id = SubplotID)%>%
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
  ))%>%
  mutate(rep = case_when(subplot_id == "A01E" |subplot_id == "A02E" |subplot_id == "A03E" |subplot_id == "A04E" |subplot_id == "A01W" |subplot_id == "A02W" |subplot_id == "A03W"|subplot_id == "A04W" ~ "A",
                            subplot_id == "B01E" |subplot_id == "B02E" |subplot_id == "B03E" |subplot_id == "B04E" |subplot_id == "B01W" |subplot_id == "B02W" |subplot_id == "B03W" |subplot_id == "B04W" ~ "B", 
                            subplot_id == "C01E" |subplot_id == "C02E" |subplot_id == "C03E" |subplot_id == "C04E" |subplot_id == "C01W" |subplot_id == "C02W" |subplot_id == "C03W"|subplot_id == "C04W" ~ "C", 
                            subplot_id == "D01E" |subplot_id == "D02E" |subplot_id == "D03E" |subplot_id == "D04E" |subplot_id == "D01W" |subplot_id == "D02W" |subplot_id == "D03W"|subplot_id == "D04W" ~ "D"))%>%
  group_by(severity, rep)%>%
  summarize(biomass_kg_ha = mean(biomass_kg_ha))

NEP_dataframe_biomass <- NEP_dataframe%>%
  select(year, severity, rep, NEP_MgChayr_grandmean)

NEP_dataframe_biomass <- NEP_dataframe_biomass%>%
  inner_join(pre_disturbance_biomass_summary, by = c("rep", "severity"))
  
NEP_dataframe_biomass$severity <- as.numeric(NEP_dataframe_biomass$severity)

NEP_biomass_model <- lm(NEP_MgChayr_grandmean ~ severity*biomass_kg_ha + year, data = NEP_dataframe_biomass)

summary(NEP_biomass_model)

ggplot(NEP_dataframe_biomass, aes(x = biomass_kg_ha, y = NEP_MgChayr_grandmean, group = severity)) +
  geom_point() +
  geom_smooth(method = "lm")
