library(ggplot2)
library(dplyr)
library(lubridate)
### AGE-BAND AND SEX STRATIFIED SEROPOSITIVITY PLOTS
### import data (full_data) from data_prep_EDA.R
out.dir <- "C:/Users/antho/OneDrive - Imperial College London/Dissertation/EDA"
load(paste(out.dir,"/full_data.RData",sep=""))
full_data <- full_data %>% 
  mutate(CHKG.1_INTERPRETATION = case_when(
    CHKG.1_INTERPRETATION == "Positivo" ~ 1,
    CHKG.1_INTERPRETATION == "Negativo" ~ 0)) %>%  
  mutate(DENG.1_INTERPRETATION = case_when(
    DENG.1_INTERPRETATION == "Positivo" ~ 1,
    DENG.1_INTERPRETATION == "Negativo" ~ 0))


###################################################


##########
###AGE BAND STRATIFIED CHKG
#########
### first for CHKG
STRAT_data <- full_data %>% select(SEX, BIRTH_DATE,DONATION_DATE, CHKG.1_INTERPRETATION)
STRAT_data <- STRAT_data[-which(is.na(STRAT_data$CHKG.1_INTERPRETATION)==TRUE),]
STRAT_data <- STRAT_data %>% 
  mutate(DONATION_DATE = format(dmy(DONATION_DATE), "%d-%m-%Y")) %>% 
  mutate(BIRTH_DATE = format(dmy(BIRTH_DATE), "%d-%m-%Y")) %>% 
  mutate(AGE = floor(as.numeric(difftime(dmy(DONATION_DATE), dmy(BIRTH_DATE), units= "days")/365.25)))
STRAT_data <- STRAT_data[-which(is.na(STRAT_data$AGE)==TRUE),]
STRAT_data <- STRAT_data %>% 
  mutate(AGE_BAND = case_when(
    between(AGE, 16, 19) ~ "16-19 Years",
    between(AGE, 20, 29) ~"20-29 Years",
    between(AGE, 30, 39) ~ "30-39 Years",
    between(AGE, 40, 49) ~ "40-49 Years",
    between(AGE, 50, 59) ~ "50-59 Years",
    between(AGE, 60, 69) ~ "60-69 Years",
    AGE > 69 ~ ">69 Years"
  ))

STRAT_data$DONATION_DATE <- dmy(STRAT_data$DONATION_DATE)
STRAT_data$DONATION_DATE <- dmy(paste0("01-",format(STRAT_data$DONATION_DATE, "%m-%y")))

STRAT_data <- STRAT_data %>% 
  group_by(AGE_BAND,DONATION_DATE) %>% 
  summarise(POS = mean(CHKG.1_INTERPRETATION),
            SE = sd(CHKG.1_INTERPRETATION)/n(),
            CI_LOWER = POS - qt(0.975, df = n()-1)*SE,
            CI_UPPER = POS + qt(0.975, df = n()-1)*SE)
CHKG_AGE_PLOT <- ggplot(data= STRAT_data, aes(x = DONATION_DATE,
                             y = POS,
                             colour = AGE_BAND, group = AGE_BAND))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin = CI_LOWER, ymax = CI_UPPER), width =5)+
  labs(x = 'Date', y = 'Proportion Seropositive', colour = "AGE") +
  scale_y_continuous(breaks = seq(0,1, 0.01))+
  scale_x_continuous(breaks = unique(STRAT_data$DONATION_DATE))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))+
  ggtitle("CHKG Seropositivity Stratified by Age-Bands")
ggsave(file = file.path(paste(out.dir,"/Infection Plots",sep=""), 'CHKG_AGE_SEROPOS_PLOT.png'), CHKG_AGE_PLOT, w = 5, h = 8)


##########
###AGE BAND STRATIFIED DENG
#########
STRAT_data <- full_data %>% select(SEX, BIRTH_DATE,DONATION_DATE, DENG.1_INTERPRETATION)
STRAT_data <- STRAT_data[-which(is.na(STRAT_data$DENG.1_INTERPRETATION)==TRUE),]
STRAT_data <- STRAT_data %>% 
  mutate(DONATION_DATE = format(dmy(DONATION_DATE), "%d-%m-%Y")) %>% 
  mutate(BIRTH_DATE = format(dmy(BIRTH_DATE), "%d-%m-%Y")) %>% 
  mutate(AGE = floor(as.numeric(difftime(dmy(DONATION_DATE), dmy(BIRTH_DATE), units= "days")/365.25)))
STRAT_data <- STRAT_data[-which(is.na(STRAT_data$AGE)==TRUE),]
STRAT_data <- STRAT_data %>% 
  mutate(AGE_BAND = case_when(
    between(AGE, 16, 19) ~ "16-19 Years",
    between(AGE, 20, 29) ~"20-29 Years",
    between(AGE, 30, 39) ~ "30-39 Years",
    between(AGE, 40, 49) ~ "40-49 Years",
    between(AGE, 50, 59) ~ "50-59 Years",
    between(AGE, 60, 69) ~ "60-69 Years",
    AGE > 69 ~ ">69 Years"
  ))

STRAT_data$DONATION_DATE <- dmy(STRAT_data$DONATION_DATE)
STRAT_data$DONATION_DATE <- dmy(paste0("01-",format(STRAT_data$DONATION_DATE, "%m-%y")))

STRAT_data <- STRAT_data %>% 
  group_by(AGE_BAND,DONATION_DATE) %>% 
  summarise(POS = mean(DENG.1_INTERPRETATION),
            SE = sd(DENG.1_INTERPRETATION)/n(),
            CI_LOWER = POS - qt(0.975, df = n()-1)*SE,
            CI_UPPER = POS + qt(0.975, df = n()-1)*SE)
DENG_AGE_PLOT <- ggplot(data= STRAT_data, aes(x = DONATION_DATE,
                                              y = POS,
                                              colour = AGE_BAND, group = AGE_BAND))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = CI_LOWER, ymax = CI_UPPER), width =5)+
  labs(x = 'Date', y = 'Percent Seropositive', colour = "AGE") +
  scale_x_continuous(breaks = unique(STRAT_data$DONATION_DATE))+
  scale_y_continuous(breaks = seq(0,1, 0.05))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))+
  ggtitle("DENG Seropositivity Stratified by Age-Bands")
ggsave(file = file.path(paste(out.dir,"/Infection Plots",sep=""), 'DENG_AGE_SEROPOS_PLOT.png'), DENG_AGE_PLOT, w = 5, h = 8)

##########
###SEX STRATIFIED CHKG
#########
STRAT_data <- full_data %>% select(SEX, BIRTH_DATE,DONATION_DATE, CHKG.1_INTERPRETATION)
STRAT_data <- STRAT_data[-which(is.na(STRAT_data$CHKG.1_INTERPRETATION)==TRUE),]
STRAT_data <- STRAT_data %>% 
  mutate(DONATION_DATE = format(dmy(DONATION_DATE), "%d-%m-%Y")) %>% 
  mutate(BIRTH_DATE = format(dmy(BIRTH_DATE), "%d-%m-%Y"))
STRAT_data$DONATION_DATE <- dmy(STRAT_data$DONATION_DATE)
STRAT_data$DONATION_DATE <- dmy(paste0("01-",format(STRAT_data$DONATION_DATE, "%m-%y")))

STRAT_data <- STRAT_data %>% 
  group_by(SEX,DONATION_DATE) %>% 
  summarise(POS = mean(CHKG.1_INTERPRETATION),
            SE = sd(CHKG.1_INTERPRETATION)/n(),
            CI_LOWER = POS - qt(0.975, df = n()-1)*SE,
            CI_UPPER = POS + qt(0.975, df = n()-1)*SE)
STRAT_data <- STRAT_data %>% 
  ungroup() %>% 
  mutate(SEX = case_when(
    SEX == "M" ~ "Male",
    SEX == "F" ~ "Female"
  ))
CHKG_SEX_PLOT <- ggplot(data= STRAT_data, aes(x = DONATION_DATE,
                                              y = POS,
                                              colour = SEX, group = SEX))+
  geom_line()+
  geom_point()+
  scale_y_continuous(breaks = seq(0,1, 0.01))+
  scale_x_continuous(breaks = unique(STRAT_data$DONATION_DATE))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))+
  ggtitle("CHKG Seropositivity Stratified by Sex (7 Cities)")
ggsave(file = file.path(paste(out.dir,"/Infection Plots",sep=""), 'CHKG_SEX_SEROPOS_PLOT.png'), CHKG_SEX_PLOT, w = 5, h = 8)

##########
###SEX STRATIFIED DENG
#########
STRAT_data <- full_data %>% select(SEX, BIRTH_DATE,DONATION_DATE, DENG.1_INTERPRETATION)
STRAT_data <- STRAT_data[-which(is.na(STRAT_data$DENG.1_INTERPRETATION)==TRUE),]
STRAT_data <- STRAT_data %>% 
  mutate(DONATION_DATE = format(dmy(DONATION_DATE), "%d-%m-%Y")) %>% 
  mutate(BIRTH_DATE = format(dmy(BIRTH_DATE), "%d-%m-%Y"))
STRAT_data$DONATION_DATE <- dmy(STRAT_data$DONATION_DATE)
STRAT_data$DONATION_DATE <- dmy(paste0("01-",format(STRAT_data$DONATION_DATE, "%m-%y")))

STRAT_data <- STRAT_data %>% 
  group_by(SEX,DONATION_DATE) %>% 
  summarise(POS = mean(DENG.1_INTERPRETATION))

DENG_SEX_PLOT <- ggplot(data= STRAT_data, aes(x = DONATION_DATE,
                                              y = POS,
                                              colour = SEX, group = SEX))+
  geom_line()+
  labs(x = 'Date', y = 'Proportion Seropositive', colour = "SEX") +
  scale_y_continuous(breaks = seq(0,1, 0.01))+
  scale_x_continuous(breaks = unique(STRAT_data$DONATION_DATE))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))+
  ggtitle("DENG Seropositivity Stratified by Sex")
ggsave(file = file.path(paste(out.dir,"/Infection Plots",sep=""), 'DENG_SEX_SEROPOS_PLOT.png'), DENG_SEX_PLOT, w = 5, h = 8)


