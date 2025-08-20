library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(MASS)
### import data (full_data) from data_prep_EDA.R
out.dir <- "C:/Users/antho/OneDrive - Imperial College London/Dissertation/EDA"
load(paste(out.dir,"/full_data.RData",sep=""))

chkg_data <- full_data[which(is.na(full_data$CHKG.1_RESULT)==FALSE),]
chkg_data <- chkg_data %>% 
  select(CITY, REGION, CHKG.1_DATE, CHKG.1_RESULT, CHKG.1_INTERPRETATION) %>% 
  mutate(CHKG.1_DATE = as_date(dmy_hm(CHKG.1_DATE)))

chkg_data

chkg1 <- ggplot(chkg_data, aes(x = CHKG.1_DATE, y = CHKG.1_RESULT, colour = CHKG.1_INTERPRETATION))+
  geom_point()+
  labs(
    title = "CHKG.1 Test and Positivity",
    x = "Date",
    y = "CHKG Antibody Assay Result",
    color = "Interpretted Infection Status"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 16, margin = margin(t = 10)),  # X-axis title
        axis.title.y = element_text(size = 16, margin = margin(r = 10)))+  # Y-axis title) 
  scale_x_date(breaks = "1 month")  # Only show dates in dataset
ggsave(file = file.path(paste(out.dir,"/Infection Plots",sep=""), 'CHKG1testresults.pdf'), chkg1, w = 5, h = 8)
########################### same but for dengue
###DENG1
###########################
DENG_data <- full_data[which(is.na(full_data$DENG.1_RESULT)==FALSE),]
DENG_data <- DENG_data %>% 
  select(CITY, REGION, DENG.1_DATE, DENG.1_RESULT, DENG.1_INTERPRETATION) %>% 
  mutate(DENG.1_DATE = as_date(dmy_hm(DENG.1_DATE)))

deng1 <- ggplot(DENG_data, aes(x = DENG.1_DATE, y = DENG.1_RESULT, colour = DENG.1_INTERPRETATION))+
  geom_point()+
  labs(
    title = "DENG.1 Test and Positivity",
    x = "Date",
    y = "DENG Antibody Assay Result",
    color = "Interpretted Infection Status"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 16, margin = margin(t = 10)),  # X-axis title
        axis.title.y = element_text(size = 16, margin = margin(r = 10)))+  # Y-axis title) 
  scale_x_date(breaks = "1 month")  # Only show dates in dataset
ggsave(file = file.path(paste(out.dir,"/Infection Plots",sep=""), 'DENG1testresults.pdf'), deng1, w = 5, h = 8)

##################
### Cross Plot for both, 0 indicates neither, 1 is chkg, 2, is deng, 3 is both
##################
CROSS_data <- full_data[which(is.na(full_data$DENG.1_RESULT)==FALSE),]
CROSS_data <- CROSS_data[which(is.na(full_data$CHKG.1_RESULT)==FALSE),]

CROSS_data <- CROSS_data %>% 
  select(CITY, REGION, CHKG.1_RESULT, CHKG.1_INTERPRETATION, DENG.1_RESULT, DENG.1_INTERPRETATION) %>% 
  mutate(CROSS = case_when(
    CHKG.1_INTERPRETATION == "Negativo" & DENG.1_INTERPRETATION == "Negativo" ~ "None",  # Both 0
    CHKG.1_INTERPRETATION == "Positivo" & DENG.1_INTERPRETATION == "Negativo" ~ "CHKG Positive",  # First 1, second 0
    CHKG.1_INTERPRETATION == "Negativo" & DENG.1_INTERPRETATION == "Positivo" ~ "DENG Positive",  # First 0, second 1
    CHKG.1_INTERPRETATION == "Positivo" & DENG.1_INTERPRETATION == "Positivo" ~ "BOTH Positive"
  )) %>% 
  filter(CROSS != "NA") 

CROSS <- ggplot(CROSS_data, aes(x = CHKG.1_RESULT, y = DENG.1_RESULT))+
  geom_point(aes(colour = CROSS))+
  labs(
    title = "Plot of Cross Infection Status",
    x = "CHKG Antibody Assay Result",
    y = "DENG Antibody Assay Result",
    color = "Interpretted Infection Status"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 16, margin = margin(t = 10)),  # X-axis title
        axis.title.y = element_text(size = 16, margin = margin(r = 10)))  # Y-axis title) 
ggsave(file = file.path(paste(out.dir,"/Infection Plots",sep=""), 'CROSStestresults.pdf'), CROSS, w = 5, h = 8)

### 3d density plot from https://www.youtube.com/watch?v=2zBpq6f5sJ8&ab_channel=TheDataDigest

CROSS_data$CHKG.1_RESULT
kd <- with(CROSS_data, MASS::kde2d(DENG.1_RESULT, CHKG.1_RESULT, n = 50))
plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()
## CHKG antibodies are MUCH more prevalent


### Percent Positive Plots
# convert the dates into month-year
CHKGpos_data <- full_data %>% 
  select(CITY, SERIES,
         CHKG.1_INTERPRETATION)
CHKGpos_data <- CHKGpos_data %>% 
  mutate(SERIES = format(ymd(SERIES), "%m-%Y"))
CHKGpos_data <- CHKGpos_data[-which(CHKGpos_data$CHKG.1_INTERPRETATION==""),]
CHKGpos_data <- CHKGpos_data %>%
  mutate(SERIES = factor(SERIES, levels = c("11-2023", "06-2024"), ordered = TRUE))
CHKGpos_data <- CHKGpos_data %>% 
  mutate(CHKG.1_INTERPRETATION = case_when(
    CHKG.1_INTERPRETATION == "Positivo" ~ 1,
    CHKG.1_INTERPRETATION == "Negativo" ~ 0)) %>% 
  group_by(CITY, SERIES) %>% 
  summarise(PERCENT_POS = mean(CHKG.1_INTERPRETATION, na.rm = TRUE)) %>% arrange(SERIES)
CHKGpos <- ggplot(data = CHKGpos_data, aes(colour = CITY, group = CITY))+
  geom_point(aes(x = SERIES, y = PERCENT_POS))+
  geom_line(aes(x = SERIES, y = PERCENT_POS))+
  labs(
    title = "Percentage of Donors Testing Positive for CHKG",
    x = "Date",
    y = "Percent Positive",
    color = "City"
  ) +
  scale_y_continuous(breaks= seq(0,0.5, 0.05))+
  theme_bw()
ggsave(file = file.path(paste(out.dir,"/Infection Plots",sep=""), 'CHKGpercentPositive.pdf'), CHKGpos, w = 5, h = 8)


### same for dengue::
# convert the dates into month-year
DENGpos_data <- full_data %>% 
  select(CITY, SERIES,
         DENG.1_INTERPRETATION)
DENGpos_data <- DENGpos_data %>% 
  mutate(SERIES = format(ymd(SERIES), "%m-%Y"))
DENGpos_data <- DENGpos_data[-which(DENGpos_data$DENG.1_INTERPRETATION==""),]
DENGpos_data <- DENGpos_data %>%
  mutate(SERIES = factor(SERIES, levels = c("11-2023", "06-2024"), ordered = TRUE))
DENGpos_data <- DENGpos_data %>% 
  mutate(DENG.1_INTERPRETATION = case_when(
    DENG.1_INTERPRETATION == "Positivo" ~ 1,
    DENG.1_INTERPRETATION == "Negativo" ~ 0)) %>% 
  group_by(CITY, SERIES) %>% 
  summarise(PERCENT_POS = mean(DENG.1_INTERPRETATION, na.rm = TRUE)) %>% arrange(SERIES)
DENGpos <- ggplot(data = DENGpos_data, aes(colour = CITY, group = CITY))+
  geom_point(aes(x = SERIES, y = PERCENT_POS))+
  geom_line(aes(x = SERIES, y = PERCENT_POS))+
  labs(
    title = "Percentage of Donors Testing Positive for DENG",
    x = "Date",
    y = "Percent Positive",
    color = "City"
  ) +
  scale_y_continuous(breaks= seq(0,1, 0.05))+
  theme_bw()
ggsave(file = file.path(paste(out.dir,"/Infection Plots",sep=""), 'DENGpercentPositive.pdf'), DENGpos, w = 5, h = 8)

### Now do the same, marginalising over all cities and stratifying by 3 age bands and sex
### first for CHKG
STRAT_data <- full_data %>% select(SEX, BIRTH_DATE,DONATION_DATE, CHKG.1_INTERPRETATION)
STRAT_data <- STRAT_data[-which(STRAT_data$CHKG.1_INTERPRETATION==""),]
STRAT_data <- STRAT_data %>% 
  mutate(DONATION_DATE = format(dmy(DONATION_DATE), "%d-%m-%Y")) %>% 
  mutate(BIRTH_DATE = format(dmy(BIRTH_DATE), "%d-%m-%Y")) %>% 
  mutate(AGE = floor(as.numeric(difftime(dmy(DONATION_DATE), dmy(BIRTH_DATE), units= "days")/365.25)))
STRAT_data <- STRAT_data[-which(is.na(STRAT_data$AGE)==TRUE),]
max(STRAT_data$AGE)
min(STRAT_data$AGE)
sum(STRAT_data$AGE<18)
hist(STRAT_data$AGE)
STRAT_data <- STRAT_data %>% 
  mutate(AGE_BAND = case_when(
    AGE < 25 ~ "<25 Years",
    between(AGE, 25, 40) ~"25-40 Years",
    between(AGE, 41, 100) ~ ">40 Years"
  ))
STRAT_data <- STRAT_data %>% 
  group_by(AGE_BAND)
  
ggplot(data = STRAT_data, )
