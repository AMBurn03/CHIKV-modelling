###########
###### CITY SAMPLE SIZES 
###########
library(ggplot2)
library(dplyr)
library(lubridate)
#load data
out.dir <- "C:/Users/antho/OneDrive - Imperial College London/Dissertation/EDA"
load(paste(out.dir,"/full_data.RData",sep=""))
full_data <- full_data %>% 
  mutate(CHKG.1_INTERPRETATION = case_when(
    CHKG.1_INTERPRETATION == "Positivo" ~ 1,
    CHKG.1_INTERPRETATION == "Negativo" ~ 0)) %>%  
  mutate(DENG.1_INTERPRETATION = case_when(
    DENG.1_INTERPRETATION == "Positivo" ~ 1,
    DENG.1_INTERPRETATION == "Negativo" ~ 0))


STRAT_data <- full_data %>% 
  select(CITY, SERIES,CHKG.1_INTERPRETATION)
STRAT_data <- STRAT_data[-which(STRAT_data$CHKG.1_INTERPRETATION==""),]
STRAT_data <- STRAT_data %>% 
  group_by(CITY, SERIES) %>% 
  summarise(N = n())

ggplot(STRAT_data, aes(x =SERIES, y = N, colour = CITY)) +
  geom_col()+
  labs(
    title = "CHKG Seropositivity Test Sample Sizes",
    x = "Date",
    y = "Sample Size"
  ) +
  scale_x_date(breaks = unique(STRAT_data$SERIES))+
  theme_bw()


tmp.plot <- ggplot(STRAT_data, aes(x = SERIES, y = N, fill = CITY)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Date", y = "Sample Size, N", fill = "City",
       title = "CHKG Seropositivity Test Sample Sizes") +
  geom_hline(yintercept = 704, linetype = "dashed", color = "red") +
  annotate("text", x = min(STRAT_data$SERIES), y = 750, label = "N = 704", hjust = 0, color = "red") +
  geom_hline(yintercept = min(STRAT_data$N), linetype = "dashed", colour = "blue")+
  annotate("text", x = max(STRAT_data$SERIES), y = min(STRAT_data$N)-50, label = "N = 685", colour = "blue")+
  scale_x_date(breaks = unique(STRAT_data$SERIES))+
  theme_bw()
ggsave(file = file.path(paste(out.dir,"/Sample Size Plots",sep=""), 'CHKG_CITY_SAMPLESIZE_PLOT.png'), tmp.plot, w = 10, h = 10)



##########
### CHKG SEROPOS BY CITY PLOT
#########
STRAT_data <- full_data %>% 
  select(CITY, SERIES,CHKG.1_INTERPRETATION)
STRAT_data <- STRAT_data[-which(is.na(STRAT_data$CHKG.1_INTERPRETATION)==TRUE),]
STRAT_data <- STRAT_data %>% 
  group_by(CITY, SERIES) %>% 
  summarise(POS = mean(CHKG.1_INTERPRETATION, na.rm = TRUE))

tmp.plot2 <- ggplot(STRAT_data, aes(x = SERIES, y = POS, fill = CITY)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Date", y = "Proportion of Donors Seropositive", fill = "City",
       title = "CHKG Seropositivity Proportions") +
  scale_x_date(breaks = unique(STRAT_data$SERIES))+
  theme_bw()
    
ggsave(file = file.path(paste(out.dir,"/Infection Plots",sep=""), 'CHKG_CITY_SEROPOS_PLOT.png'), tmp.plot2, w = 10, h = 10)
    