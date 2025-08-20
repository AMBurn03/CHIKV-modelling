##################CITY AND LOCATION SEROPOSITIVITY PLOTS
out.dir <- "C:/Users/antho/OneDrive - Imperial College London/Dissertation/EDA"
load(paste(out.dir,"/full_data.RData",sep=""))
full_data <- full_data %>% 
  mutate(DENG.1_INTERPRETATION = case_when(
    CHKG.1_INTERPRETATION == "Positivo" ~ 1,
    CHKG.1_INTERPRETATION == "Negativo" ~ 0)) %>%  
  mutate(DENG.1_INTERPRETATION = case_when(
    DENG.1_INTERPRETATION == "Positivo" ~ 1,
    DENG.1_INTERPRETATION == "Negativo" ~ 0))
###########
nrow(full_data)
##########
### LOCATION STRATIFIED CHKG
#########
full_data <- full_data %>% 
  mutate(CHKG.1_INTERPRETATION = case_when(
    CHKG.1_INTERPRETATION == "Positivo" ~ 1,
    CHKG.1_INTERPRETATION == "Negativo" ~ 0)) %>%  
  mutate(DENG.1_INTERPRETATION = case_when(
    DENG.1_INTERPRETATION == "Positivo" ~ 1,
    DENG.1_INTERPRETATION == "Negativo" ~ 0))
STRAT_data <- full_data %>% 
  select(CITY, REGION, SERIES,CHKG.1_INTERPRETATION)
STRAT_data <- STRAT_data[-which(is.na(STRAT_data$CHKG.1_INTERPRETATION)==TRUE),]

STRAT_data$SERIES <- ymd(STRAT_data$SERIES)
STRAT_data <- STRAT_data %>% 
  group_by(CITY, REGION,SERIES) %>% 
  summarise(POS = mean(CHKG.1_INTERPRETATION),
            N = n(),
            SE = sd(CHKG.1_INTERPRETATION)/n(),
            CI_LOWER = POS - qt(0.975, df = n()-1)*SE,
            CI_UPPER = POS + qt(0.975, df = n()-1)*SE)

### REMOVE INVALID CHARACTERS FOR THE PLOT
STRAT_data$REGION <- gsub("Administra√ß√£o ", "Administracao ", STRAT_data$REGION)
STRAT_data$REGION <- gsub("S√£o ", "", STRAT_data$REGION)
STRAT_data$REGION <- gsub("Microrregi√£o ", "Microrregiao ", STRAT_data$REGION)
STRAT_data$REGION <- gsub("√£", "a", STRAT_data$REGION)
STRAT_data$REGION <- gsub("√¥", "o", STRAT_data$REGION)
STRAT_data$REGION <- gsub("√°", "a", STRAT_data$REGION)
STRAT_data$REGION <- gsub("√©", "e", STRAT_data$REGION)

###
for(i in unique(STRAT_data$CITY)){
  plot.df <- STRAT_data[which(STRAT_data$CITY==i),]
  plot.df$REGION <- factor(plot.df$REGION)
  tmp.plot <- ggplot(plot.df, aes(x = SERIES, colour = REGION,
                                  shape = REGION)) +
    scale_shape_manual(values=1:nlevels(plot.df$REGION))+
    geom_point(aes(y = POS), size = 2, alpha = 1)+
    geom_line(aes(y = POS))+
    geom_errorbar(aes(ymin = CI_LOWER, ymax = CI_UPPER), width = 1)+
    labs(
      title = paste0("CHKG Seropositive Proportions in ", i),
      x = "Date",
      y = "Proportion Seropositive"
    ) +
    scale_x_date(breaks = unique(STRAT_data$SERIES))+
    scale_linetype_manual(values = rep(c("solid", "dashed", "dotted"), length.out = n_groups))+
    theme_bw()
  ggsave(file = file.path(paste(out.dir,"/Infection Plots",sep=""), paste0('CHKG_',i,'_SEROPOS_PLOT.png')), tmp.plot, w = 10, h = 16)}

##########
### LOCATION STRATIFIED DENG
#########
STRAT_data <- full_data %>% 
  select(CITY, REGION, SERIES,DENG.1_INTERPRETATION)
STRAT_data <- STRAT_data[-which(is.na(STRAT_data$DENG.1_INTERPRETATION)==TRUE),]

STRAT_data$SERIES <- ymd(STRAT_data$SERIES)
STRAT_data <- STRAT_data %>% 
  group_by(CITY, REGION,SERIES) %>% 
  summarise(POS = mean(DENG.1_INTERPRETATION),
            N = n(),
            SE = sd(DENG.1_INTERPRETATION)/n(),
            CI_LOWER = POS - qt(0.975, df = n()-1)*SE,
            CI_UPPER = POS + qt(0.975, df = n()-1)*SE)

### REMOVE INVALID CHARACTERS FOR THE PLOT
STRAT_data$REGION <- gsub("Administra√ß√£o ", "Administracao ", STRAT_data$REGION)
STRAT_data$REGION <- gsub("S√£o ", "", STRAT_data$REGION)
STRAT_data$REGION <- gsub("Microrregi√£o ", "Microrregiao ", STRAT_data$REGION)
STRAT_data$REGION <- gsub("√£", "a", STRAT_data$REGION)
STRAT_data$REGION <- gsub("√¥", "o", STRAT_data$REGION)
STRAT_data$REGION <- gsub("√°", "a", STRAT_data$REGION)
STRAT_data$REGION <- gsub("√©", "e", STRAT_data$REGION)



###
for(i in unique(STRAT_data$CITY)){
  plot.df <- STRAT_data[which(STRAT_data$CITY==i),]
  plot.df$REGION <- factor(plot.df$REGION)
  tmp.plot <- ggplot(plot.df, aes(x = SERIES, colour = REGION,
                                  shape = REGION)) +
    scale_shape_manual(values=1:nlevels(plot.df$REGION))+
    geom_point(aes(y = POS), size = 2, alpha = 1)+
    geom_line(aes(y = POS))+
    geom_errorbar(aes(ymin = CI_LOWER, ymax = CI_UPPER), width = 1)+
    labs(
      title = paste0("DENG Seropositive Proportions in ", i),
      x = "Date",
      y = "Proportion Seropositive"
    ) +
    scale_x_date(breaks = unique(STRAT_data$SERIES))+
    scale_linetype_manual(values = rep(c("solid", "dashed", "dotted"), length.out = n_groups))+
    theme_bw()
  ggsave(file = file.path(paste(out.dir,"/Infection Plots",sep=""), paste0('DENG_',i,'_SEROPOS_PLOT.png')), tmp.plot, w = 10, h = 16)}
STRAT_data[which(STRAT_data$REGION=="Administracao Regional Oitava"),]


##############
##### STRATIFY BY CITY
##############
STRAT_data <- full_data %>% 
  select(CITY, SERIES,CHKG.1_INTERPRETATION)
STRAT_data <- STRAT_data[-which(is.na(STRAT_data$CHKG.1_INTERPRETATION)==TRUE),]

STRAT_data$SERIES <- ymd(STRAT_data$SERIES)
STRAT_data <- STRAT_data %>% 
  group_by(CITY, SERIES) %>% 
  summarise(POS = mean(CHKG.1_INTERPRETATION),
            N = n(),
            SE = sd(CHKG.1_INTERPRETATION)/n(),
            CI_LOWER = POS - qt(0.975, df = n()-1)*SE,
            CI_UPPER = POS + qt(0.975, df = n()-1)*SE)

CHKG_CITY_PLOT <- ggplot(data= STRAT_data, aes(x = SERIES,
                             y = POS,
                             colour = CITY, group = CITY))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin = CI_LOWER, ymax = CI_UPPER), width = 0.2)+
  labs(x = 'Date', y = 'Proportion Seropositive', colour = "CITY") +
  scale_y_continuous(breaks = seq(0,1, 0.1))+
  scale_x_continuous(breaks = unique(STRAT_data$SERIES))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))+
  ggtitle("CHKG Seropositivity Stratified by City")
ggsave(file = file.path(paste(out.dir,"/Infection Plots",sep=""), 'CHKG_CITY_SEROPOS_PLOT.png'), CHKG_CITY_PLOT, w = 5, h = 8)
