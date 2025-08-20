library(ggplot2)
### get seromodel_long from serology_model_data_run.R
seromodel_long <- seromodel_data %>%
  pivot_longer(
    cols = c(BELOHORIZONTE, CURITIBA, FORTALEZA,
             MANAUS, RECIFE, RIODEJANEIRO, SAOPAOLO),  # Exclude non-one-hot columns
    names_to = "CITY",              # New column for original column names
    values_to = "value"                 # New column for 0/1 values
  ) %>%
  filter(value == 1) %>%                # Keep only rows where value is 1
  select(-value) %>% 
  pivot_longer(
    cols = c(X16.19.Years,X20.29.Years,X30.39.Years,
             X40.49.Years,X50.59.Years,X60.69.Years),
    names_to = "AGE_BAND",
    values_to = "value"
  ) %>% filter(value==1) %>% 
  select(-value)
###### AGE_BAND + CITY interaction
plot.df_margsex <- seromodel_long %>% 
  group_by(CITY, AGE_BAND) %>% 
  summarise(N = sum(N),
            N_POSITIVE = sum(N_POSITIVE))

ggplot(data = plot.df_margsex, aes(x = AGE_BAND, y = N_POSITIVE/N, fill = CITY)) + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(title = "Proportion Seropositive (Blood bank data)")+
  ylab("Seropositive Proportion")+
  xlab("Age Band")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1, size = 10))

#### AGE_BAND + SEX interaction
plot.df_margcity <- seromodel_long %>% 
  group_by(AGE_BAND, SEX) %>% 
  summarise(N = sum(N),
            N_POSITIVE = sum(N_POSITIVE))
View(plot.df_margcity)

ggplot(data = plot.df_margcity, aes(x = AGE_BAND, y = N_POSITIVE/N, fill = as.factor(SEX))) + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(title = "Proportion Seropositive (Blood bank data)",
       fill = "SEX")+
  ylab("Seropositive Proportion")+
  xlab("Age Band")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1, size = 10))


### SEX + CITY interaction

plot.df_margage <- seromodel_long %>% 
  group_by(CITY, SEX) %>% 
  summarise(N = sum(N),
            N_POSITIVE = sum(N_POSITIVE))

ggplot(data = plot.df_margage, aes(x = CITY, y = N_POSITIVE/N, fill = as.factor(SEX))) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(title = "Proportion Seropositive (Blood bank data)",
       fill = "SEX")+
  ylab("Seropositive Proportion")+
  xlab("City")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1, size = 10))

##### CITY + SERIES INTERACTION


plot.df_margagesex <- seromodel_data %>% 
  group_by(CITY, SERIES) %>% 
  summarise(N = sum(N),
            N_POSITIVE = sum(N_POSITIVE))

ggplot(data = plot.df_margagesex, aes(x = CITY, y = N_POSITIVE/N, fill = as.factor(SERIES))) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(title = "Proportion Seropositive 0=Nov23, 1=Jun24",
       fill = "DONATION DATE")+
  ylab("Seropositive Proportion")+
  xlab("City")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1, size = 10))
