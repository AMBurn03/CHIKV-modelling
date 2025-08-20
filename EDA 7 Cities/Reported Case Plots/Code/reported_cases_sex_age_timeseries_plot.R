### reported case

reported_CHKG <- readRDS("C:/Users/antho/OneDrive - Imperial College London/Ratmann, Oliver's files - Chikungunya/sinan_complete_chk.rds")
CITY_CODES <- read_excel("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Modelling/Reported Case Modelling/CITY_CODES.xlsx")
rep_model <- reported_CHKG
CITY_CODES <- CITY_CODES[,1:2]
colnames(CITY_CODES) <- c("CITY", "ID_MUNICIP")
# reported case data only stores first 6 numbers of state ID, so we must remove the last character of the CITY_CODES IDs (which are 7 characters)
CITY_CODES$ID_MUNICIP <- sub(".$", "", CITY_CODES$ID_MUNICIP) 
rep_model <- rep_model %>%
  inner_join(CITY_CODES, 
             by = "ID_MUNICIP")

# remove all observations from cities which are not in the serology dataset

rep_model <- rep_model %>%
  filter(CITY %in% c("São Paulo", "Rio de Janeiro", "Fortaleza",
                     "Manaus", "Recife", "Belo Horizonte", "Curitiba"))
rep_model <- rep_model %>% 
  select(CITY, age, CS_SEXO, DT_SIN_PRI)

### format age bands in reported data and offsett data
rep_model$age <- as.integer(rep_model$age)
rep_model <- rep_model %>% 
  mutate(AGE_BAND = case_when(
    between(age,  0, 9) ~ "<10 Years",
    between(age, 10, 19) ~ "10-19 Years",
    between(age, 20, 29) ~"20-29 Years",
    between(age, 30, 39) ~ "30-39 Years",
    between(age, 40, 49) ~ "40-49 Years",
    between(age, 50, 59) ~ "50-59 Years",
    between(age, 60, 69) ~ "60-69 Years",
    between(age, 70, 1000) ~ ">69 Years"
  )) %>% select(-age)
setDT(rep_model)
rep_model$DT_SIN_PRI <- as.Date(rep_model$DT_SIN_PRI)
### firstly make time series of cases for each sex
cumulative_by_sex <- rep_model[, .(daily_cases = .N), by = .(CITY, CS_SEXO, DT_SIN_PRI)] %>%
  .[order(CITY, CS_SEXO, DT_SIN_PRI)] %>%
  .[, cumulative_cases := cumsum(daily_cases), by = .(CITY, CS_SEXO)]
library(ggplot2)
library(scales)

# Create facet wrapped plot by city
ggplot(cumulative_by_sex, aes(x = DT_SIN_PRI, y = cumulative_cases, color = CS_SEXO)) +
  geom_line(size = 0.8) +
  facet_wrap(~ CITY, scales = "free_y", ncol = 3) +  # Adjust ncol as needed
  labs(
    title = "Cumulative Cases Over Time by Sex in Each City",
    x = "Date",
    y = "Cumulative Cases",
    color = "Sex"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold"),  # City names
    legend.position = "bottom"
  ) +
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y",
               limits = c(as.Date("2012-01-01"), NA)) +  scale_y_continuous(labels = comma_format()) +
  scale_color_manual(values = c("M" = "#1f77b4", "F" = "#ff7f0e", 
                                "Unknown" = "#2ca02c")) 


#### and now by age-bands

cumulative_by_age <- rep_model[, .(daily_cases = .N), by = .(CITY, AGE_BAND, DT_SIN_PRI)] %>%
  .[order(CITY, AGE_BAND, DT_SIN_PRI)] %>%
  .[, cumulative_cases := cumsum(daily_cases), by = .(CITY, AGE_BAND)]

# Create facet wrapped plot by city
ggplot(cumulative_by_age, aes(x = DT_SIN_PRI, y = cumulative_cases, color = AGE_BAND)) +
  geom_line(size = 0.8) +
  facet_wrap(~ CITY, scales = "free_y", ncol = 3) +  # Adjust ncol as needed
  labs(
    title = "Cumulative Cases Over Time by Age Band in Each City",
    x = "Date",
    y = "Cumulative Cases",
    color = "Age Band"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold"),  # City names
    legend.position = "bottom"
  ) +
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y",
               limits = c(as.Date("2012-01-01"), NA)) +  scale_y_continuous(labels = comma_format()) 
# +
  # scale_color_manual(values = c("M" = "#1f77b4", "F" = "#ff7f0e", 
                                # "Unknown" = "#2ca02c"))  # Adjust based on your CS_SEXO values
