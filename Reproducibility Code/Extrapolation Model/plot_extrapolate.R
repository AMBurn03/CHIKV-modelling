library(ggplot2)
library(tidyverse)
library(geobr)
library(sf)
library(viridis)
library(posterior)
pi_extrapolate <- extrap_fit$summary(variables = c("pi_ext"),
measures = "median")
# pi_extrapolate <- extrap_fit$summary(
# variables = c("pi_ext"),
# quantiles =  ~quantile2(.,probs = c(0.95)) )

location_data <- readRDS("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Modelling/Extra Data Modelling/Extra Data/brazil_municipalities_2020.rds")
# location_data <- geobr::read_municipality(code_muni = "all", year = 2020)
# location_data$pi_ext <- pi_extrapolate$q95

location_data$pi_ext <- pi_extrapolate$measures


our_cities <- data.frame(CITY=unique(extrap_data_key$CITY))
## find all cities which we use to inform our extrapolation
setdiff(our_cities$CITY,location_data$name_muni)
our_cities <- our_cities %>% 
  mutate(CITY = case_when(
    CITY == "Brasilia" ~ "Brasília",
    CITY == "Cruzeiro do Sul" ~ "Cruzeiro Do Sul",
    CITY == "Feira de Santana" ~ "Feira De Santana",
    CITY == "Juazeiro do Norte" ~ "Juazeiro Do Norte",
    CITY == "Riachão do Jacuípe" ~"Riachão Do Jacuípe",
    CITY == "Rio de Janeiro" ~ "Rio De Janeiro",
    CITY == "São José do Rio Preto" ~ "São José Do Rio Preto",
    TRUE ~ CITY
  ))
setdiff(our_cities$CITY,location_data$name_muni)
location_data <- location_data %>%
  mutate(has_city_data = ifelse(name_muni %in% our_cities$CITY, 1, 0))

ggplot(location_data) +
  geom_sf(aes(fill = pi_ext), 
          color = NA, 
          size = 0.0000001)  +
  geom_sf(data = location_data %>% filter(has_city_data == 1),
          fill = "red",      # Dot color
          color = "black",     # Dot border
          size = 0.8,          # Dot size
          shape = 21)+
  scale_fill_viridis_c(
    name = "Extrapolated Prevalences",
    labels = scales::percent_format(),
    na.value = NA,
    option = "plasma"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right",
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.5, "cm")
  ) +
  labs(
    title = "Extrapolated Prevalences in Brazil",
    subtitle = "Using the Upper 95% Quantiles of Extrapolated Prevalence Values",
    x = "Longitude",
    y = "Latitude"
  )

