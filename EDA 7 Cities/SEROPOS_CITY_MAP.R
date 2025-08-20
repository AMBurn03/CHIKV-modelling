########
### SEROPOSITIVITY BY CITY, MAP
#######
library(ggplot2)
library(tidyverse)
library(geobr)
library(sf)
library(viridis)
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
  select(CITY, SERIES,CHKG.1_INTERPRETATION, LONGITUDE, LATITUDE)
STRAT_data <- STRAT_data[-which(is.na(STRAT_data$CHKG.1_INTERPRETATION)==TRUE),]
STRAT_data <- STRAT_data %>% 
  group_by(CITY, SERIES) %>% 
  reframe(POS = mean(CHKG.1_INTERPRETATION, na.rm = TRUE))
STRAT_data$city_code <- c(3106200, 3106200 ,
                          4106902, 4106902 ,
                          2304400,
                          1302603, 1302603 ,
                          2611606, 2611606 ,
                          3304557, 3304557 ,
                          3550308, 3550308 )

STRAT_data$CITY <- c(
  "Belo Horizonte", "Belo Horizonte",
  "Curitiba", "Curitiba",
  "Fortaleza",
  "Manaus", "Manaus",
  "Recife", "Recife",
  "Rio De Janeiro", "Rio De Janeiro",
  "São Paolo", "São Paolo"
)
STRAT_data.2023 <- STRAT_data[which(STRAT_data$SERIES == STRAT_data$SERIES[1]),]
STRAT_data.2024 <- STRAT_data[which(STRAT_data$SERIES == STRAT_data$SERIES[2]),]

# Get municipality data to find which states contain our cities
municipalities <- geobr::read_municipality(code_muni = "all", year = 2020)

# Merge POS data with municipality data 2023
cities_with_pos.2023 <- municipalities %>%
  inner_join(STRAT_data.2023, by = c("code_muni" = "city_code")) %>%
  select(code_state, abbrev_state, name_state, POS)

# Merge POS data with municipality data 2024
cities_with_pos.2024 <- municipalities %>%
  inner_join(STRAT_data.2024, by = c("code_muni" = "city_code")) %>%
  select(code_state, abbrev_state, name_state, POS)


states <- geobr::read_state(code_state = "all", year = 2020)
##labels for plot
# Calculate centroids for label placement
state_centroids <- st_centroid(states)
state_centroids <- cbind(state_centroids, st_coordinates(state_centroids))
# Aggregate to state level (if multiple cities in same state)
state_pos <- cities_with_pos.2023 %>%
  st_drop_geometry() %>%
  group_by(code_state, abbrev_state, name_state) %>%
  summarize(mean_POS = mean(POS, na.rm = TRUE),
            .groups = "drop")
# Merge with state boundaries
states_merged <- states %>%
  left_join(state_pos, by = c("code_state", "abbrev_state", "name_state"))

# Create the map
map.2023 <- ggplot() +
  # Plot all states first (white background)
  geom_sf(data = states, fill = "white", color = "gray70", size = 0.3) +
  
  # Plot states with POS data
  geom_sf(data = states_merged %>% filter(!is.na(mean_POS)),
          aes(fill = mean_POS), color = "gray40", size = 0.5) +
  
  # Color scale for POS values
  scale_fill_viridis(
    name = "Proportion Seropositive",
    option = "plasma",
    direction = -1,
    limits = c(0, 0.5),  # Adjust based on your data range
    na.value = "white"
  ) +
  
  # Add state abbreviations
  geom_sf_text(data = states,
               aes(label = abbrev_state),
               size = 3,
               color = "black") +
  
  # Add city points with values
  geom_sf(data = cities_with_pos.2023,
          color = "black", size = 2, shape = 21, fill = "white") +
  geom_sf_text(data = cities_with_pos.2023,
               aes(label = round(POS, 3)),
               nudge_y = -0.5,
               size = 3,
               color = "black") +
  
  # Theme and labels
  theme_bw() +
  labs(title = "Brazil: Proportion of Seropositive Individuals by City (Region) (November 2023)",
       subtitle = "States containing study cities shaded by estimated seropositive proportion",
       caption = "White states indicate no data available") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )
ggsave(file = file.path(paste(out.dir,"/Seropositive Maps",sep=""), 'CHKG_SEROPOS_MAP2023.png'), map.2023, w = 16, h = 9)



###################################

##### and for 2024


# Merge POS data with municipality data 2024
cities_with_pos.2024 <- municipalities %>%
  inner_join(STRAT_data.2024, by = c("code_muni" = "city_code")) %>%
  select(code_state, abbrev_state, name_state, POS)


states <- geobr::read_state(code_state = "all", year = 2020)
##labels for plot
# Calculate centroids for label placement
state_centroids <- st_centroid(states)
state_centroids <- cbind(state_centroids, st_coordinates(state_centroids))
# Aggregate to state level (if multiple cities in same state)
state_pos <- cities_with_pos.2024 %>%
  st_drop_geometry() %>%
  group_by(code_state, abbrev_state, name_state) %>%
  summarize(mean_POS = mean(POS, na.rm = TRUE),
            .groups = "drop")
# Merge with state boundaries
states_merged <- states %>%
  left_join(state_pos, by = c("code_state", "abbrev_state", "name_state"))

# Create the map
map.2024 <- ggplot() +
  # Plot all states first (white background)
  geom_sf(data = states, fill = "white", color = "gray70", size = 0.3) +
  
  # Plot states with POS data
  geom_sf(data = states_merged %>% filter(!is.na(mean_POS)),
          aes(fill = mean_POS), color = "gray40", size = 0.5) +
  
  # Color scale for POS values
  scale_fill_viridis(
    name = "Proportion Seropositive",
    option = "plasma",
    direction = -1,
    limits = c(0, 0.5),  # Adjust based on your data range
    na.value = "white"
  ) +
  
  # Add state abbreviations
  geom_sf_text(data = states,
               aes(label = abbrev_state),
               size = 3,
               color = "black") +
  
  # Add city points with values
  geom_sf(data = cities_with_pos.2024,
          color = "black", size = 2, shape = 21, fill = "white") +
  geom_sf_text(data = cities_with_pos.2024,
               aes(label = round(POS, 3)),
               nudge_y = -0.5,
               size = 3,
               color = "black") +
  
  # Theme and labels
  theme_bw() +
  labs(title = "Brazil: Proportion of Seropositive Individuals by City (Region) (June 2024)",
       subtitle = "States containing study cities shaded by estimated seropositive proportion",
       caption = "White states indicate no data available") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill ="white")
  )
ggsave(file = file.path(paste(out.dir,"/Seropositive Maps",sep=""), 'CHKG_SEROPOS_MAP2024.png'), map.2024, w = 16, h = 9)
