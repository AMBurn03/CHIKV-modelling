library(tidyverse)
library(readxl)
library(lubridate)
library(geobr)
library(sf)
location_data <- geobr::read_municipality(code_muni = "all", year = 2020)
saveRDS(location_data, file = file.path(out.dir, "brazil_municipalities_2020.rds"))

data.dir <- "C:/Users/antho/OneDrive - Imperial College London/Dissertation/Modelling/Extra Data Modelling/Extra Data"
out.dir <- "C:/Users/antho/OneDrive - Imperial College London/Dissertation/Modelling/Extra Data Modelling/Extra Data"


location_data <- readRDS("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Modelling/Extra Data Modelling/Extra Data/brazil_municipalities_2020.rds")


#### Load municipality codes and population size data
reported_CHKG <- readRDS("C:/Users/antho/OneDrive - Imperial College London/Ratmann, Oliver's files - Chikungunya/sinan_complete_chk.rds")
CITY_CODES <- read_excel("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Modelling/Reported Case Modelling/CITY_CODES.xlsx")
population_census2022 <- readRDS("C:/Users/antho/OneDrive - Imperial College London/Ratmann, Oliver's files - Chikungunya/population_census2022.rds")
## remove rows containing unusable data (eg unknown mixes of symptomatic/asymptomatic and samples of exclusively homeless people etc.)
#### Load the new serology data
new_sm_data <- read_excel(paste(data.dir,"/CHIKV seroprevalence_Brazil_extra.xlsx", sep=""))
old_sm_data <- readRDS("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Modelling/Integrated Modelling/int_model_2_key.rds")

## only include data from blood banks and asymptomatic samples 
new_sm_data <- new_sm_data[c(which(new_sm_data$Population == "asymptomatic"),
                             which(new_sm_data$Population == "blood donor"),
                             which(new_sm_data$Population == "symptomatic/asymptomatic")),]

### assign dates to the data
## when the data is collected over a period, I assign the date to be the midpoint of the period
new_sm_data$Period <- c("2016-01-01", "2016-01-01","2015-01-01",
                        "2015-06-01", "2014-03-01", "2015-10-01",
                        "2016-04-01", "2017-01-01", "2017-06-01",
                        "2016-06-01", "2019-01-01", "2017-01-01",
                        "2016-01-01", "2017-01-01", "2017-01-01",
                        "2019-01-01", "2015-06-01", "2019-06-01",
                        "2021-01-01", "2016-06-01", "2018-01-01",
                        "2016-01-01", "2016-01-01","2016-06-01",
                        "2016-01-01", "2017-01-01", "2017-06-01",
                        "2019-01-01", "2018-01-01", "2018-01-01",
                        "2019-01-01","2021-01-01", "2017-01-01",
                        "2017-01-01", "2019-01-01", "2017-06-01",
                        "2018-06-01","2019-01-01")
new_sm_data[new_sm_data == "-"] <- "0"
new_sm_data <- new_sm_data %>% 
  select(Municipality,Female, Male, `Total participants (F+M)`, `Total positive`,         Period, `Testing method`)
## for cohort studies, only consider the earliest observation
new_sm_data <- new_sm_data %>%
  group_by(Municipality, `Total participants (F+M)`) %>%
  filter(Period == min(Period)) %>%
  ungroup()
colnames(new_sm_data) <- c("CITY", "N_FEMALE", "N_MALE", "N_TOTAL", "N_POSITIVE",
                           "SERIES", "TEST_TYPE")
new_sm_data$N_FEMALE <- as.integer(new_sm_data$N_FEMALE)
new_sm_data$N_MALE <- as.integer(new_sm_data$N_MALE)

## where specific number of males tested vs. number of females tested is not given, we discard the respective rows
new_sm_data <- new_sm_data[-c(9,11,12),]

##also there is no information online about the sensitivity/specificity of 
##"Hemagglutination inhibition assay (HIA)", so those studies are discarded

### sens/spec for ELISA https://www.euroimmun.com/documents/Indications/Infections/Ermerging-viruses-and-parasites/EI_293a_D_UK_A.pdf
### sens/spec for qRT-PCR and PRNT https://pmc.ncbi.nlm.nih.gov/articles/PMC5560405/
### sens/spec for Dual-path immunochromatographic platform https://www.sciencedirect.com/science/article/pii/S2589537022002085
### sens/spec for HIA not available, so we remove the relevant row
### ELISA test sensitivity=96.8%, specificity = 98% 
### qRT-PCR and PRNT test sensitivity = 98.4%, specificity = 100%
### Dual-path immunochromatographic platform sens. = 90.6%, spec. =97.2%
new_sm_data <- new_sm_data %>% 
  select(CITY, SERIES, N_FEMALE, N_MALE,N_TOTAL, N_POSITIVE, TEST_TYPE) %>% 
  filter(TEST_TYPE != "Hemagglutination inhibition assay (HIA)") %>% 
  mutate(
    SENSITIVITY = case_when(
      TEST_TYPE == "qRT-PCR and PRNT" ~ 0.984,
      TEST_TYPE == "Dual-path immunochromatographic platform" ~ 0.906,
      TRUE ~ 0.968  # Default to ELISA values
    ),
    SPECIFICITY = case_when(
      TEST_TYPE == "qRT-PCR and PRNT" ~ 1.00,
      TEST_TYPE == "Dual-path immunochromatographic platform" ~ 0.972,
      TRUE ~ 0.98   # Default to ELISA values
    )
  )

### there are some mistakes in the new_sm_data N_MALE and N_FEMALE observations, these can be fixed as they are correctly specified elsewhere in the excel document
# which(new_sm_data$N_FEMALE+new_sm_data$N_MALE != new_sm_data$N_TOTAL)
new_sm_data$N_TOTAL[2] <- 638
new_sm_data$N_FEMALE[16] <- 1339
new_sm_data$N_MALE[16] <- 642
new_sm_data$N_TOTAL[20] <- 1402
## age info is sporadically given, and so we choose to ignore for simplicity
## as previous inference showed it was not particularly important in sero-surveys
new_sm_data$AGE_BAND <- "Unknown"

## get the reported cases for each city now!!
## dates must be of same months as serology data, for each observation
## WE WILL USE DT_SIN_PRI AS DATE
##

### import region codes and merge to the dataset

rep_model <- reported_CHKG
rep_model$DT_SIN_PRI <- as.Date(rep_model$DT_SIN_PRI)
CITY_CODES <- read_excel(file.path(data.dir,"CITY_CODES.xlsx"))
CITY_CODES <- CITY_CODES[,1:3]
colnames(CITY_CODES) <- c("CITY", "ID_MUNICIP", "TOWN")
# reported case data only stores first 6 numbers of state ID, so we must remove the last character of the CITY_CODES IDs (which are 7 characters)
CITY_CODES$ID_FULL <- CITY_CODES$ID_MUNICIP
CITY_CODES$ID_MUNICIP <- sub(".$", "", CITY_CODES$ID_MUNICIP) 

rep_model <- rep_model %>%
  right_join(CITY_CODES, 
             by = "ID_MUNICIP") 


rep_model <- rep_model %>% 
  select(CITY, age, CS_SEXO, DT_SIN_PRI, TOWN, ID_FULL)

### format age bands in reported data and offsett data
rep_model$age <- "Unknown"  ## as we are ignoring age in the extra data (info scarcely available)

rep_model <- rep_model %>% 
  mutate(SEX = case_when(
    CS_SEXO == "F" ~ 0,
    CS_SEXO == "M" ~ 1)) %>% select(-CS_SEXO)
rep_model <- rep_model[-which(is.na(rep_model$SEX)),]

# Add N_REPORTED column to new_sm_data
###### make sure the city names are the same
###### Cruzeiro do Sul, Macapa, Ribeirão Preto, São Carlos all have no reported cases before the date of their respective sero-samples
rep_model <- rep_model %>%
  mutate(CITY = case_when(
    CITY == "Distrito Federal" ~ "Brasilia",
    CITY == "São José do Rio Preto" ~ "São Jose do Rio Preto",
    TRUE ~ CITY  # keeps all other values unchanged
  ))
rep_model <- rep_model %>% 
  filter(CITY %in% unique(new_sm_data$CITY) | TOWN %in% unique(new_sm_data$CITY)) %>%   mutate(CITY = ifelse(TOWN %in% unique(new_sm_data$CITY), TOWN, CITY))
new_sm_data <- new_sm_data %>%
  rowwise() %>%
  mutate(
    N_REPORTED = sum(rep_model$CITY == CITY & 
                       rep_model$DT_SIN_PRI < SERIES)
  ) %>%
  ungroup()
#### and now we will modify the form of the population offsetts

### need to get offsetts for each city age gender and merge with data and then make and fit model
CITY_CODES <- read_excel(file.path(data.dir,"CITY_CODES.xlsx"))
CITY_CODES <- CITY_CODES[,1:3]
colnames(CITY_CODES) <- c("CITY", "ID_MUNICIP", "TOWN")
population_census2022 <- readRDS("C:/Users/antho/OneDrive - Imperial College London/Ratmann, Oliver's files - Chikungunya/population_census2022.rds")
colnames(population_census2022)[1] <- "ID_MUNICIP"
population_census2022$ID_MUNICIP <- as.character(population_census2022$ID_MUNICIP)
pop_offs <- left_join(population_census2022, CITY_CODES, by = "ID_MUNICIP")
## join with location data 
tmp_coords <- st_coordinates(st_centroid(location_data$geom))
tmp_location <- location_data %>% 
  select(code_muni) %>% mutate(
    avg_x = tmp_coords[,1],
    avg_y = tmp_coords[,2],
    code_muni = as.character(code_muni)
  ) %>% rename(ID_MUNICIP = code_muni)
pop_offs <- left_join(pop_offs, tmp_location, by = "ID_MUNICIP")
# setdiff( unique(new_sm_data$CITY),unique(pop_offs$CITY))
pop_offs <- pop_offs %>%
  mutate(CITY = case_when(
    CITY == "Distrito Federal" ~ "Brasilia",
    CITY == "São José do Rio Preto" ~ "São Jose do Rio Preto",
    TRUE ~ CITY  # keeps all other values unchanged
  ))
pop_offs <- pop_offs %>% 
  filter(CITY %in% unique(new_sm_data$CITY) | TOWN %in% unique(new_sm_data$CITY)) %>%   mutate(CITY = ifelse(TOWN %in% unique(new_sm_data$CITY), TOWN, CITY))


pop_offs <- pop_offs %>% 
  group_by(CITY) %>% 
  summarise(POP_FEMALE = sum(population[sex == "F"], na.rm = TRUE),
            POP_MALE = sum(population[sex == "M"], na.rm = TRUE),
            POP_TOTAL = sum(population, na.rm = TRUE),
            AVG_X = mean(avg_x),
            AVG_Y = mean(avg_y),
            .groups = "drop")

## ready for merging
new_sm_data <- right_join(pop_offs, new_sm_data, by = c("CITY") ) 
new_sm_data <- new_sm_data %>% 
  mutate(PROP_MALE = N_MALE/(N_FEMALE+N_MALE))

#### incorporate original data, after getting it into the same form
tmp_location <- location_data %>% 
  select(name_muni) %>% mutate(
    avg_x = tmp_coords[,1],
    avg_y = tmp_coords[,2]) %>% 
  rename(CITY = name_muni)
tmp_old_data_df <- old_sm_data %>% 
  select(-AGE_BAND) %>% 
  group_by(CITY, SERIES, SEX) %>% 
  summarise(N_TOTAL = sum(N),
            N_POSITIVE = sum(N_POSITIVE),
            POP = sum(POP),
            N_REPORTED = sum(N_POS_rp), .groups="drop"
  ) %>% 
  group_by(CITY, SERIES) %>%
  summarise(POP_FEMALE = POP[SEX == "F"],
            POP_MALE = POP[SEX == "M"],
            N_FEMALE = N_TOTAL[SEX=="F"],
            N_MALE = N_TOTAL[SEX=="M"],
            POP_TOTAL = sum(POP),
            N_TOTAL = sum(N_TOTAL),
            N_POSITIVE = sum(N_POSITIVE),
            N_REPORTED = sum(N_REPORTED),
            PROP_MALE = N_MALE/(N_FEMALE+N_MALE),
            .groups="drop") %>% 
  mutate(AGE_BAND = "Unknown",
         TEST_TYPE = "Original 7 Cities",
         SENSITIVITY = 0.975,
         SPECIFICITY = 0.971) %>% 
  mutate(CITY = case_when(
    CITY == "BELOHORIZONTE" ~ "Belo Horizonte",
    CITY == "CURITIBA" ~ "Curitiba",
    CITY == "FORTALEZA" ~ "Fortaleza",
    CITY == "MANAUS" ~ "Manaus",
    CITY == "RECIFE" ~ "Recife",
    CITY == "RIODEJANEIRO" ~ "Rio De Janeiro",
    CITY == "SAOPAOLO" ~ "São Paulo",
    CITY == "Brasilia" ~ "Brasília",
    CITY == "São Jose do Rio Preto"~"São José do Rio Preto",  
    TRUE ~ CITY
  )) %>% left_join(tmp_location, by = "CITY") %>% 
  select(-geom) %>% 
  mutate(CITY = case_when(
    CITY == "Rio De Janeiro" ~ "Rio de Janeiro",
    TRUE ~ CITY
  )) %>% 
  rename(AVG_X = avg_x,
         AVG_Y = avg_y)
tmp_old_data_df <- tmp_old_data_df[-6,] # remove the 2024-06-01 Fortaleza (no data)
### now we want to encode this data and its parameters for use in a model
extra_data_key <- rbind(tmp_old_data_df, new_sm_data) %>% 
  arrange(CITY, SERIES)
extra_data_key <- extra_data_key %>% 
  mutate(CITY = case_when(
    CITY == "BELOHORIZONTE" ~ "Belo Horizonte",
    CITY == "CURITIBA" ~ "Curitiba",
    CITY == "FORTALEZA" ~ "Fortaleza",
    CITY == "MANAUS" ~ "Manaus",
    CITY == "RECIFE" ~ "Recife",
    CITY == "RIODEJANEIRO" ~ "Rio De Janeiro",
    CITY == "SAOPAOLO" ~ "São Paulo",
    CITY == "Brasilia" ~ "Brasília",
    CITY == "São Jose do Rio Preto"~"São José do Rio Preto",  
    TRUE ~ CITY
  ))### (optional) normalise scaling in X and Y
# extra_data_key <- extra_data_key %>% 
#   mutate(AVG_X_mean = mean(AVG_X),
#          AVG_Y_mean = mean(AVG_Y),
#          AVG_X_sd = sd(AVG_X),
#          AVG_Y_sd = sd(AVG_Y),
#          AVG_X = as.vector(scale(AVG_X)),
#          AVG_Y = as.vector(scale(AVG_Y)))
extra_data_key <- extra_data_key %>% 
  mutate(AVG_X = as.vector(AVG_X),
         AVG_Y = as.vector(AVG_Y))
## input the locations of each municipality we want to predict the seroprevalence of
location_data
pred_key <- location_data
pred_location <- tmp_location %>% select(CITY, avg_x, avg_y)

########### make a variable which checks if the city has less seropos than expected false positives, so a variable can be assigned with low density to reduce variance in rho

extra_data_key <- extra_data_key %>% 
  mutate(EXPECTED_FALSE_POS = N_TOTAL * (1 - SPECIFICITY) * ((N_TOTAL-N_POSITIVE)/N_TOTAL),
         NO_TRUE_CASES = ifelse( EXPECTED_FALSE_POS < N_POSITIVE , 0, 1))
id_nb <- which(extra_data_key$NO_TRUE_CASES==0)
id_po <- which(extra_data_key$NO_TRUE_CASES==1)
P_cities_nb <- length(unique(extra_data_key$CITY[id_nb]))
P_cities_po <- length(unique(extra_data_key$CITY[id_po]))

# ## trouble shooting
# extra_data_key <- extra_data_key %>% 
#   filter(N_POSITIVE>0)

extra_df <- extra_data_key
##### one-hot-encode cities, and dates
##### dates must be rounded to nearest year for implementation
## rounding dates
extra_df$SERIES <- as.Date(extra_df$SERIES)
extra_df$SERIES <- floor_date(extra_df$SERIES, "6 months") + 
  ifelse(day(extra_df$SERIES) >= 15 & month(extra_df$SERIES) %in% c(1,2,3,7,8,9), months(6), 0)
#####
## make sure CITY names match in extra_df

onehot_city <- data.frame(model.matrix(~extra_df$CITY-1,
                                       data = extra_df))
colnames(onehot_city) <- gsub("extra_df.CITY","", colnames(onehot_city))
onehot_series <- data.frame(model.matrix(~as.factor(extra_df$SERIES)-1,
                                         data = extra_df))
colnames(onehot_series) <- gsub("as.factor.extra_df.SERIES.","", colnames(onehot_series))

# series and proportion male (spm) interaction matrix
spm_inter <- onehot_series * extra_df$PROP_MALE

#### outbreak parameter, places with sero-pos rates greater than 0.15 are considered outbreaks
outbreak_city <- extra_df %>% group_by(CITY) %>% 
  summarise(POS_PROP = sum(N_POSITIVE)/sum(N_TOTAL)) %>% 
  mutate(OUTBREAK_THRESH = 0.15,
         OUTBREAK = as.integer(POS_PROP>OUTBREAK_THRESH))
outbreak_sm <- extra_df %>%
  mutate(POS_PROP = N_POSITIVE/N_TOTAL) %>% 
  mutate(OUTBREAK_THRESH = 0.15,
         OUTBREAK = as.integer(POS_PROP>OUTBREAK_THRESH)) %>% 
  select(OUTBREAK)
#### account for hospital access (median travel time to a hospital) and also surveillance rating.
surv_rating <- data.frame(read_excel(file.path(data.dir,"surveillance_rating.xlsx")))
hsptl_time <- data.frame(read_excel(file.path(data.dir,"hsptl_time.xlsx")))
surv_rating <- surv_rating[,c(1,2,4,6)]
surv_rating[,4] <- scale(surv_rating[,4])
colnames(surv_rating) <- c("ID","CITY","AREA", "surv_rating")
hsptl_time <- hsptl_time[,c(1,2,4,7)]
hsptl_time[,4] <- scale(hsptl_time[,4])
colnames(hsptl_time) <- c("ID","CITY","AREA", "hsptl_time")
tmp <- merge(surv_rating, hsptl_time, by = c("ID","CITY", "AREA"))
### some very small cities exist with the same names as the cities we are looking at, remove these smaller ones.
tmp_max <- tmp %>% 
  group_by(CITY) %>% 
  slice_max(AREA, n = 1) %>% 
  ungroup()

# setdiff("Braslia",tmp_max$CITY)
extra_df <- left_join(extra_df, tmp_max, by = "CITY")
hospital_times <- extra_df %>% select(CITY, hsptl_time) %>% 
  select(hsptl_time)
surv_ratings <- extra_df %>% select(CITY, surv_rating) %>% 
  select(surv_rating)
extra_df <- extra_df %>% 
  mutate(TEST_INDICATOR = ifelse(TEST_TYPE== "Original 7 Cities", 1, 0))

###### trying to have a city exclusive reporting rate:
extra_df_city <- extra_df %>% 
  group_by(CITY, POP_TOTAL) %>% 
  summarise(N_MALE = sum(N_MALE),
            N_FEMALE = sum(N_FEMALE),
            N_TOTAL = N_TOTAL,
            N_POSITIVE = sum(N_POSITIVE))
#### implement a basic model to check performance
extra_data <- list()
extra_data$N <- nrow(extra_df)
extra_data$P_cities <- ncol(onehot_city)
extra_data$P_dates <- ncol(onehot_series)
extra_data$X_cities <- onehot_city
extra_data$X_dates <- onehot_series
extra_data$X_prop_male <- extra_df$PROP_MALE
extra_data$population <- extra_df$POP_TOTAL
extra_data$sensitivity <- extra_df$SENSITIVITY
extra_data$specificity <- extra_df$SPECIFICITY
extra_data$N_sm <- extra_df$N_TOTAL
extra_data$y_sm <- extra_df$N_POSITIVE
extra_data$y_rp <- extra_df$N_REPORTED
extra_data$X_hospital_time_city <- as.numeric(hospital_times$hsptl_time)
extra_data$X_surv_rating_city <- as.numeric(surv_ratings$surv_rating)
extra_data$X_outbreak_city <- outbreak_city$OUTBREAK
extra_data$X_outbreak_sm <- outbreak_sm$OUTBREAK
extra_data$X_datepropmale_sm <- spm_inter
extra_data$nu_cities_rp <- 3
extra_data$group_indicator <- extra_df$NO_TRUE_CASES
extra_data$N_nb <- sum(1-extra_df$NO_TRUE_CASES)
extra_data$N_po <- sum(extra_df$NO_TRUE_CASES)
extra_data$id_nb <- id_nb
extra_data$id_po <- id_po
extra_data$P_cities_nb <- P_cities_nb
extra_data$P_cities_po <- P_cities_po
extra_data$X_test_type <- extra_df$TEST_INDICATOR
extra_data$X_latitude <- extra_df$AVG_X
extra_data$X_longitude <- extra_df$AVG_Y

extra_data$X_coords <- matrix(c(as.vector(extra_df$AVG_X),as.vector(extra_df$AVG_Y)), ncol = 2)

extra_data$Z_coords <- matrix(c(as.vector(pred_location$avg_x),
                                as.vector(pred_location$avg_y)),
                              ncol = 2)
extra_data$N_ext <- nrow(extra_data$Z_coords)
saveRDS(extra_data, file.path(out.dir,"extrap_inputdata.rds"))
saveRDS(extra_df, file.path(out.dir, "extrap_data_key.rds"))

