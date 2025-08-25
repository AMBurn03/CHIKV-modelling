### integrated model data prep
set_cmdstan_path(file.path("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Modelling", "cmdstan-2.36.0"))
## load data:
data.dir <- "C:/Users/antho/OneDrive - Imperial College London/Dissertation/Reproducability Code/7 Cities Final Model and Data"
out.dir <- "C:/Users/antho/OneDrive - Imperial College London/Dissertation/Reproducability Code/7 Cities Final Model and Data"
rp_key <- readRDS(file.path(data.dir,"rp_key.rds"))
rep_model_data <- readRDS("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Modelling/Reported Case Modelling/rep_model_input_data.rds")
load(paste(data.dir,"/full_data.RData",sep=""))
full_data <- full_data %>% 
  mutate(CHKG.1_INTERPRETATION = case_when(
    CHKG.1_INTERPRETATION == "Positivo" ~ 1,
    CHKG.1_INTERPRETATION == "Negativo" ~ 0))

seromodel_data <- full_data[-which(is.na(full_data$CHKG.1_INTERPRETATION)==TRUE),] %>% 
  select(CHKG.1_INTERPRETATION, CITY, SEX, SERIES, BIRTH_DATE, DONATION_DATE)
# sum(is.na(seromodel_data)) = 0, nice!
### calculate age bands using BIRTH_DATE and DONATION_DATE
seromodel_data <- seromodel_data %>% 
  mutate(BIRTH_DATE = dmy(BIRTH_DATE),
         DONATION_DATE = dmy(DONATION_DATE),
         AGE = round(time_length(interval(BIRTH_DATE,DONATION_DATE),"years"),digits=0))
seromodel_data <- seromodel_data %>% 
  mutate(AGE_BAND = case_when(
    between(AGE,  0, 15) ~ "<16 Years",
    between(AGE, 16, 19) ~ "16-19 Years",
    between(AGE, 20, 29) ~"20-29 Years",
    between(AGE, 30, 39) ~ "30-39 Years",
    between(AGE, 40, 49) ~ "40-49 Years",
    between(AGE, 50, 59) ~ "50-59 Years",
    between(AGE, 60, 69) ~ "60-69 Years",
    between(AGE, 70, 1000) ~ ">69 Years"
  )) %>% select(-c(BIRTH_DATE, DONATION_DATE))
# remove the one observation with no recorded age
seromodel_data <- seromodel_data[-which(is.na(seromodel_data$AGE_BAND)==TRUE),] %>% select(-AGE)
seromodel_data <- seromodel_data
seromodel_data <- seromodel_data %>% 
  group_by(CITY, SERIES, SEX, AGE_BAND) %>% 
  summarise(N = n(),
            N_POSITIVE = sum(CHKG.1_INTERPRETATION)) 


rp_key <- rp_key %>% mutate(SEX = sex) %>% select(-sex) %>% 
  mutate(SERIES = case_when(
    SERIES == 0 ~ as.Date("2023-11-01", "%Y-%m-%d"),
    SERIES == 1 ~ as.Date("2024-06-01", "%Y-%m-%d")
  )) %>% 
  mutate(CITY = case_when(
    CITY == "São Paulo" ~ "SAOPAOLO",
    CITY == "Rio de Janeiro" ~ "RIODEJANEIRO",
    CITY == "Belo Horizonte" ~ "BELOHORIZONTE",
    CITY == "Curitiba" ~ "CURITIBA",
    CITY == "Fortaleza" ~ "FORTALEZA",
    CITY == "Manaus" ~ "MANAUS",
    CITY == "Recife" ~ "RECIFE"
  )) 
colnames(rp_key)[5] <- "N_POS_rp"
combined_df <- merge(
  rp_key,
  seromodel_data,
  by = c("CITY", "SERIES", "SEX", "AGE_BAND"),
  all = TRUE
)
combined_df <- combined_df %>% 
  mutate_all(~ ifelse(is.na(.), 0, .))
combined_df$SERIES <- as.Date(combined_df$SERIES)
## trouble shooting
combined_df <- combined_df %>% 
  filter(!(AGE_BAND %in% c("<16 Years", ">69 Years")) )

saveRDS(combined_df, file = file.path(out.dir, "int_model_2_key.rds"))
##### now we want to one-hot-encode city, sex, series and age-band
onehot_city <- data.frame(model.matrix(~combined_df$CITY-1,
                                       data = combined_df))
colnames(onehot_city) <- gsub("combined_df.CITY","", colnames(onehot_city))
onehot_sex <- data.frame(model.matrix(~combined_df$SEX-1,
                                      data = combined_df))
colnames(onehot_sex) <- gsub("combined_df.","", colnames(onehot_sex))
onehot_series <- data.frame(model.matrix(~as.factor(combined_df$SERIES)-1,
                                         data = combined_df))
colnames(onehot_series) <- gsub("as.factor.combined_df.SERIES.","", colnames(onehot_series))
onehot_age <- data.frame(model.matrix(~combined_df$AGE_BAND-1,
                                      data = combined_df))
colnames(onehot_age) <- gsub("combined_df.AGE_BAND","", colnames(onehot_age))
######### Create interaction data matrices
N <- nrow(combined_df)

datesex_interact <- matrix(nrow = N, ncol = 2*2) # 2sexes * 2dates
citydate_interact <- matrix(nrow = N, ncol = 7*2) # 7 cities * 2dates

col_idx <- 1
for (i in 1:ncol(onehot_series)) {
  for (j in 1:ncol(onehot_sex)) {
    datesex_interact[, col_idx] <- onehot_series[, i] * onehot_sex[, j]
    col_idx <- col_idx + 1
  }
}
col_idx <- 1
for (i in 1:ncol(onehot_city)) {
  for (j in 1:ncol(onehot_series)) {
    citydate_interact[, col_idx] <- onehot_city[, i] *onehot_series[, j] 
    col_idx <- col_idx + 1
  }
}
#######reported model outbreak parameter

BH2024Ind <- combined_df %>%
  mutate(indicator = case_when(
    CITY == "BELOHORIZONTE" & SERIES =="2024-06-01" ~ 1,
    TRUE ~ 0
  ))

combined_df$SERIES[13]
####### OUTBREAK DESIGN MATRIX
colnames(combined_df)
outbreak <- combined_df %>% 
  mutate(POS_PROP = N_POS_rp/POP,
         OUTBREAK_THRESH = quantile(POS_PROP, 0.75),
         OUTBREAK = as.integer(POS_PROP>OUTBREAK_THRESH))

colnames(combined_df) <- toupper(colnames(combined_df))
colnames(combined_df)[7:8] <- c("N_SM", "N_POS_SM")

### outbreaks for cities:
outbreak_city <- combined_df %>% 
  group_by(CITY) %>% 
  summarise(POS_PROP = sum(N_POS_RP)/sum(POP)) %>% 
  ungroup() %>% 
  mutate(OUTBREAK_THRESH = quantile(POS_PROP, 0.75),
         OUTBREAK = as.integer(POS_PROP>OUTBREAK_THRESH))
### formulate data to be sent to model
int_data <- list()
int_data$N <- nrow(combined_df)
int_data$P_cities <- 7
int_data$P_age <- length(unique(combined_df$AGE_BAND))
int_data$X_cities <- onehot_city
int_data$X_datesex <- datesex_interact
int_data$X_citydate <- citydate_interact
int_data$X_age <- onehot_age
int_data$X_outbreak <- outbreak$OUTBREAK
int_data$population <- combined_df$POP
int_data$X_sex <- onehot_sex[,2] # the column indicating male
int_data$X_date <- onehot_series[,2] # the column indicating June 2024
int_data$N_sm <- combined_df$N_SM
int_data$y_sm <- combined_df$N_POS_SM
int_data$sensitivity_sm <- 0.975
int_data$specificity_sm <- 0.971
int_data$y_rp <- combined_df$N_POS_RP
int_data$outbreak_ind <- BH2024Ind$indicator
int_data$outbreak_city <- c(1, 0, 1, 0, 0, 0, 0)
int_data$outbreak_citydate <- c(0,1,0,0,1,0,0,0,1,1,0,0,0,0)

saveRDS(int_data, file.path(out.dir,"integrated_inputdata.rds"))
