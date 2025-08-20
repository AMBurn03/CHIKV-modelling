## Initial EDA
library(ggplot2)
library(dplyr)
library(lubridate)
library(data.table)
out.dir <- "C:/Users/antho/OneDrive - Imperial College London/Dissertation/EDA"
## load data
belohorizonte <- read.csv("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Separate Sheets/Database_serology_belohorizonte.csv")
curitiba <- read.csv("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Separate Sheets/Database_serology_curitiba.csv")
fortaleza <- read.csv("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Separate Sheets/Database_serology_fortaleza.csv")
manaus <- read.csv("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Separate Sheets/Database_serology_manaus.csv")
recife <- read.csv("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Separate Sheets/Database_serology_recife.csv")
riodejaneiro <- read.csv("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Separate Sheets/Database_serology_riodejaneiro.csv")
saopaolo <- read.csv("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Separate Sheets/Database_serology_saopaolo.csv")

# change portuguese month names to english, then lubridate them into YYYY-MM-DD
number_date <- function(data){
  # only months in data are June and November
  data_english <- data %>%
    mutate(
      SERIES =  gsub("Junho", "June", gsub("Novembro", "November",
                     SERIES)))
  data_english$SERIES <- my(data_english$SERIES)
  return(data_english)
}

city_dfs <- list(
  "BELOHORIZONTE" = belohorizonte,
  "CURITIBA" = curitiba,
  "FORTALEZA" = fortaleza,
  "MANAUS" = manaus,
  "RECIFE" = recife,
  "RIODEJANEIRO" = riodejaneiro,
  "SAOPAOLO" = saopaolo
)
full_data <- rbindlist(city_dfs, idcol = "CITY", fill = TRUE)
full_data <- number_date(full_data)

### We will first plot our sample sizes in an example city (Belo Horizonte)
belohorizonte <- full_data[which(full_data$CITY=="BELOHORIZONTE"),]
samples_cities <- full_data %>% 
  group_by(SERIES, CITY) %>% 
  count(name = "SAMPLES")
samples_cities
city_samplesizes <- 
  ggplot(samples_cities, aes(x = SERIES, y = SAMPLES, color = CITY)) +
      # Add points for each city and date
      geom_point( size = 2, alpha = 0.4) +
      geom_line()+
      labs(
        title = "Sample Sizes by City",
        x = "Date",
        y = "Number of Samples",
        color = "CITY"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
            axis.title.x = element_text(size = 16, margin = margin(t = 10)),  # X-axis title
            axis.title.y = element_text(size = 16, margin = margin(r = 10)))+  # Y-axis title) 
      scale_x_date(breaks = unique(samples_cities$SERIES))  # Only show dates in dataset
ggsave(file = file.path(out.dir, 'city_samplesizes.pdf'), city_samplesizes, w = 5, h = 8)


## now for individual cities and the locations of the blood donations

plot_region_samplesize <- function(data, city){
  city_data <- data[which(data$CITY == city),]
  city_data <- city_data %>% 
    group_by(REGION, SERIES) %>% 
    count(name = "SAMPLES")
  p <- ggplot(city_data, aes(x = SERIES, y = SAMPLES, color = REGION)) +
    # Add points for each city and date
    geom_point( size = 2, alpha = 0.4) +
    geom_line()+
    labs(
      title = paste("Sample Sizes by Region, ", city),
      x = "Date",
      y = "Number of Samples",
      color = "Region"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.title.x = element_text(size = 16, margin = margin(t = 10)),  # X-axis title
          axis.title.y = element_text(size = 16, margin = margin(r = 10)))+  # Y-axis title) 
    scale_x_date(breaks = unique(samples_cities$SERIES))  # Only show dates in dataset
  return(p)
}
for(i in unique(full_data$CITY)){
  p <- plot_region_samplesize(full_data, i)
  ggsave(file = file.path(out.dir, paste(i,'_samplesizes.pdf',sep="")), p, w = 10, h = 16)
  }
View(full_data)
