library(cmdstanr)

set_cmdstan_path(file.path("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Modelling", "cmdstan-2.36.0"))
out.dir <- "C:/Users/antho/OneDrive - Imperial College London/Dissertation/Reproducibility Code/7 Cities Final Model and Data"
data_key <- readRDS(file.path(out.dir, "int_model_2_key.rds"))
stan_data <- readRDS(file.path(out.dir, "integrated_inputdata.rds"))

int_model_compiled <- cmdstan_model(file.path(out.dir, "int_full_stan.stan"))

  # sample
int_fit <- int_model_compiled$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 5e2,
  iter_sampling = 4e3,
  refresh = 500, # print update every 500 iters,
  save_warmup = TRUE,
  adapt_delta = 0.9,
  max_treedepth = 15)
# brief mixing check ###
tmp <- int_fit$summary()
sort(tmp$ess_bulk)[1]   
sort(tmp$rhat, decreasing = T)[1]
#########################
##### reporting rate plotting and PPI checks for 7 cities model
#########################
rp_rate_key <- data_key %>% group_by(CITY,SERIES) %>%
    summarise(NAIVE_REPORT_RATE = sum(N_POS_rp)/((sum(N_POSITIVE)/sum(N)) *sum(POP))) %>% rename(variable = CITY)
# rp_rate_key <- data_key %>% group_by(CITY, SERIES) %>%
#   summarise(NAIVE_REPORT_RATE = sum(N_POS_rp)/((sum(N_POSITIVE)/sum(N)) *sum(POP))) %>% rename(variable = CITY)
infection_summary <- int_fit$summary(variables = c("rho_rp"))
# infection_summary$variable <- make.unique(sort(rep(unique(data_key$CITY),2)))
infection_summary$variable <- make.unique(sort(rep(unique(data_key$CITY),2)))
infection_summary$variable <- c("BELOHORIZONTE (2023)", "BELOHORIZONTE (2024)",
                                "CURITIBA (2023)", "CURITIBA (2024)",
                                "FORTALEZA (2023)", "FORTALEZA (2024)",
                                "MANAUS (2023)", "MANAUS (2024)",
                                "RECIFE (2023)", "RECIFE (2024)",
                                "RIODEJANEIRO (2023)", "RIODEJANEIRO (2024)",
                                "SAOPAOLO (2023)", "SAOPAOLO (2024)")

infection_summary <- infection_summary %>%
  mutate(NAIVE_REPORT_RATE = rp_rate_key$NAIVE_REPORT_RATE)

ggplot(data = infection_summary, aes(x = variable))+
  geom_boxplot(aes(ymin = q5, lower= q5,
                   middle = median, upper = q95, ymax = q95), stat = "identity",
               position = "dodge")+  geom_point(aes(y = NAIVE_REPORT_RATE), colour = "grey")+
theme_bw()+
  labs(y = "Proportion of true cases reported",
       x = "City",
       title = "Each City's Reported Case Rate (95% Credible Intervals)")+
  scale_y_continuous(breaks = seq(0, 1, by = 0.01))+  # From 0 to 1 by 0.01
  theme(  # Main title size and face (bold)
    plot.title = element_text(size = 16, face = "bold"),
    
    # Axis titles size and face
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    
    # Axis text (tick labels)
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 14),# Legend settings
    legend.text = element_text(size = 14),           # Legend item text
    legend.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size=13, face = "bold")) 



tmp <- int_fit$summary(variable = "y_pred_rp")
plot_df <- data_key %>% 
  mutate(N_med_rp = tmp$median,
         N_q5_rp = tmp$q5,
         N_q95_rp = tmp$q95,
         IN_PPI = ifelse(between(N_POS_rp, N_q5_rp, N_q95_rp),"green","red")) %>% 
  arrange(SERIES)

ggplot(data = plot_df[85:168,], aes(x = CITY))+
  facet_wrap(~ SEX + AGE_BAND)+
  geom_boxplot(aes(ymin = N_q5_rp, lower= N_q5_rp,
                   middle = N_med_rp, upper = N_q95_rp, ymax = N_q95_rp), stat = "identity") + geom_point(aes(y = N_POS_rp), colour = plot_df[85:168,]$IN_PPI)+theme_bw()+
  labs(y = "Reported Case Count",
       title = "90% PPI Check for Reported Cases (7 cities, 2024 Only, Reporting Rate Varying by City Negative-Binomial)")+
  theme(  # Main title size and face (bold)
    plot.title = element_text(size = 16, face = "bold"),
    
    # Axis titles size and face
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    
    # Axis text (tick labels)
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 14),# Legend settings
    legend.text = element_text(size = 14),           # Legend item text
    legend.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size=13, face = "bold"))
table(plot_df$IN_PPI)



##### sm PPI check
tmp <- int_fit$summary(variable = "y_pred_adj_sm")
plot_df <- data_key %>% 
  mutate(N_med_rp = tmp$median,
         N_q5_rp = tmp$q5,
         N_q95_rp = tmp$q95,
         IN_PPI = ifelse(between(N_POSITIVE, N_q5_rp, N_q95_rp),"green","red")) %>% 
  arrange(SERIES)
# plot_df <- plot_df %>% 
ggplot(data = plot_df[85:168,], aes(x = CITY))+
  facet_wrap(~ SEX + AGE_BAND)+
  geom_boxplot(aes(ymin = N_q5_rp, lower= N_q5_rp,
                   middle = N_med_rp, upper = N_q95_rp, ymax = N_q95_rp), stat = "identity") + geom_point(aes(y = N_POSITIVE), colour = plot_df[85:168,]$IN_PPI)+theme_bw()+
  labs(y = "Seropositive Count",
       title = "90% PPI Check for Seroprevalence Cases (Final Model, 7 cities, 2024 Only)")+
  theme(  # Main title size and face (bold)
    plot.title = element_text(size = 16, face = "bold"),
    
    # Axis titles size and face
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    
    # Axis text (tick labels)
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 14),# Legend settings
    legend.text = element_text(size = 14),           # Legend item text
    legend.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size=13, face = "bold")) 
table(plot_df$IN_PPI)



### age adjusted reporting rates
rp_rate_key <- data_key %>% group_by(CITY) %>%
  summarise(NAIVE_REPORT_RATE = sum(N_POS_rp)/((sum(N_POSITIVE)/sum(N)) *sum(POP))) %>% rename(variable = CITY)
tmp <- int_fit$summary(variables = c("rho_rp"),
                       posterior::default_summary_measures(),
                       posterior::default_convergence_measures(),
                       extra_quantiles = ~posterior::quantile2(., probs = c(.025, .975))
)
plot_df <- data_key %>% 
  mutate(N_med_rp = tmp$median,
         N_q5_rp = tmp$q5,
         N_q95_rp = tmp$q95,
         N_q2.5_rp = tmp$q2.5,
         N_q97.5_rp = tmp$q97.5) %>% 
  group_by(CITY) %>% 
  summarise(median = sum(N_med_rp*POP)/sum(POP),
            q5 = sum(N_q5_rp*POP)/sum(POP),
            q95 = sum(N_q95_rp*POP)/sum(POP),
            q2.5 = sum(N_q2.5_rp*POP)/sum(POP),
            q97.5 = sum(N_q97.5_rp*POP)/sum(POP))
plot_df$NAIVE_REPORT_RATE <- rp_rate_key$NAIVE_REPORT_RATE

ggplot(data = plot_df, aes(x = CITY))+
  geom_boxplot(aes(ymin = q5, lower= q5,
                   middle = median, upper = q95, ymax = q95), stat = "identity",
               position = "dodge")+  geom_point(aes(y = NAIVE_REPORT_RATE), colour = "grey")+
  theme_bw()+
  labs(y = "Proportion of true cases reported",
       x = "City",
       title = "Each City's Reported Case Rate (90% Credible Intervals)")+
  scale_y_continuous(breaks = seq(0, 1, by = 0.01))+  # From 0 to 1 by 0.01
  theme(  # Main title size and face (bold)
    plot.title = element_text(size = 16, face = "bold"),
    
    # Axis titles size and face
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    
    # Axis text (tick labels)
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 14),# Legend settings
    legend.text = element_text(size = 14),           # Legend item text
    legend.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size=13, face = "bold")) 






















### age adjusted reporting rates
rp_rate_key <- data_key %>% group_by(CITY, SERIES) %>%
  summarise(NAIVE_REPORT_RATE = sum(N_POS_rp)/((sum(N_POSITIVE)/sum(N)) *sum(POP))) %>% rename(variable = CITY)
tmp <- int_fit$summary(variables = c("rho_rp"),
                       posterior::default_summary_measures(),
                       posterior::default_convergence_measures(),
                       extra_quantiles = ~posterior::quantile2(., probs = c(.025, .975))
)
plot_df <- data_key %>% 
  mutate(N_med_rp = tmp$median,
         N_q5_rp = tmp$q5,
         N_q95_rp = tmp$q95,
         N_q2.5_rp = tmp$q2.5,
         N_q97.5_rp = tmp$q97.5) %>% 
  group_by(CITY,SERIES) %>% 
  summarise(median = sum(N_med_rp*POP)/sum(POP),
            q5 = sum(N_q5_rp*POP)/sum(POP),
            q95 = sum(N_q95_rp*POP)/sum(POP),
            q2.5 = sum(N_q2.5_rp*POP)/sum(POP),
            q97.5 = sum(N_q97.5_rp*POP)/sum(POP))
plot_df$NAIVE_REPORT_RATE <- rp_rate_key$NAIVE_REPORT_RATE
plot_df$CITY <- make.unique(plot_df$CITY)
plot_df$CITY <- c("Belo Horizonte (2023)", "Belo Horizonte (2024)",
                  "Curitiba (2023)", "Curitiba (2024)",
                  "Fortaleza (2023)", "Fortaleza (2024)",
                  "Manaus (2023)", "Manaus (2024)",
                  "Recife (2023)", "Recife (2024)",
                  "Rio de Janeiro (2023)", "Rio de Janeiro (2024)",
                  "São Paulo (2023)", "São Paulo (2024)")
ggplot(data = plot_df, aes(x = CITY))+
  geom_boxplot(aes(ymin = q5, lower= q5,
                   middle = median, upper = q95, ymax = q95), stat = "identity",
               position = "dodge")+  geom_point(aes(y = NAIVE_REPORT_RATE), colour = "grey")+
  theme_bw()+
  labs(y = "Proportion of true cases reported",
       x = "City (date)",
       title = "Each City (and Date's) Reported Case Rate (90% EQT Credible Intervals)")+
  scale_y_continuous(breaks = seq(0, 1, by = 0.01))+  # From 0 to 1 by 0.01
  theme(  # Main title size and face (bold)
    plot.title = element_text(size = 16, face = "bold"),
    
    # Axis titles size and face
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    
    # Axis text (tick labels)
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 14),# Legend settings
    legend.text = element_text(size = 14),           # Legend item text
    legend.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size=13, face = "bold")) 
