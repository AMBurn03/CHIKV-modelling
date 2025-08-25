### serology PPI check
tmp <- simple_fit$summary(variable ="y_pred_sm")
plot_df <- simple_data_key %>%
  mutate(N_med_rp = tmp$median,
         N_q5_rp = tmp$q5,
         N_q95_rp = tmp$q95,
         IN_PPI = ifelse(between(N_POSITIVE, N_q5_rp, N_q95_rp),"green","red")) 
plot_df <- plot_df %>%
  mutate(CITY = paste0(CITY, " (", year(SERIES), ")"))
plot_df$CITY <- make.unique(plot_df$CITY)

ggplot(data = plot_df, aes(x = CITY))+
  geom_boxplot(aes(ymin = N_q5_rp, lower= N_q5_rp,
                   middle = N_med_rp, upper = N_q95_rp, ymax = N_q95_rp), stat = "identity") + geom_point(aes(y = N_POSITIVE), colour = plot_df$IN_PPI)+
  geom_errorbar(aes(ymin = EXPECTED_FALSE_POS, ymax = EXPECTED_FALSE_POS), 
                colour = "purple", alpha = 0.4, width = 0.5)+theme_bw()+
  labs(y = "Seropositive Count",
       x= "City (Date)",
       title = "90% PPI Check for Serology Data in Extra Data Model")+
  theme(  # Main title size and face (bold)
    plot.title = element_text(size = 20, face = "bold"),
    
    # Axis titles size and face
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    
    # Axis text (tick labels)
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 14))

table(plot_df$IN_PPI)
### reported cases PPI check
tmp <- simple_fit$summary(variable = "y_pred_rp")
# tmp <- simple_fit$summary(variable = "mu_rp")

plot_df <- simple_data_key %>% ungroup() %>% 
  mutate(N_med_rp = tmp$median,
         N_q5_rp = tmp$q5,
         N_q95_rp = tmp$q95,
         IN_PPI = ifelse(between(N_REPORTED, N_q5_rp, N_q95_rp),"green","red")) %>%
  arrange(SERIES) 
plot_df$CITY <- make.unique(plot_df$CITY)

ggplot(data = plot_df, aes(x = CITY, colour = as.factor(SERIES)))+
  geom_boxplot(aes(ymin = N_q5_rp, lower= N_q5_rp,
                   middle = N_med_rp, upper = N_q95_rp, ymax = N_q95_rp), stat = "identity") + geom_point(aes(y = N_REPORTED), colour = plot_df$IN_PPI)+theme_bw()+
  labs(y = "Proportion of population with a reported case",
       title = "90% PPI Check for Simple Poisson Model")+
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title=element_text(size=10, face="bold"))
table(plot_df$IN_PPI)
#### reporting rate plot
tmp <- simple_fit$summary(variable = "rho_rp")
plot_df <- simple_data_key %>%
  arrange(CITY) %>%
  mutate(N_med_rp = tmp$median,
         N_q5_rp = tmp$q5,
         N_q95_rp = tmp$q95
         )%>%
  arrange(SERIES) # %>% filter(NAIVE_REPORTING_RATE!=Inf|0|NA)

plot_df <- plot_df %>%
  mutate(CITY = paste0(CITY, " (", year(SERIES), ")"))
plot_df$CITY <- make.unique(plot_df$CITY)
ggplot(data = plot_df, aes(x = CITY))+
  geom_boxplot(aes(ymin = N_q5_rp, lower= N_q5_rp,
                   middle = N_med_rp, upper = N_q95_rp, ymax = N_q95_rp), stat = "identity") +
  # geom_point(aes(y = NAIVE_REPORTING_RATE), colour = plot_df$IN_PPI)+
  theme_bw()+
  # ylim(0,0.15)+
  labs(y = "Reporting Rate",
       title = "90% Credible Intervals for Reporting Rates (Simple model, Poisson)")+
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title=element_text(size=10, face="bold"))
