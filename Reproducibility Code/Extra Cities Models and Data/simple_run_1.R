library(cmdstanr)
set_cmdstan_path(file.path("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Modelling", "cmdstan-2.36.0"))

### obtain extra_inputdata.rds from extra_model_data_prep.R

data.dir <-"C:/Users/antho/OneDrive - Imperial College London/Dissertation/Reproducibility Code/Extra Cities Models and Data"
out.dir <- "C:/Users/antho/OneDrive - Imperial College London/Dissertation/Reproducibility Code/Extra Cities Models and Data"

#### extra_data_key shows which variables relate to which rows of observations
#### extra_data can be fed into stan, it has more information than is currently utilised in extra_stan_6.stan (linked in email)
simple_data_key <- readRDS( file.path(data.dir, "simple_data_key.rds") )
simple_data <- readRDS( file.path(data.dir, "simple_inputdata.rds") )

# sum to zero constraints are not working in this model for some weird reason

  # simple_model_compiled <- cmdstan_model(file.path(out.dir, "simple_stan_8.stan"))
# simple_model_compiled <- cmdstan_model(file.path(out.dir, "simple_stan_sm_cut_final.stan"))
# simple_model_compiled <- cmdstan_model(file.path(out.dir, "simple_saocarlos_stan.stan"))
# simple_model_compiled <- cmdstan_model(file.path(out.dir, "simple_stan_8_test.stan"))
simple_model_compiled <- cmdstan_model(file.path(out.dir, "simple_stan_8_tmp.stan"))

# simple_model_compiled <- cmdstan_model(file.path(out.dir, "sm_extrapolate_stan_1.stan"))
# simple_model_compiled <- cmdstan_model(file.path(out.dir, "simple_stan_4.stan"))


simple_fit <- simple_model_compiled$sample(
  data = simple_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 5e2,
  iter_sampling = 4e3,
  refresh = 500, # print update every 500 iters,
  save_warmup = TRUE,
  adapt_delta = 0.9,
  max_treedepth = 15)

  
### take 200 samples of the adjusted probabilities to be used in the reported cases model
simple_data_rp <- simple_data
tmp <- summary(simple_fit$draws(variables = "pi_adj_sm"))

get_Beta_Parameters <- function(mu, sd){
  var <- sd^2
  alpha <- ((1-mu)/ var - (1/mu))* (mu^2)
  beta <- alpha* ( (1/ mu) - 1)
  return(c(alpha,beta))
}
simple_data_rp$pi_adj_sm_alpha_beta_params <- matrix(nrow = simple_data$N,
                                                     ncol = 2, byrow = FALSE,
          data = c(get_Beta_Parameters(tmp$mean,tmp$sd)))

simple_model_rp_compiled <- cmdstan_model(file.path(out.dir, "simple_stan_rp_cut_final.stan"))

simple_fit_rp <- simple_model_rp_compiled$sample(
  data = simple_data_rp,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 5e2,
  iter_sampling = 4e3,
  refresh = 500, # print update every 500 iters,
  save_warmup = TRUE,
  adapt_delta = 0.9,
  max_treedepth = 15)


#### fixed version:
simple_model_rp_compiled_fix <- cmdstan_model(file.path(out.dir, "simple_stan_rp_cut_final_fix.stan"))
tmp_rho_all <- data.frame()
tmp_rp_all <- data.frame()
set.seed(123)
for(i in 1:5){
simple_fit_rp_fixed <- simple_model_rp_compiled_fix$sample(
  data = simple_data_rp,
  seed = sample(1:1000, 1),
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 5e2,
  iter_sampling = 1e3,
  refresh = 500, # print update every 500 iters,
  save_warmup = TRUE,
  adapt_delta = 0.9,
  max_treedepth = 15)
tmp_rho <- simple_fit_rp_fixed$summary(variables = "rho_rp",
                            extra_quantiles = ~posterior::quantile2(., 
                                                       probs = c(.025,                                                                   0.05,0.5,0.95 ,.975)))
tmp_rp <- simple_fit_rp_fixed$summary(variables = "y_pred_rp",
                                       extra_quantiles = ~posterior::quantile2(., 
                                                                               probs = c(.025,                                                                   0.05,0.5,0.95 ,.975)))
tmp_rho_all <- rbind(tmp_rho_all,tmp_rho)
tmp_rp_all <- rbind(tmp_rp_all, tmp_rp)
}
tmp_rho_all <- tmp_rho_all %>% 
  group_by(variable) %>% 
  summarise(
    q2.5 = mean(q2.5),
    q5 = mean(q5),
    q50 = mean(q50),
    q95 = mean(q95),
    q97.5 = mean(q97.5)
  )
# Add numeric column for ordering
tmp_rho_all$number <- as.numeric(gsub(".*\\[(\\d+)\\].*", "\\1", tmp_rho_all$variable))
# Order by that column
tmp_rho_all <- tmp_rho_all[sort(tmp_rho_all$number), ]
tmp_rp_all <- tmp_rp_all %>% 
  group_by(variable) %>% 
  summarise(
    q2.5 = mean(q2.5),
    q5 = mean(q5),
    q50 = mean(q50),
    q95 = mean(q95),
    q97.5 = mean(q97.5)
  )

# Add numeric column for ordering
tmp_rp_all$number <- as.numeric(gsub(".*\\[(\\d+)\\].*", "\\1", tmp_rp_all$variable))
# Order by that column
tmp_rp_all <- tmp_rho_all[sort(tmp_rp_all$number), ]

#### plotting predictive checks
plot_df <- simple_data_key %>% ungroup() %>% 
  mutate(N_med_rp = tmp_rp$q50,
         N_q5_rp = tmp_rp$q5,
         N_q95_rp = tmp_rp$q95,
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
tmp_rho_all <- tmp_rho_all %>%ungroup() %>% arrange(variable)
plot_df <- simple_data_key %>%
  arrange(CITY) %>%
  mutate(N_med_rp = tmp_rho_all$q50,
         N_q5_rp = tmp_rho_all$q5,
         N_q95_rp = tmp_rho_all$q95
  )%>%
  arrange(SERIES) # %>% filter(NAIVE_REPORTING_RATE!=Inf|0|NA)

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
