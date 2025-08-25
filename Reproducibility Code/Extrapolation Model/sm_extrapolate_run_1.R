library(cmdstanr)
set_cmdstan_path(file.path("C:/Users/antho/OneDrive - Imperial College London/Dissertation/Modelling", "cmdstan-2.36.0"))

### obtain extra_inputdata.rds from extra_model_data_prep.R

data.dir <-"C:/Users/antho/OneDrive - Imperial College London/Dissertation/Reproducibility Code/Extrapolation Model"
out.dir <- "C:/Users/antho/OneDrive - Imperial College London/Dissertation/Reproducibility Code/Extrapolation Model"

#### extra_data_key shows which variables relate to which rows of observations
#### extra_data can be fed into stan, it has more information than is currently utilised in extra_stan_6.stan (linked in email)
extrap_data_key <- readRDS( file.path(data.dir, "extrap_data_key.rds") )
extrap_data <- readRDS( file.path(data.dir, "extrap_inputdata.rds") )

### sum to zero constraints are not working in this model for some weird reason
  
extrap_model_compiled <- cmdstan_model(file.path(out.dir, "sm_extrapolate_stan_3.stan"))
extrap_fit <- extrap_model_compiled$sample(
  data = extrap_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 5e2,
  iter_sampling = 4e3,
  refresh = 500, # print update every 500 iters,
  save_warmup = TRUE,
  adapt_delta = 0.9,
  max_treedepth = 15)

tmp <- extrap_fit$summary(measures = c("rhat"))
sort(tmp$measures, decreasing = T)[1]
