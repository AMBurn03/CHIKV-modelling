##### see how the log-likehood varies with the true prevalence as false negative rate varies in the serology model!

llike_gen <- function(N, N_pos, prob,sensitivity, specificity){
  true_prob = prob*sensitivity + (1-specificity) * (1-prob)
  return(dbinom(N_pos,N, true_prob, log= T))
}
## do it for first observation in simple_data_key
#### do plot for specificity in c(0.8, 0.85, 0.90, 0.95, 1)
plot_df <- data.frame(prob = rep(seq(0, 0.05, length.out = 100),5),
                      specificity = sort(rep(c(0.98, 1, 0.96, 0.97, 0.99), 100)),
                      sensitivity = 0.975)
plot_df$llike <-  llike_gen(data_key$N[3],
                            data_key$N_POSITIVE[3],
                            plot_df$prob,
                            plot_df$sensitivity,
                            plot_df$specificity)

plot_df$MLE = 0
ggplot(data = plot_df, aes(x = prob, y = llike, colour = as.factor(specificity)))+
  geom_line()+labs(title = "Specificity's Impact on Log Likelihood, for Women in Belo Horizonte Between Ages 30-39 (2023-11-01)",
                   x = "True Prevalence",
                   y = "Log Likelihood",
                   colour = "Specificity")+
  annotate("text", x = plot_df$MLE[1], y = -0.5, label = paste0("MLE = ", round(plot_df$MLE[1],3)), 
           angle = 90, hjust = 0.5, vjust = -0.5, color = "black", size = 3.5)+
  geom_vline(xintercept = plot_df$MLE[1], linetype = "dashed", color = "black") +
  theme_bw()+
  xlim(c(-0.01,0.05))+ylim(c(-3,0))







#### now for sensitivity varying
plot_df <- data.frame(prob = rep(seq(0, 1, length.out = 1000),5),
                      specificity = 0.971,
                      sensitivity = sort(rep(c(0.98, 1, 0.96, 0.97, 0.99), 1000)))
plot_df$llike <-  llike_gen(data_key$N[104],
                            data_key$N_POSITIVE[104],
                            plot_df$prob,
                            plot_df$sensitivity,
                            plot_df$specificity)
plot_df$MLE = data_key$N_POSITIVE[104]/data_key$N[104]
ggplot(data = plot_df, aes(x = prob, y = llike, colour = as.factor(sensitivity)))+
  geom_line()+labs(title = "Sensitivity's Impact on Log Likelihood, for Men in Recife Between Ages 20-29 (2023-11-01)",
                   x = "True Prevalence",
                   y = "Log Likelihood",
                   colour = "Sensitivity")+
  annotate("text", x = plot_df$MLE[1], y = -200, label = paste0("MLE = ", round(plot_df$MLE[1],3)), 
           angle = 90, hjust = 0.5, vjust = -0.5, color = "black", size = 3.5)+
  geom_vline(xintercept = plot_df$MLE[1], linetype = "dashed", color = "black") +
  theme_bw()
  # xlim(c(-0.01,0.05))+ylim(c(-3,0))

