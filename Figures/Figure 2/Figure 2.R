# ---------------------------------------------------------------------------
# Load All the Packages and Import Data 
# ---------------------------------------------------------------------------

load("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Sim/Sim_Def.RData")
source("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Utils/Packages.R")
setwd("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Sim/Fitted_Models/")

# Simulation Parameters 
beta_f <- c(-5,0.5)
range_spde = 100
sigma_spde = 1

# ---------------------------------------------------------------------------
# Code to produce the Plots relative to the Intercept
# ---------------------------------------------------------------------------

load("IDM_Thinning.RData")
trues_prs_abs_df <- rbind(do.call("rbind",MSE_intercept))
colnames(trues_prs_abs_df)[1] <- "values"
trues_prs_abs_df$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta0",par =as.factor(par), mod = "C", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[1]& q975>=beta_f[1],1,0)) -> trues_prs_abs_df

load("IDM_Extra_Field.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_intercept))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta0",par =as.factor(par), mod = "E", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[1]& q975>=beta_f[1],1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)


# load("IDM_Residual.RData")
# 
# trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_intercept))
# colnames(trues_prs_abs_df_2)[1] <- "values"
# trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
# trues_prs_abs_df_2 |>
#   pivot_wider(names_from = id, values_from = "values")|>
#   mutate(par = "beta0_PO",par =as.factor(par), mod = "IDM RES", mod = as.factor(mod),
#          coverage = ifelse(q25<=beta_f[1]& q975>=beta_f[1],1,0)) -> trues_prs_abs_df_2
# 
# 
# trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)


load("IDM_Thinning_linear.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_intercept))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta0",par =as.factor(par), mod = "D", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[1]& q975>=beta_f[1],1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)

load("IDM_Thinning_no_count.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_intercept))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100)
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta0",par =as.factor(par), mod = "F", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[1]& q975>=beta_f[1],1,0)) -> trues_prs_abs_df_2

trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)
# trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)


load("no_corr.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_intercept))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta0",par =as.factor(par), mod = "G", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[1]& q975>=beta_f[1],1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)



load("PA.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_intercept))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta0",par =as.factor(par), mod = "A", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[1]& q975>=beta_f[1],1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)

load("PO.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_beta0))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta0",par =as.factor(par), mod = "B", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[1]& q975>=beta_f[1],1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)


#levels(trues_prs_abs_df$mod) <- c("A", "B", "C", "D", "E", "F", "G" )

save(trues_prs_abs_df, file = "C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/beta0/Intercept.RData")

summary(trues_prs_abs_df$Mean[trues_prs_abs_df$mod == "B"])
p_mse  <- create_boxplot(metric_column = "MSE",
                         title_label  = expression("MSE " ~ beta[0]),
                         y_axis_label = "MSE")
p_mean <- create_boxplot(
  metric_column = "Mean",
  title_label  = expression("Mean Estimate " ~ beta[0]),
  y_axis_label = expression(hat(beta)[0]),
  true_par = beta_f[1])
p_bias <- create_boxplot(metric_column = "bias",
                         title_label  = expression("Bias " ~ beta[0]),
                         y_axis_label = "Bias")
p_var  <- create_boxplot(metric_column = "var",
                         title_label  = expression("Variance " ~ beta[0]),
                         y_axis_label = "Var")

# final_plot <- (p_mse + p_mean) / (p_bias + p_var) + 
#   plot_layout(guides = "collect") 


print(final_plot)
#setwd("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/beta0")
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/beta0/beta0_mean.png", p_mean, width = 9, height = 7.5, dpi = 600)
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/beta0/beta0_mse.png", p_mse, width = 9, height = 7.5, dpi = 600)
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/beta0/beta0_bias.png", p_bias, width = 9, height = 7.5, dpi = 600)
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/beta0/beta0_var.png", p_var, width = 9, height = 7.5, dpi = 600)
# ggsave(
#   filename = "model_performance_boxplots_def.png", 
#   plot = final_plot, 
#   width = 18, 
#   height = 10, 
#   dpi = 300
# )

summary_table <- trues_prs_abs_df %>%
  group_by(mod) %>%
  summarise(`mean(coverage)` = mean(coverage)) %>%
  arrange(mod)
  

# ---------------------------------------------------------------------------
# Code to produce the Plots relative to the Envirometal Covariate
# ---------------------------------------------------------------------------


load("IDM_Thinning.RData")
trues_prs_abs_df <- rbind(do.call("rbind",MSE_beta1))
colnames(trues_prs_abs_df)[1] <- "values"
trues_prs_abs_df$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta1",par =as.factor(par), mod = "C", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[2]& q975>=beta_f[2],1,0)) -> trues_prs_abs_df

load("IDM_Extra_Field.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_beta1))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta1",par =as.factor(par), mod = "E", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[2]& q975>=beta_f[2],1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)


load("IDM_Thinning_linear.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_beta1))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta1",par =as.factor(par), mod = "D", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[2]& q975>=beta_f[2],1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)


# load("IDM_Residual.RData")
# 
# trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_beta1))
# colnames(trues_prs_abs_df_2)[1] <- "values"
# trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
# trues_prs_abs_df_2 |>
#   pivot_wider(names_from = id, values_from = "values")|>
#   mutate(par = "beta1",par =as.factor(par), mod = "IDM res", mod = as.factor(mod),
#          coverage = ifelse(q25<=beta_f[2]& q975>=beta_f[2],1,0)) -> trues_prs_abs_df_2
# 
# 
# trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)


load("IDM_Thinning_no_count.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_beta1))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta1",par =as.factor(par), mod = "F", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[2]& q975>=beta_f[2],1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)

load("no_corr.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_beta1))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta1",par =as.factor(par), mod = "G", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[2]& q975>=beta_f[2],1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)



load("PA.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_beta1))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta1",par =as.factor(par), mod = "A", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[2]& q975>=beta_f[2],1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)

load("PO.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_beta1))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta1",par =as.factor(par), mod = "B", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[2]& q975>=beta_f[2],1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)


save(trues_prs_abs_df, file = "C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/beta1/Enviromenal_Cov.RData")


p_mse  <- create_boxplot(metric_column = "MSE",
                         title_label  = expression("MSE " ~ beta[1]),
                         y_axis_label = "MSE")
p_mean <- create_boxplot(
  metric_column = "Mean",
  title_label  = expression("Mean Estimate " ~ beta[1]),
  y_axis_label = expression(hat(beta)[1]),
  true_par = beta_f[2])
p_bias <- create_boxplot(metric_column = "bias",
                         title_label  = expression("Bias " ~ beta[1]),
                         y_axis_label = "Bias")
p_var  <- create_boxplot(metric_column = "var",
                         title_label  = expression("Variance " ~ beta[1]),
                         y_axis_label = "Var")

 # final_plot <- (p_mse + p_mean) / (p_bias + p_var) + 
 #   plot_layout(guides = "collect") 


# print(final_plot)
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/beta1/beta1_mean.png", p_mean, width = 9, height = 7.5, dpi = 600)
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/beta1/beta1_mse.png", p_mse, width = 9, height = 7.5, dpi = 600)
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/beta1/beta1_bias.png", p_bias, width = 9, height = 7.5, dpi = 600)
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/beta1/beta1_var.png", p_var, width = 9, height = 7.5, dpi = 600)


trues_prs_abs_df|>
  group_by(mod)|>
  summarise(mean(coverage))
boh$`mean(coverage)`[sort(boh$mod)]


# ---------------------------------------------------------------------------
# Code to produce the Plots relative to the standard deviation of the Field
# ---------------------------------------------------------------------------

load("IDM_Thinning.RData")
trues_prs_abs_df <- rbind(do.call("rbind",MSE_sigma))
colnames(trues_prs_abs_df)[1] <- "values"
trues_prs_abs_df$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "sigma",par =as.factor(par), mod = "C", mod = as.factor(mod),
         coverage = ifelse(q25<=sigma_spde& q975>=sigma_spde,1,0)) -> trues_prs_abs_df

load("IDM_Extra_Field.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_sigma))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "sigma",par =as.factor(par), mod = "E", mod = as.factor(mod),
         coverage = ifelse(q25<=sigma_spde& q975>=sigma_spde,1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)


# load("IDM_Residual.RData")
# 
# trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_sigma))
# colnames(trues_prs_abs_df_2)[1] <- "values"
# trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
# trues_prs_abs_df_2 |>
#   pivot_wider(names_from = id, values_from = "values")|>
#   mutate(par = "sigma",par =as.factor(par), mod = "IDM res", mod = as.factor(mod),
#          coverage = ifelse(q25<=sigma_spde& q975>=sigma_spde,1,0)) -> trues_prs_abs_df_2
# 
# 
# trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)


load("IDM_Thinning_linear.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_sigma))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "sigma",par =as.factor(par), mod = "D", mod = as.factor(mod),
         coverage = ifelse(q25<=sigma_spde& q975>=sigma_spde,1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)


load("IDM_Thinning_no_count.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_sigma))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "sigma",par =as.factor(par), mod = "F", mod = as.factor(mod),
         coverage = ifelse(q25<=sigma_spde& q975>=sigma_spde,1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)

load("no_corr.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_sigma))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "sigma",par =as.factor(par), mod = "G", mod = as.factor(mod),
         coverage = ifelse(q25<=sigma_spde& q975>=sigma_spde,1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)



load("PA.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_sigma))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "sigma",par =as.factor(par), mod = "A", mod = as.factor(mod),
         coverage = ifelse(q25<=sigma_spde& q975>=sigma_spde,1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)

load("PO.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_sigma))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "sigma",par =as.factor(par), mod = "B", mod = as.factor(mod),
         coverage = ifelse(q25<=sigma_spde& q975>=sigma_spde,1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)


table(trues_prs_abs_df$mod)

save(trues_prs_abs_df, file = "C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/GP_param/Sigma.RData")


p_mse  <- create_boxplot(metric_column = "MSE",
                         title_label  = expression("MSE " ~ sigma),
                         y_axis_label = "MSE")
p_mean <- create_boxplot(
  metric_column = "Mean",
  title_label  = expression("Mean Estimate " ~ sigma),
  y_axis_label = expression(hat(sigma)),
  true_par = sigma_spde)
p_bias <- create_boxplot(metric_column = "bias",
                         title_label  = expression("Bias " ~ sigma),
                         y_axis_label = "Bias")
p_var  <- create_boxplot(metric_column = "var",
                         title_label  = expression("Variance " ~ sigma),
                         y_axis_label = "Var")

# final_plot <- (p_mse + p_mean) / (p_bias + p_var) + 
#   plot_layout(guides = "collect") 


# print(final_plot)
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/GP_param/sigma_mean.png", p_mean, width = 9, height = 7.5, dpi = 600)
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/GP_param/sigma_mse.png", p_mse, width = 9, height = 7.5, dpi = 600)
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/GP_param/sigma_bias.png", p_bias, width = 9, height = 7.5, dpi = 600)
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/GP_param/sigma_var.png", p_var, width = 9, height = 7.5, dpi = 600)


trues_prs_abs_df|>
  group_by(mod)|>
  summarise(mean(coverage))
rm(trues_prs_abs_df)

# ---------------------------------------------------------------------------
# Code to produce the Plots relative to the range of the Field
# ---------------------------------------------------------------------------

load("IDM_Thinning.RData")
trues_prs_abs_df <- rbind(do.call("rbind",MSE_range))
colnames(trues_prs_abs_df)[1] <- "values"
trues_prs_abs_df$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "range",par =as.factor(par), mod = "C", mod = as.factor(mod),
         coverage = ifelse(q25<=range_spde& q975>=range_spde,1,0)) -> trues_prs_abs_df

load("IDM_Extra_Field.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_range))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "range",par =as.factor(par), mod = "E", mod = as.factor(mod),
         coverage = ifelse(q25<=range_spde& q975>=range_spde,1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)

# 
# load("IDM_Residual.RData")
# 
# trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_range))
# colnames(trues_prs_abs_df_2)[1] <- "values"
# trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
# trues_prs_abs_df_2 |>
#   pivot_wider(names_from = id, values_from = "values")|>
#   mutate(par = "range",par =as.factor(par), mod = "IDM res", mod = as.factor(mod),
#          coverage = ifelse(q25<=range_spde& q975>=range_spde,1,0)) -> trues_prs_abs_df_2
# 
# 
# trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)


load("IDM_Thinning_linear.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_range))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "range",par =as.factor(par), mod = "D", mod = as.factor(mod),
         coverage = ifelse(q25<=range_spde& q975>=range_spde,1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)

load("IDM_Thinning_no_count.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_range))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "range",par =as.factor(par), mod = "F", mod = as.factor(mod),
         coverage = ifelse(q25<=range_spde& q975>=range_spde,1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)
load("no_corr.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_range))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "range",par =as.factor(par), mod = "G", mod = as.factor(mod),
         coverage = ifelse(q25<=range_spde& q975>=range_spde,1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)



load("PA.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_range))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "range",par =as.factor(par), mod = "A", mod = as.factor(mod),
         coverage = ifelse(q25<=range_spde& q975>=range_spde,1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)

load("PO.RData")

trues_prs_abs_df_2 <- rbind(do.call("rbind",MSE_range))
colnames(trues_prs_abs_df_2)[1] <- "values"
trues_prs_abs_df_2$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "range",par =as.factor(par), mod = "B", mod = as.factor(mod),
         coverage = ifelse(q25<=range_spde& q975>=range_spde,1,0)) -> trues_prs_abs_df_2


trues_prs_abs_df <- rbind(trues_prs_abs_df, trues_prs_abs_df_2)


table(trues_prs_abs_df$mod)

save(trues_prs_abs_df, file = "C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/GP_param/Range.RData")


p_mse  <- create_boxplot(metric_column = "MSE",
                         title_label  = expression("MSE " ~ phi),
                         y_axis_label = "MSE")
p_mean <- create_boxplot(
  metric_column = "Mean",
  title_label  = expression("Mean Estimate " ~ phi),
  y_axis_label = expression(hat(phi)),
  true_par = range_spde)
p_bias <- create_boxplot(metric_column = "bias",
                         title_label  = expression("Bias " ~ phi),
                         y_axis_label = "Bias")
p_var  <- create_boxplot(metric_column = "var",
                         title_label  = expression("Variance " ~ phi),
                         y_axis_label = "Var")

# final_plot <- (p_mse + p_mean) / (p_bias + p_var) + 
#   plot_layout(guides = "collect") 


# print(final_plot)
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/GP_param/phi_mean.png", p_mean, width = 9, height = 7.5, dpi = 600)
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/GP_param/phi_mse.png", p_mse, width = 9, height = 7.5, dpi = 600)
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/GP_param/phi_bias.png", p_bias, width = 9, height = 7.5, dpi = 600)
ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/GP_param/phi_var.png", p_var, width = 9, height = 7.5, dpi = 600)


trues_prs_abs_df|>
  group_by(mod)|>
  summarise(mean(coverage))
#boh$`mean(coverage)`[sort(boh$mod)]
rm(trues_prs_abs_df)

# ---------------------------------------------------------------------------
# Code to produce the Plots relative to the Intensity Estimation
# ---------------------------------------------------------------------------


load("IDM_Thinning.RData")
int_df <- rbind(do.call("rbind",MAE_intensity))
colnames(int_df)[1] <- "values"
# int_df$id <- rep(c("MAE"), 100) 
int_df$id <- rep(c("MAE"), 100) 
int_df |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "Intensity",par =as.factor(par), mod = "C", mod = as.factor(mod)) -> int_df

load("IDM_Extra_Field.RData")

int_df_2 <- rbind(do.call("rbind",MAE_intensity))
colnames(int_df_2)[1] <- "values"
# int_df$id <- rep(c("MAE"), 100) 
int_df_2$id <- rep(c("MAE"), 100) 
int_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "Intensity",par =as.factor(par), mod = "E", mod = as.factor(mod)) -> int_df_2

int_df <- rbind(int_df, int_df_2)

# 
# load("IDM_Residual.RData")
# 
# int_df_2 <- rbind(do.call("rbind",MAE_intensity))
# colnames(int_df_2)[1] <- "values"
# # int_df$id <- rep(c("MAE"), 100) 
# int_df_2$id <- rep(c("MAE"), 100) 
# int_df_2 |>
#   pivot_wider(names_from = id, values_from = "values")|>
#   mutate(par = "Intensity",par =as.factor(par), mod = "IDM Res", mod = as.factor(mod)) -> int_df_2
# 
# int_df <- rbind(int_df, int_df_2)

load("IDM_Thinning_linear.RData")

int_df_2 <- rbind(do.call("rbind",MAE_intensity))
colnames(int_df_2)[1] <- "values"
# int_df$id <- rep(c("MAE"), 100) 
int_df_2$id <- rep(c("MAE"), 100) 
int_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "Intensity",par =as.factor(par), mod = "D", mod = as.factor(mod)) -> int_df_2

int_df <- rbind(int_df, int_df_2)

load("IDM_Thinning_no_count.RData")

int_df_2 <- rbind(do.call("rbind",MAE_intensity))
colnames(int_df_2)[1] <- "values"
# int_df$id <- rep(c("MAE"), 100) 
int_df_2$id <- rep(c("MAE"), 100) 
int_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "Intensity",par =as.factor(par), mod = "F", mod = as.factor(mod)) -> int_df_2

int_df <- rbind(int_df, int_df_2)



load("no_corr.RData")


int_df_2 <- rbind(do.call("rbind",MAE_intensity))
colnames(int_df_2)[1] <- "values"
# int_df$id <- rep(c("MAE"), 100) 
int_df_2$id <- rep(c("MAE"), 100) 
int_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "Intensity",par =as.factor(par), mod = "G", mod = as.factor(mod)) -> int_df_2

int_df <- rbind(int_df, int_df_2)

load("PA.RData")



int_df_2 <- rbind(do.call("rbind",MAE_intensity))
colnames(int_df_2)[1] <- "values"
# int_df$id <- rep(c("MAE"), 100) 
int_df_2$id <- rep(c("MAE"), 100) 
int_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "Intensity",par =as.factor(par), mod = "A", mod = as.factor(mod)) -> int_df_2

int_df <- rbind(int_df, int_df_2)

load("PO.RData")

int_df_2 <- rbind(do.call("rbind",MAE_intensity))
colnames(int_df_2)[1] <- "values"
int_df_2$id <- rep(c("MAE"), 100) 
int_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "Intensity",par =as.factor(par), mod = "B", mod = as.factor(mod)) -> int_df_2


int_df <- rbind(int_df, int_df_2)


table(int_df$mod)

save(int_df, file = "C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/Intensity.RData")

trues_prs_abs_df <- int_df
p_mae  <- create_boxplot(metric_column = "MAE",
                                   title_label  = expression("MAE " ~ lambda(s)),
                                   y_axis_label = "MAE")



ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/Intensity/MAE.png", p_mae, width = 9, height = 7.5, dpi = 600)
rm(trues_prs_abs_df)
# ---------------------------------------------------------------------------
# Code to produce the Plots relative to the MAE of the GP
# ---------------------------------------------------------------------------
load("IDM_Thinning.RData")
int_df <- rbind(do.call("rbind",MAE_GP))
colnames(int_df)[1] <- "values"
# int_df$id <- rep(c("MAE"), 100) 
int_df$id <- rep(c("MAE"), 100) 
int_df |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "GP",par =as.factor(par), mod = "C", mod = as.factor(mod)) -> int_df

load("IDM_Extra_Field.RData")

int_df_2 <- rbind(do.call("rbind",MAE_GP))
colnames(int_df_2)[1] <- "values"
# int_df$id <- rep(c("MAE"), 100) 
int_df_2$id <- rep(c("MAE"), 100) 
int_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "GP",par =as.factor(par), mod = "E", mod = as.factor(mod)) -> int_df_2

int_df <- rbind(int_df, int_df_2)


# load("IDM_Residual.RData")
# 
# int_df_2 <- rbind(do.call("rbind",MAE_GP))
# colnames(int_df_2)[1] <- "values"
# # int_df$id <- rep(c("MAE"), 100) 
# int_df_2$id <- rep(c("MAE"), 100) 
# int_df_2 |>
#   pivot_wider(names_from = id, values_from = "values")|>
#   mutate(par = "GP",par =as.factor(par), mod = "IDM Res", mod = as.factor(mod)) -> int_df_2

# int_df <- rbind(int_df, int_df_2)


load("IDM_Thinning_linear.RData")

int_df_2 <- rbind(do.call("rbind",MAE_GP))
colnames(int_df_2)[1] <- "values"
# int_df$id <- rep(c("MAE"), 100) 
int_df_2$id <- rep(c("MAE"), 100) 
int_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "GP",par =as.factor(par), mod = "D", mod = as.factor(mod)) -> int_df_2

int_df <- rbind(int_df, int_df_2)



load("IDM_Thinning_no_count.RData")

int_df_2 <- rbind(do.call("rbind",MAE_GP))
colnames(int_df_2)[1] <- "values"
# int_df$id <- rep(c("MAE"), 100) 
int_df_2$id <- rep(c("MAE"), 100) 
int_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "GP",par =as.factor(par), mod = "F", mod = as.factor(mod)) -> int_df_2

int_df <- rbind(int_df, int_df_2)




load("no_corr.RData")


int_df_2 <- rbind(do.call("rbind",MAE_GP))
colnames(int_df_2)[1] <- "values"
# int_df$id <- rep(c("MAE"), 100) 
int_df_2$id <- rep(c("MAE"), 100) 
int_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "GP",par =as.factor(par), mod = "G", mod = as.factor(mod)) -> int_df_2

int_df <- rbind(int_df, int_df_2)


load("PA.RData")



int_df_2 <- rbind(do.call("rbind",MAE_GP))
colnames(int_df_2)[1] <- "values"
# int_df$id <- rep(c("MAE"), 100) 
int_df_2$id <- rep(c("MAE"), 100) 
int_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "GP",par =as.factor(par), mod = "A", mod = as.factor(mod)) -> int_df_2

int_df <- rbind(int_df, int_df_2)

load("PO.RData")

int_df_2 <- rbind(do.call("rbind",MAE_GP))
colnames(int_df_2)[1] <- "values"
int_df_2$id <- rep(c("MAE"), 100) 
int_df_2 |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "GP",par =as.factor(par), mod = "B", mod = as.factor(mod)) -> int_df_2


int_df <- rbind(int_df, int_df_2)


table(int_df$mod)

save(int_df, file = "C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/GP_param/GP_MAE.RData")


trues_prs_abs_df <- int_df
p_GP  <- create_boxplot(metric_column = "MAE",
                         title_label  = expression("MAE " ~ w(s)),
                         y_axis_label = "MAE")



ggsave("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure 2/GP_param/MAE_GP.png", p_GP, width = 9, height = 7.5, dpi = 600)
