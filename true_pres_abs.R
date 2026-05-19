
# Set up ------------------------------------------------------------------

# Rispetto a quello che ha fatto jafet noi dobbiamo definire una covariata binaria da moltiplicare per il campo
# o comunque una covariata con hotspots molto simile a ciò che abbiamo.
# In questo modo simuliamo il bias nei presence-absence.

# Per quanto riguarda i presence only, il bias possiamo tranquillamente rappresentarlo tramite 
# un'altra covariata che va a thinnare una realizzazione in qualche modo.


# The basic Idea of this script is the following:
# Assume that the true intensity of the process is \lambda(s) = \beta_{0} + \beta_{1}x(s) + w(s)
# first of all we generate two different realization of the process 
# - the first realization will be thinned according to a thinning mechanism
# - the second realization will be transformed into presence absence using a {0, 1} covariate and the GF
# instead of actually defiune a covariate, what we can do is to consider a sort of latent effect in the sense
# that only I will sample points only in super high value areas of the GP
load("results_simulation.RData")
# load libraries ----------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(scico)
library(INLA)
library(inlabru)
library(fmesher)
library(sp)
library(spatstat)
library(sf)
library(patchwork)
library(terra)
library(tidyterra)
library(stars)
library(purrr)
library(ggthemes)
library(gridExtra)
library(viridis)

# Define spatial domain

save(boundary_sf, sim_field_true, sim_field3_X, pp_set_0, po_sf, po_sf_new, Biased_Sampling_10cellID,
     True_Pres_all, customGrid, xy, x_s, delta_s_PA, sampling_bias, lambda_f, omega_s, file = "Sim1.RData")
load("Sim1.RData")
win <- owin(c(0,300), c(0,300))
npix <- 1000

Domain <- rast(nrows=npix, ncols=npix,
               xmax=win$xrange[2],xmin=win$xrange[1],
               ymax = win$yrange[2],ymin=win$yrange[1])

values(Domain) <- 1:ncell(Domain)
xy <- crds(Domain)

# Define regular grid
cell_size = 5
customGrid <- st_make_grid(Domain,cellsize = c(cell_size,cell_size)) %>% 
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(cellid = row_number())

# number of cells
ncells <- nrow(customGrid)

# Spatial boundary
boundary_sf = st_bbox(c(xmin = 0, xmax = 300, ymax = 0, ymin = 300)) |>
  st_as_sfc()

# Create a fine mesh
mesh_sim = fm_mesh_2d(loc.domain = xy,
                      offset = c(-0.1, -.2),
                      max.edge = c(4, 50))
plot(mesh_sim)
# Matern model
matern_sim <- inla.spde2.pcmatern(mesh_sim,
                                  prior.range = c(100, 0.5),
                                  prior.sigma = c(1, 0.5))


range_spde = 100

sigma_spde = 1

#spde = inla.spde2.matern(mesh_sim, alpha = 2)
# Precision matrix
Q1 = inla.spde.precision(matern_sim, theta = c(log(range_spde), 
                                               log(sigma_spde))) # this is the real gaussian process in the true intensity
Q2 = inla.spde.precision(matern_sim, theta = c(log(range_spde-20),
                                               log(sigma_spde))) # this is used to generate sampling bias in PO

Q3 = inla.spde.precision(matern_sim, theta = c(log(range_spde-30),
                                               log(sigma_spde -0.3))) # this is used to generate sampling bias in PA

Q4 = inla.spde.precision(matern_sim, theta = c(log(range_spde+20),
                                               log(sigma_spde + 0.5))) #covariate in the true intensity
#exp(sqrt(8)/spde$param.inla$theta.initial[1])
# kappa_spde = sqrt(8) / range_spde
# tau_spde = 1 / (sqrt(4 * pi) * kappa_spde * sigma_spde)
# 
# kappa_spde2= sqrt(8) / (range_spde -20)
# tau_spde2 = 1 / (sqrt(4 * pi) * kappa_spde2 * sigma_spde )
# 
# kappa_spde3= sqrt(8) / (range_spde -30)
# tau_spde3 = 1 / (sqrt(4 * pi) * kappa_spde3 * (sigma_spde - 0.3))
# 
# kappa_spde4= sqrt(8) / (range_spde + 20)
# tau_spde4 = 1 / (sqrt(4 * pi) * kappa_spde4 * (sigma_spde + 0.5))
# # Precision matrix
# Q1 = inla.spde.precision(matern_sim, theta = c(log(tau_spde), 
#                                                log(kappa_spde))) # this is the real gaussian process in the true intensity
# Q2 = inla.spde.precision(matern_sim, theta = c(log(tau_spde2), 
#                                                log(kappa_spde2)))# this is used to generate sampling bias in PO
# 
# Q3 = inla.spde.precision(matern_sim, theta = c(log(tau_spde3), 
#                                                log(kappa_spde3))) # this is used to generate sampling bias in PA
# 
# Q4 = inla.spde.precision(matern_sim, theta = c(log(tau_spde4), 
#                                                log(kappa_spde4)))#covariate in the true intensity

# Simulate three spatial fields
# ?inla.spde.precision
# seed = 07072000

sim_field_true = inla.qsample(n = 2, Q = Q1, seed = 123)
sim_field_SB_PO = inla.qsample(n = 1, Q = Q2, seed = 231)
# sim_field2_SB_PA = inla.qsample(n = 1, Q = Q3, seed = seed)
sim_field3_X = inla.qsample(n = 1, Q = Q4, seed = 1234)
sampling_bias_function = function(x,y)
{
  alpha = -0.5
  beta = 1/max(x)
  res <- alpha + beta * x
  return(res)
}
sampling_bias <- data.frame(x = xy[,1],y= xy[,2]) %>% 
  mutate( s = sampling_bias_function(x,y) )

# A matrix
A_proj = inla.spde.make.A(mesh_sim, loc = xy)

# Spatial components
omega_s = (A_proj %*% sim_field_true)[,1] # spatial random field
x_s = (A_proj %*% sim_field3_X)[,1] # spatial environmental covariate
delta_s_PO = (A_proj %*% sim_field_SB_PO)[,1] # spatial bias field
delta_s_PA = (A_proj %*% sim_field_true)[,2]





x_rast_PO = rast(data.frame(x = xy[,1], y = xy[,2],x_s))
omega_rast = rast(data.frame(x = xy[,1], y = xy[,2],omega_s))
g_rast = rast(data.frame(x = xy[,1], y = xy[,2],sampling_bias$s))
x_rast_PA = rast(data.frame(x = xy[,1], y = xy[,2],delta_s_PA)) 
plot(omega_rast)
plot(x_rast_PO)
# Simulate SPP ------------------------------------------------------------

n_data <- 100 # number of data sets

### Focal species

beta_f <- c(-5.5,1)
lambda_f <- exp( beta_f[1] + beta_f[2]*x_s + omega_s) # focal species intensity 
lambda_im <-  data.frame(z = lambda_f, 
                         x = xy[,1],
                         y = xy[,2]) %>%   as.im() #convert to im 
lambda_im_plot <-  data.frame(z = lambda_f, 
                         x = xy[,1],
                         y = xy[,2],
                         omega  = omega_s,
                         delta_s_PO = delta_s_PO, 
                         delta_s_PA = delta_s_PA,
                         x_cov = x_s) %>%   as.im() #convert to im 
plot(lambda_im_plot$delta_s_PO)
plot(lambda_im_plot$delta_s_PA > quantile(lambda_im_plot$delta_s_PA, probs = c(0.9)))
plot(lambda_im_plot$omega)
pp_f = lambda_im %>% rpoispp(nsim = n_data*2) # simulate PP for PO and PA


pp_f_lst <- list() # store the point pattern of the focal species for each j data set

for(j in 1:(n_data*2)){
  pp_f_lst[[j]] <- data.frame(x = pp_f[[j]]$x,  y = pp_f[[j]]$y, sim = j)
}

pp_set_0 <- do.call("rbind",pp_f_lst) # point patters for the focal species (50 replicates)

pp_set_0
save(boundary_sf, sim_field_true, sim_field3_X, pp_set_0)
# Expected abundance in the study area

mean(lambda_f)*st_area(boundary_sf)
# expected counts per cell
mean(lambda_f)*cell_size**2
unique(pp_set_0$sim)

# Presence only data ----------------------------------------------------------
po_sf = pp_set_0[pp_set_0$sim %in% c(1:100), ] %>% st_as_sf(coords= c("x","y"))

plot(lambda_im_plot$x_cov)
points(po_sf[po_sf$sim == 2, ])
alpha= c(0.5,2)


ps_rast <-  data.frame( x = xy[,1],
                        y = xy[,2],
                        p_s =  plogis(alpha[1]+alpha[2]*sampling_bias$s),
                        p_s2 = plogis(alpha[1]+alpha[2]*sampling_bias$s+ delta_s_PO)) %>%
  rast()

plot(ps_rast)
n <- nrow(po_sf)
po_sf_new = po_sf %>%
  mutate( p_s1 = extract(ps_rast,.)$p_s,
          p_s2 = extract(ps_rast,.)$p_s2,
          det1= rbinom(n = n,size = 1,p_s1),
          det2= rbinom(n = n,size = 1,p_s2)) # add indicator of whether observation is detected or not

plot(lambda_im_plot$x_cov)
points(po_sf[po_sf$sim == 2, ])
points(po_sf[po_sf$sim == 2 & po_sf$det1 == 1, ], col = "blue")
po_sf[po_sf$sim == 1 & po_sf$det1 == 1, ]
# Sampling cells ----------------------------------------------------------

# centroid of each grid

grids_centroid <- suppressWarnings(customGrid %>%
                                     st_centroid()) %>% st_coordinates()

# True presence-absence  --------------------------------------------------
# 
# # sample size
n_samp <- round(ncells*.10)
# 
# # Random Sampling cells (1 if cell is is sampled and 0 otherwise for each j data set)
# Random_Sampling_10cellID <- matrix(NA,nrow = ncells,ncol = n_data)
# 
# for(j in 1:n_data){
#   rs_i10 = sample(1:ncells, size=n_samp, replace=FALSE) 
#   Random_Sampling_10cellID[,j] <- 1:ncells%in%rs_i10
# }

# check sample size
# all(n_samp==apply(Random_Sampling_10cellID,2,sum))

# bias sampling probability surface
# beta_star <- -0.7
# BS =  exp( beta_star[1]*x_s + delta_s )/sum(exp(  beta_star[1]*x_s + delta_s ))
BS = delta_s_PA
BS[BS < quantile(BS, probs = c(0.85))] = 0
BS[BS >=  quantile(BS, probs = c(0.85))] =  exp( BS[BS >=  quantile(BS, probs = c(0.85))] )/sum(exp(BS[BS >=  quantile(BS, probs = c(0.85))]))
sum(BS)
#scale_values <- function(x){(x-min(x))/(max(x)-min(x))}
#BS = scale_values(BS)
plot(lambda_im_plot$delta_s_PA)
bs_rast = rast(data.frame(x = xy[,1], y = xy[,2],BS)) #create raster
delta_s_PA_rast = rast(data.frame(x = xy[,1], y = xy[,2],delta_s_PA))
plot(bs_rast)
plot(delta_s_PA_rast)
# mean value per cell
bs_p<- terra::extract(bs_rast, terra::vect(customGrid), fun = mean)[,2]


# Biased Sampling matrix of cells (1 if cell is sampled and 0 otherwise for each j data set)
Biased_Sampling_10cellID <- matrix(NA,nrow = ncells,ncol = n_data)

for(j in 1:n_data){
  bs_i10 = sample(1:ncells, size=n_samp, replace=FALSE, prob=bs_p) 
  Biased_Sampling_10cellID[,j] <- 1:ncells%in%bs_i10
}
customGrid
# Check this is T 
# all(n_samp == apply(Biased_Sampling_10cellID,2,sum))
save(boundary_sf, sim_field_true, sim_field3_X, pp_set_0, po_sf, po_sf_new, Biased_Sampling_10cellID)



#  True-presence absence records at surveyed cells -----------------------


True_Pres_lst <- list() # empty list for j-th data set (cell presences-absence)

for(j in (n_data + 1):(2*n_data)){
  
  # True presence-absence in the whole area
  
  pres <- ifelse(pp_set_0%>%filter(sim==j) %>% select(c(x,y)) %>%
                   st_as_sf(coords = c("x", "y")) %>%
                   st_intersects(x = customGrid) %>% 
                   lengths() > 0,1,0) 
  
  # subset for the surveyed cells (under both random and biased sampling for each beta0 coef)
  
  # True_Pres_lst[[j]] <- rbind(data.frame(pres = pres[Random_Sampling_10cellID[,j]]) %>% 
  #                               bind_cols(grids_centroid[Random_Sampling_10cellID[,j],],
  #                                         sim=j,
  #                                         scheme="RS"),
  #                             data.frame(pres = pres[Biased_Sampling_10cellID[,j]]) %>% 
  #                               bind_cols(grids_centroid[Biased_Sampling_10cellID[,j],],
  #                                         sim=j,
  #                                         scheme="BS"))
  
  True_Pres_lst[[j]] <- data.frame(pres = pres[Biased_Sampling_10cellID[,j - n_data]]) %>% 
                                bind_cols(grids_centroid[Biased_Sampling_10cellID[,j - n_data],],
                                          sim=j - n_data,
                                          scheme="BS")
}


True_Pres_all <-  do.call("rbind",True_Pres_lst) # store cell presences-absence


# check matching is done correctly: 
flag <- 46
customGrid$tst <- Biased_Sampling_10cellID[,flag] #Biased_Sampling_10cellID[,flag]
ggplot()+
  geom_sf(data=customGrid,aes(fill=factor(tst)))+
  geom_sf(data= pp_set_0%>%filter(sim==flag + n_data) %>% select(c(x,y)) %>%
            st_as_sf(coords = c("x", "y")),color="purple",shape=17)+
  geom_sf(data= True_Pres_all %>%
            filter(sim==flag&scheme=="BS")%>% 
            st_as_sf(coords = c("X", "Y")),aes(color=factor(pres)))+
  scale_color_viridis(discrete = T,na.value = "grey50")

rm(flag)

nrow(True_Pres_all[True_Pres_all$sim == 1,])
po_sf[po_sf$sim == 1 & po_sf$det1 == 1, ]



# Analysis  ---------------------------------------------------------------

x_rast_both = rast(data.frame(x = xy[,1], y = xy[,2],x_s))
# g_rast = rast(data.frame(x = xy[,1], y = xy[,2],sampling_bias$s))
x_rast_PA = rast(data.frame(x = xy[,1], y = xy[,2],delta_s_PA))
x_rast_PO =rast(data.frame(x = xy[,1], y = xy[,2],sampling_bias$s))
boundary_sf = st_bbox(c(xmin = 0, xmax = 300, ymax = 0, ymin = 300)) |>
  st_as_sfc() %>% st_as_sf()


# x_rast_both <- focal(x_rast_both, w=7, fun="mean", 
#       expand = TRUE, na.rm = T)
# 
# x_rast_PA <- focal(x_rast_PA, w=7, fun="mean", 
#                      expand = TRUE, na.rm = T)
# 
# x_rast_PO <- focal(x_rast_PO, w=7, fun="mean", 
#                      expand = TRUE, na.rm = T)

# dim = c(0,300)
# 
# loc.d <- cbind(c(dim[1], dim[2], dim[2], dim[2], dim[1]), 
#                c(dim[1], dim[1], dim[2], dim[2], dim[2]))
# 
# sp1 <- list(Polygon(loc.d))
# sp2 <- list(Polygons(sp1,1))
# sp = SpatialPolygons(sp2) 
# boundary_sf <- sf::st_as_sf(sp)


cell_size = 5 

# SPDE
mesh = fm_mesh_2d(boundary = boundary_sf,
                  offset = c(20, 50),   
                  max.edge = c(8, 20))  
# plot(mesh)
# ggplot() +
#   gg(mesh) + 
#   geom_spatraster(data = x_rast_PA)
matern_both <- inla.spde2.pcmatern(mesh,
                              prior.range = c(150, 0.9),
                              prior.sigma = c(0.5, 0.5))
# ?inla.spde2.pcmatern
matern_PO <- inla.spde2.pcmatern(mesh,
                              prior.range = c(60, 0.5),
                              prior.sigma = c(1, 0.5))


cmp = ~ Intercept_PO(1) +
  Intercept_PA(1) + 
  covariate_both(eval_spatial(data=x_rast_both,where = .data.), model = "linear") +
  covariate_PO(eval_spatial(data=x_rast_PO,where = .data.), model = "linear") + 
  covariate_PA(eval_spatial(data=x_rast_PA,where = .data.), model = "linear") +
  field_both(geometry, model = matern_both) + 
  field_PO(geometry, model = matern_PO)




adjust_int = function(x)
  x-log(cell_size^2)

# compute MSE
mse_fx <- function(x,beta,m="mse"){
  tmp <- inla.zmarginal(x,silent = T)
  bias2 <- (tmp$mean - beta)**2
  var <- tmp$sd**2
  if(m=="mse"){
    res = var+bias2
  } else if(m=="rmse"){
    res = sqrt(var+bias2)
  } else if(m=="bias"){
    res = sqrt(bias2)
  } else if(m=="var"){
    res =var
  }
  return(res)
}

# True values 
# beta_f <- c(-5,-1)
# beta_f <- c(-6.5,1)
# beta_f
MSE_beta0 <- list()
MSE_beta1 <- list()
MSE_range <- list()
MSE_sigma <- list()
MSE_intercept <- list()
MAE_intensity <- list()
MAE_GP <- list()
fit_pred <- list()

true_abs = True_Pres_all

true_abs|>
  st_as_sf(coords= c("X","Y")) -> true_abs_sf
bru_options_set(
  bru_verbose = TRUE,
  verbose = TRUE,
  bru_max_iter = 100,
  control.inla = list(int.strategy = "eb")
)

pred_grid_df <- data.frame(xy) %>% 
  rename(x = x, y = y) %>% 
  mutate(
    x_cov = terra::extract(x_rast_both, xy)[,1] 
  ) %>% 
  st_as_sf(coords = c("x", "y"), crs = st_crs(boundary_sf))

beta_f <- c(-5.5,1)
range_spde = 100
sigma_spde = 1
for( j in 1:100){
  
  # True zeros
  # Random sampling
  Presence_Only <- po_sf_new[po_sf_new$sim == j & po_sf_new$det1 == 1, ]
  Presence_Absence <- true_abs_sf[true_abs_sf$sim == j,]
  lik_po <- bru_obs(
    formula = geometry ~ Intercept_PO + covariate_both + field_both + covariate_PO,
    family = "cp",
    data = Presence_Only,
    domain = list(geometry = mesh),
    samplers = boundary_sf
  )
  # ?bru_obs
  lik_PA = bru_obs("binomial",
                 formula = pres ~ Intercept_PA + covariate_both + field_both + covariate_PA + log(cell_size^2),
                 data = Presence_Absence,
                 control.family = list(link = "cloglog"),
                 Ntrials = 1,
                 domain = list(geometry = mesh)) # select response
  
  
  fit = bru(cmp, lik_po, lik_PA)
  
  summary(fit)
  
  # Save results
  
  result_int0 = data.frame(c(mse_fx(fit$marginals.fixed$Intercept_PO,beta_f[1],m="mse"),
                                   inla.zmarginal(fit$marginals.fixed$Intercept_PO,silent = T)$mean,
                                   inla.qmarginal(p = c(0.025, 0.975),fit$marginals.fixed$Intercept_PO),
                                   mse_fx(fit$marginals.fixed$Intercept_PO,beta_f[1],m="bias"),
                                   mse_fx(fit$marginals.fixed$Intercept_PO,beta_f[1],m="var")))
  
  result_intPA = data.frame(c(mse_fx(fit$marginals.fixed$Intercept_PA,beta_f[1],m="mse"),
                             inla.zmarginal(fit$marginals.fixed$Intercept_PA,silent = T)$mean,
                             inla.qmarginal(p = c(0.025, 0.975),fit$marginals.fixed$Intercept_PA),
                             mse_fx(fit$marginals.fixed$Intercept_PA,beta_f[1],m="bias"),
                             mse_fx(fit$marginals.fixed$Intercept_PA,beta_f[1],m="var")))
        
  result_cov0 = data.frame(c(mse_fx(fit$marginals.fixed$covariate_both,beta_f[2],m="mse"),
                                   inla.zmarginal(fit$marginals.fixed$covariate_both,silent = T)$mean,
                                   inla.qmarginal(p = c(0.025, 0.975), fit$marginals.fixed$covariate_both),
                                   mse_fx(fit$marginals.fixed$covariate_both,beta_f[2],m="bias"),
                                   mse_fx(fit$marginals.fixed$covariate_both,beta_f[2],m="var")))
  
  result_range = data.frame(c(mse_fx(fit$marginals.hyperpar$`Range for field_both`,range_spde,m="mse"),
                             inla.zmarginal(fit$marginals.hyperpar$`Range for field_both`,silent = T)$mean,
                             inla.qmarginal(p = c(0.025, 0.975), fit$marginals.hyperpar$`Range for field_both`),
                             mse_fx(fit$marginals.hyperpar$`Range for field_both`,range_spde,m="bias"),
                             mse_fx(fit$marginals.hyperpar$`Range for field_both`,range_spde,m="var")))
  
  result_sigma = data.frame(c(mse_fx(fit$marginals.hyperpar$`Stdev for field_both`,sigma_spde,m="mse"),
                              inla.zmarginal(fit$marginals.hyperpar$`Stdev for field_both`,silent = T)$mean,
                              inla.qmarginal(p = c(0.025, 0.975), fit$marginals.hyperpar$`Stdev for field_both`),
                              mse_fx(fit$marginals.hyperpar$`Stdev for field_both`,sigma_spde,m="bias"),
                              mse_fx(fit$marginals.hyperpar$`Stdev for field_both`,sigma_spde,m="var")))
  


  pred_intensity <- predict(
    object = fit,
    newdata = pred_grid_df,
    formula = ~ exp(Intercept_PA + covariate_both + field_both)
  )
  
  pred_GP <- predict(
    object = fit,
    newdata = pred_grid_df,
    formula = ~ field_both
  )
  predictions_int = data.frame(aver = abs(pred_intensity$mean - lambda_f), q1 = abs(pred_intensity$q0.025 - lambda_f),
                           q3 = abs(pred_intensity$q0.975 - lambda_f), x = xy[, 1], y = xy[, 2])
  
  
  predictions_GP <- data.frame(aver = abs(pred_GP$mean - omega_s), q1 = abs(pred_GP$q0.025 - omega_s),
                               q3 = abs(pred_GP$q0.975 - omega_s), x = xy[, 1], y = xy[, 2])
  MSE_beta0[[j]] <- rbind(result_int0) %>% mutate(data = j) 
  MSE_intercept[[j]] <- rbind(result_intPA) %>% mutate(data = j)
  MSE_sigma[[j]] <- rbind(result_sigma) %>% mutate(data = j)
  MSE_range[[j]] <- rbind(result_range) %>% mutate(data = j)
  MSE_beta1[[j]] <- rbind(result_cov0) %>% mutate(data = j)
  MAE_intensity[[j]] <- data.frame(apply(predictions_int[,1:3], MARGIN = 2, FUN = mean)) %>% mutate(data = j)
  MAE_GP[[j]] <- data.frame(apply(predictions_GP[,1:3], MARGIN = 2, FUN = mean)) %>% mutate(data = j)
  fit_pred[[j]] <- fit
  print(paste("iter j =",j))
  }


save(MSE_beta0, MSE_beta1, MSE_intercept,MSE_range, MSE_sigma, MAE_intensity, MAE_GP,fit_pred,
     file = "results_simulation.RData")

beta_f <- c(-5.5,1)
range_spde = 100
sigma_spde = 1
trues_prs_abs_df <- rbind(do.call("rbind",MSE_beta0))
colnames(trues_prs_abs_df)[1] <- "values"
trues_prs_abs_df$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 
trues_prs_abs_df |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta0_PO",par =as.factor(par), mod = "IDM", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[1]& q975>=beta_f[1],1,0)) -> trues_prs_abs_df


trues_int_abs_df <- rbind(do.call("rbind",MSE_intercept))
colnames(trues_int_abs_df)[1] <- "values"
trues_int_abs_df$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 

trues_int_abs_df |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta0_PA",par =as.factor(par),mod = "IDM", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[1]& q975>=beta_f[1],1,0)) -> trues_int_abs_df

trues_cov_df <- rbind(do.call("rbind",MSE_beta1))
colnames(trues_cov_df)[1] <- "values"
trues_cov_df$id <- rep(c("MSE", "Mean", "q25", "q975", "bias", "var"), 100) 

trues_cov_df |>
  pivot_wider(names_from = id, values_from = "values")|>
  mutate(par = "beta1",par =as.factor(par),mod = "IDM", mod = as.factor(mod),
         coverage = ifelse(q25<=beta_f[2]& q975>=beta_f[2],1,0)) -> trues_cov_df




param_df <- rbind(trues_cov_df, trues_int_abs_df, trues_prs_abs_df)

levels(param_df$par) <- c(expression(beta[1]), expression(beta[0]^PA),expression(beta[0]^PO))


#save(trues_prs_abs_df,file="trues_prs_abs_results.RData")

p1 <- param_df %>%
  ggplot(aes(x=as.factor(data),y=(MSE))) +
  geom_boxplot() + facet_wrap(~par,labeller = label_parsed ,scales = "free") +
  labs(x="",y=expression(MSE~(beta))) + 
  theme(legend.position = 0, text=element_text(family="serif", size=20))

p2 <- param_df %>%
  ggplot(aes(x=as.factor(data),y=(bias))) +
  geom_boxplot() + facet_wrap(~par,labeller = label_parsed ,scales = "free") +
  labs(x="",y=expression(Bias~(beta))) + 
  theme(legend.position = 0, text=element_text(family="serif", size=20))

p3 <- param_df %>%
  ggplot(aes(x=as.factor(data),y=(var))) +
  geom_boxplot() + facet_wrap(~par,labeller = label_parsed ,scales = "free") +
  labs(x="Sampling scheme",y=expression(Var~(beta))) + 
  theme(legend.position = 0, text=element_text(family="serif", size=20))

p1+p2+p3+patchwork::plot_layout(ncol=1)

ggsave(filename = "true_pres_abs_sim_all.pdf",dpi = 300,height = 4000,width = 3000,units = "px")

param_df|>
  group_by(par)|>
  summarise(mean(coverage))
# param_df %>% 
#   pivot_longer(cols = c(MSE,bias,var),names_to = "Metric") %>% filter(Metric %in% c("bias","var")) %>%
#   ggplot(aes(x=as.factor(data),y=(value))) +
#   geom_boxplot() + facet_grid(Metric~par,labeller = label_parsed ,scales = "free") +
#   labs(x="Sampling scheme",y=expression(Bias~(beta))) + 
#   theme(legend.position = 0, text=element_text(family="serif", size=20))

