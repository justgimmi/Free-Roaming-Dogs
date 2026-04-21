###### utils links #####
path <- "C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats"
setwd(path)
data_path <- file.path(path, "Data")
utils_path <- file.path(path, "Utils")
plot_path <- file.path(path, "Models/Plot_PA")
source(file.path(utils_path, "Packages.R"))
load(file.path(data_path, "Boundary_sf.RData"))
load(file.path(data_path, "Camera Trap/PA.RData"))
cov_por <- rast(file.path(data_path, "Conteos/covs_updated.tif"))
boundary_sf <- sf::st_transform(boundary_sf, 32629)
crs(boundary_sf)
####### Let's preprocess the data in a meaningfull way #### 

presence_sites <- presence_absence_dogs %>%
  filter(Npres == 1) %>%
  group_by(POINT_STANDARD) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(Presence = 1) # as first step we consider just the points with Npres = 1
# we have decided to just consider one if it was overall present there.

absence_sites <- presence_absence_dogs %>%
  filter(Npres == 0) %>% 
  mutate(
    Presence = 0)

pa_final <- bind_rows(presence_sites, absence_sites)
boundary_segm <- fm_segm(boundary_sf)
mesh <- fm_mesh_2d(
  boundary = boundary_sf,
  max.edge = c(10000, 60000),
  cutoff = 5000 ,
  offset = c(40000, 100000),
  crs = fm_crs(boundary_sf)
)

plot(mesh)



###### Marked Point Process #####
# Starting from the idea that there is a link between where the realizations occurs and the presence/absence nature of the data
# to mitigate this possible source of bias, we could first of all build a model using the presence only nature of the camera trap
# and in a second phase 

spde_shared <- inla.spde2.pcmatern(
  mesh = mesh,
  prior.range = c(100000, 0.5),
  prior.sigma = c(1, 0.5)
)

spde_mark <- inla.spde2.pcmatern(
  mesh = mesh,
  prior.range = c(100000, 0.5),
  prior.sigma = c(1, 0.5)
)



pa_sf <- st_as_sf(pa_final)
pa_final$Presence <- as.numeric(pa_final$Npres) - 1
# table(pa_final$Npres)
cov_por$forest <- scale(cov_por$forest, center = TRUE, scale = TRUE)
values(cov_por$forest)
cov_por$forest[is.na(cov_por$forest)] <- 0
is.na(values(cov_por$forest))
cmp <- geometry + Presence ~ 
  beta0_int(1) +
  beta0_mark(1) +
  u(geometry, model = spde_shared) + 
  u_copy(geometry, copy = "u", fixed = FALSE)+
  forest_int(cov_por$forest, model = "linear")

bru_options_set(
  bru_verbose = TRUE,
  verbose = TRUE,
  bru_max_iter = 1,
  control.inla = list(int.strategy = "eb")
)


lik_int <- bru_obs(
  formula = geometry ~ beta0_int + u + forest_int,
  family = "cp",
  data = pa_final,
  domain = list(geometry = mesh),
  samplers = boundary_sf
)

lik_mark <- bru_obs(
  formula = Presence ~ 
    beta0_mark +
    u_copy,
  family = "binomial",
  data = pa_final,
  domain = list(geometry = mesh)
)

fit <- bru(
  cmp,
  lik_int,
  lik_mark,
  options = list(
    control.inla = list(int.strategy = "eb")
  )
)

# lik_augmented <- bru_obs(
#   formula = geometry + Presence ~ beta0 + field_mark + mark_effect + field_int,
#   family = "cp", 
#   data = pa_final,
#   domain = list(
#     geometry = mesh, 
#     Presence = c(0, 1)
#   ),
#   samplers = boundary_sf
# )



fit <- bru(
  cmp,lik_augmented
)
summary(fit)

ppxl_out <- fm_pixels(mesh, mask = boundary_sf, format = "sf")
# ppxl_out <- fm_cprod(ppxl_out, data.frame(Presence = c(0, 1)))
lambda_out <- predict(
  fit,
  ppxl_out,
  ~ data.frame(
    lambda_obs =  beta0_int + u + forest_int,
    marks = exp(beta0_mark +
                  u_copy)/ (1 + exp(beta0_mark +
                                      u_copy)) ,
    w_init = u,
    w_mark =  u_copy
  )
)
summary(lambda_out$lambda_obs)

log_int <- ggplot(lambda_out$lambda_obs) +
  geom_sf(aes(color = mean), size = 2) + 
  
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.9) +
  scale_color_viridis_c(
    option = "magma", 
    name = "Log-Intensity",
    guide = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5, 
      barwidth = unit(10, "lines"), 
      barheight = unit(0.5, "lines")
    )
  ) +
  
  labs(
    title = "Log-Intensity"
  ) +
  
  annotation_scale(location = "bl", width_hint = 0.2) +
  theme_minimal(base_size = 15) + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey30")
  )

mark <- ggplot(lambda_out$marks) +
  geom_sf(aes(color = mean), size = 2) + 
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.5) +
  scale_color_viridis_c(
    option = "magma", 
    name = "P(presence)",
    guide = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5, 
      barwidth = unit(10, "lines"), 
      barheight = unit(0.5, "lines")
    )
  ) +
  
  labs(
    title = "Mark Probability"
  ) +
  
  annotation_scale(location = "bl", width_hint = 0.2) +
  
  theme_minimal(base_size = 15) + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey30")
  )

mark + log_int
w_log <- ggplot(lambda_out$w_init) +
  geom_sf(aes(color = mean), size = 2) + 
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.5) +
  scale_color_viridis_c(
    option = "magma", 
    name = "Mean GP",
    guide = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5, 
      barwidth = unit(10, "lines"), 
      barheight = unit(0.5, "lines")
    )
  ) +
  
  labs(
    title = "PP GP"
  ) +
  
  annotation_scale(location = "bl", width_hint = 0.2) +
  
  theme_minimal(base_size = 15) + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey30")
  )


w_mark <- ggplot(lambda_out$w_mark) +
  geom_sf(aes(color = mean), size = 2) + 
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.5) +
  scale_color_viridis_c(
    option = "magma", 
    name = "Mean GP",
    guide = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5, 
      barwidth = unit(10, "lines"), 
      barheight = unit(0.5, "lines")
    )
  ) +
  
  labs(
    title = "Mark GP"
  ) +
  
  annotation_scale(location = "bl", width_hint = 0.2) +
  
  theme_minimal(base_size = 15) + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey30")
  )


combined_plot <- (log_int + mark)/(w_log + w_mark)

combined_plot <- plot_grid(log_int, mark, w_log, w_mark, ncol = 4)
ggsave(file.path(plot_path, "combined.png"), combined_plot, width = 40, height = 23, dpi = 100, units = "cm", 
       bg = "white")




####à# Spatial Logistic Regression Code  #####
spde.pa <- inla.spde2.matern(
  mesh = mesh, alpha = 2)
# ?inla.spde2.matern
sites_vect <- vect(pa_final)
cov_values <- terra::extract(cov_por, sites_vect)
pa_final$forest <- cov_values[, 11] # Adjust column index as needed
pa_final$forest <- (pa_final$forest - mean(pa_final$forest, na.rm = TRUE))/sd(pa_final$forest,  na.rm = TRUE)
pa_final$forest[is.na(pa_final$forest)] <- 0

A <- inla.spde.make.A(mesh, loc = st_coordinates(pa_final))
s.index <- inla.spde.make.index(name = "spatial.field",
                                n.spde =  spde.pa$n.spde)
stack_data <- inla.stack(
  data = list(y = pa_final$Presence),
  A = list(A, 1),
  effects = list(c(s.index, list(Intercept = 1)), 
                   list(Cov = pa_final$forest)
), tag = "obs")

grid <- st_make_grid(boundary_sf, cellsize = 5000, what = "centers")
grid_sf <- st_as_sf(grid)
coords <- st_coordinates(grid_sf)
A_pred <- inla.spde.make.A(
  mesh = mesh,   
  loc = coords
)
values <- terra::extract(cov_por, grid_sf)
# grid_sf$Effort <- values[,12]
grid_sf$forest <- values[,11]
grid_sf$forest <- (grid_sf$forest - mean(grid_sf$forest, na.rm = TRUE))/sd(grid_sf$forest,  na.rm = TRUE)
grid_sf$forest[is.na(grid_sf$forest)] <- 0
# grid_sf$forest[is.na(grid_sf$forest)] = 0
is.na(grid_sf$forest)
stack_pred <- inla.stack(
  data = list(y = NA),
  A = list(A_pred, 1),
  effects = list(c(s.index, list(Intercept = 1)), 
                 list(Cov = grid_sf$forest)
  ), 
  tag = "pred")
stack_all <- inla.stack(stack_data, stack_pred)




formula <- y ~  Cov + f(spatial.field, model = spde)

# Run the model
model <- inla(
  formula,
  data = inla.stack.data(stack_all, spde = spde.pa),
  family = "binomial",
  control.predictor = list(A = inla.stack.A(stack_all)),
  control.compute = list(cpo = TRUE, dic = TRUE)
)



summary(model)
plot(model$marginals.fixed$Cov, type = 'l', main = "Effect of Forest")
abline(v = 0, col = "red", lty = 2) # Adding a reference line at zero

model$marginals.fixed$`(Intercept)`
fitted_vals <- model$summary.fitted.values$mean[1:nrow(pa_final)]

# Add to your sf object
pa_final$fitted_prob <- fitted_vals

# Plot
ggplot(pa_final) +
  geom_sf(aes(color = fitted_prob)) +
  geom_sf(data = boundary_sf, fill = NA)+
  scale_color_viridis_c(option = "plasma") +
  labs(title = "Predicted Probability of Presence")

# -1*model$summary.fitted.values$mean[nrow(pa_final) + 1]

model$summary.linear.predictor$mean
index_pred <- inla.stack.index(stack_all, "pred")$data
grid_sf$pred <- exp(model$summary.linear.predictor$mean[index_pred])/(1 +exp(model$summary.linear.predictor$mean[index_pred]) )
grid_sf <- st_intersection(grid_sf, boundary_sf)
pred_mean <- ggplot(grid_sf) +
  geom_sf(aes(color = pred)) +
  geom_sf(data = boundary_sf, fill = NA) +
  scale_color_viridis_c(option = "plasma")

ggsave(filename = "pred_mean.png", pred_mean,  dpi = 100)
fitted_vals <- model$summary.fitted.values$mean[index_pred]
fitted_vals
gproj <- inla.mesh.projector(mesh,  dims = c(400, 400))
g.mean <- inla.mesh.project(gproj, model$summary.random$spatial$mean)
g.sd <- inla.mesh.project(gproj, model$summary.random$spatial$sd)
g.mean

df_map <- expand.grid(x = gproj$x, y = gproj$y)
df_map$z <- as.vector(g.mean)


ggplot() +
  geom_raster(data = df_map, aes(x = x, y = y, fill = z)) +
  scale_fill_viridis_c(option = "magma", name = "Spatial Effect") +
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.5) +
  theme_minimal() +
  labs(
    title = "Spatial Intensity of Domestic Dogs",
    subtitle = "Posterior mean of the spatial random field"
  ) 


gproj <- inla.mesh.projector(mesh, dims = c(400, 400))
field.grid <- inla.mesh.project(
  gproj,
  model$summary.random$spatial$mean
)
length(model$summary.fitted.values$mean)

