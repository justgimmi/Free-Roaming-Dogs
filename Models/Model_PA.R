###### utils links #####
path <- "C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats"
setwd(path)
data_path <- file.path(path, "Data")
utils_path <- file.path(path, "Utils")
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
# spde <- inla.spde2.pcmatern(
#   mesh = mesh,
#   prior.range = c(100000, 0.5), # Median range of 1000m
#   prior.sigma = c(1, 0.5)     # Prior for spatial variance
# )



###### Modelll #####
pa_sf <- st_as_sf(pa_final)
cov_por$forest[is.na(cov_por$forest)] <- 0
cmp <-  ~ Intercept(1) +
  field(main = geometry, model = spde)+
  forest(cov_por$forest)

fit <- bru(
  cmp,
  family = "binomial",
  data = pa_sf,
  formula = Presence ~ forest + field + Intercept,
  options = list(
    control.compute = list(dic = TRUE, waic = TRUE)
  )
)
summary(fit)
crs(mesh)
ppxl_out <- fm_pixels(mesh, mask = boundary_sf, format = "sf")
lambda_out <- predict(
  fit,
  ppxl_out,
  ~ data.frame(
    lambda = exp(Intercept + field + forest)/(1 + exp(Intercept + field + forest)),
    w = field
  )
)
summary(lambda_out$lambda)

ggplot(lambda_out$lambda) +
  geom_sf(aes(fill = mean)) +
  geom_sf(data = boundary_sf, fill = NA, color = "black") +
  scale_fill_viridis_c(option = "magma", name = "P(presence)") +
  labs(
    title = "Predicted Presence Probability",
    subtitle = "SPDE logistic model"
  ) +
  theme_minimal()

ggplot(lambda_out$w) +
  geom_sf(aes(fill = mean))  +
  scale_fill_viridis_c(option = "magma", name = "P(presence)") +
  labs(
    title = "Predicted Presence Probability",
    subtitle = "SPDE logistic model"
  ) +
  theme_minimal()

####à# efef  #####
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

