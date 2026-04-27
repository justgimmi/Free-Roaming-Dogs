###### utils links #####
path <- "C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats"
setwd(path)
data_path <- file.path(path, "Data")
utils_path <- file.path(path, "Utils")
plot_path <- file.path(path, "Models/Plot_PA")
source(file.path(utils_path, "Packages.R"))
load(file.path(data_path, "Boundary_sf.RData"))
# load(file.path(data_path, "Camera Trap/PA.RData"))
cov_por <- rast(file.path(data_path, "Conteos/covs_updated_new.tif"))
crs <- "EPSG:3035" 
proj_string <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=km +no_defs"

# crs_km <- fm_crs("EPSG:3035", units = "km")
crs_km <- fm_crs_set_lengthunit(fm_crs("EPSG:3035"), "km")

boundary_sf <- fm_transform(boundary_sf, crs_km)

ggplot() + 
  geom_sf(data = boundary_sf)
boundary_sf <- boundary_sf |>
  st_make_valid() |>
  st_union() |>
  st_cast("POLYGON")

boundary_sf <- boundary_sf[which.max(st_area(boundary_sf)), ]

boundary_union <- st_union(boundary_sf) 
boundary_clean <- st_buffer(boundary_union, dist = 0)
boundary_final <- st_concave_hull(boundary_clean, ratio = 0.0025) 
ggplot() + 
  geom_sf(data = boundary_final, fill = "lightblue", alpha = 0.5) -> gr
  

  
ggplot() + 
  geom_sf(data = boundary_sf, fill = "lightblue", alpha = 0.5) -> gr2


gr + gr2
boundary_sf <- boundary_final
# crs_km <- fm_crs(crs, units = "km")
collision_dogs <- read_sf(file.path(data_path, "Database_Dogs&Cats/Collisions_dogs_new.shp"))
cov_ext <- extend(
  cov_por,
  ext(cov_por) + 40000)
cov_ext <- terra::project(cov_ext, crs_km$wkt)
target_classes <- c("motorway", "primary", "residential", "secondary",
                    "tertiary", "track", "trunk")
collision_dogs|>
  filter(road_class %in% target_classes) |>
  fm_transform(crs_km$wkt) |>
  distinct(geometry) |>
  st_filter(boundary_sf, .predicate = st_within)-> collisions_sf

table(collision_dogs$road_class)
# mesh <- fm_mesh_2d(
#   boundary = list(boundary_sf),
#   max.edge = c(4, 10),    # 2km interno (risoluzione raster), 6km esterno (bordo)
#   cutoff = 1,          # 500m (minima distanza tra nodi, evita ammassamenti)
#   offset = c(2, 10),     # estensione del dominio
#   crs = fm_crs(boundary_sf)
# )

mesh <- fm_mesh_2d(
  boundary = list(boundary_sf),
  max.edge = c(5, 25),
  cutoff = 5,
  offset = c(10, 20),
  crs = fm_crs(boundary_sf)
)
# mesh <- fm_mesh_2d(
#   boundary = list(boundary_sf),
#   max.edge = c(10, 30),
#   cutoff = 8 ,
#   offset = c(10, 35),
#   crs = fm_crs(boundary_sf)
# )
mesh$n
plot(mesh)
plot(mesh)
####### Let's first of all build a model that works for the collisions #######
spde_co <- inla.spde2.pcmatern(
  mesh = mesh,
  prior.range = c(10, 0.5),
  prior.sigma = c(1, 0.05)
)
# dimension of portugal are more less: 561 x 218




cov_scaled <- cov_ext
cov_scaled$min_dist_to_any_road <- cov_scaled$min_dist_to_any_road /1000
cov_scaled$forest <- (cov_scaled$forest - mean(values(cov_scaled$forest), na.rm = TRUE))/sd(values(cov_scaled$forest), na.rm = TRUE)
cov_scaled$hfp <- (cov_scaled$hfp - mean(values(cov_scaled$hfp), na.rm = TRUE))/sd(values(cov_scaled$hfp), na.rm = TRUE)
cov_scaled$density_km <- (cov_scaled$density_km - mean(values(cov_scaled$density_km), na.rm = TRUE))/sd(values(cov_scaled$density_km), na.rm = TRUE)
cov_scaled$min_dist_to_any_road <- (cov_scaled$min_dist_to_any_road - mean(values(cov_scaled$min_dist_to_any_road), na.rm = TRUE))/sd(values(cov_scaled$min_dist_to_any_road), na.rm = TRUE)
cov_scaled$hfp[is.na(cov_scaled$hfp)] <- 0
cov_scaled$prec[is.na(cov_scaled$prec)] <- 0
cov_scaled$forest[is.na(cov_scaled$forest)] <- 0
cov_scaled$density_km[is.na(cov_scaled$density_km)] <- 0
cov_scaled$min_dist_to_any_road[is.na(cov_scaled$min_dist_to_any_road)] <- 0
outline <-  st_simplify(st_as_sf(boundary_sf), dTolerance = 3)
# hfp <-  mask(cov_scaled$hfp,outline)
hfp <-  focal(cov_scaled$hfp, w=15, fun="mean", 
              expand = TRUE, na.rm = T)

density_km <-  focal(cov_scaled$density_km, w=15, fun="mean", 
              expand = TRUE, na.rm = T)

min_dist <- focal(cov_scaled$min_dist_to_any_road, w=15, fun="mean", 
                  expand = TRUE, na.rm = T)
# outline = st_simplify(st_as_sf(boundary_sf), dTolerance = 3)


summary(values(cov_scaled$hfp))
ggplot() +
  geom_spatraster(data = hfp) + # Visualizza un layer del raster
  geom_sf(data = outline, fill = NA, col = "red", size = 1) + # Il bordo
  gg(mesh) + 
  labs(title = "Controllo disallineamento: Il rosso è fuori dal raster?")

cmp <-  ~ 
  beta0_po(1, model = "linear", prec.linear = 0.01) +
  u(geometry, model = spde_co) + 
  hfp_cov(hfp, model = "linear") +  
  road_dens(min_dist, model = "linear")

bru_options_set(
  bru_verbose = TRUE,
  verbose = TRUE,
  bru_max_iter = 2,
  control.inla = list(int.strategy = "ccd")
)


lik_po <- bru_obs(
  formula = geometry ~ beta0_po + u + hfp_cov + road_dens,
  family = "cp",
  data = collisions_sf,
  domain = list(geometry = mesh),
  samplers = outline
)
fit <- bru(
  cmp,lik_po
)
summary(fit)

ppxl_out <- fm_pixels(mesh, mask = boundary_sf, format = "sf")
# ppxl_out <- fm_cprod(ppxl_out, data.frame(Presence = c(0, 1)))
lambda_out <- predict(
  fit,
  ppxl_out,
  ~ data.frame(
    lambda_po =  beta0_po +  u ,
    w_po = u
  )
)



log_int <- ggplot(lambda_out$lambda_po) +
  geom_sf(aes(color = mean), size = 2) + 
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.9) +
  scale_color_viridis_c(
    option = "magma", 
    name = "Log-Intensity PO",
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

log_pa <- ggplot(lambda_out$lambda_pa) +
  geom_sf(aes(color = mean), size = 2) + 
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.5) +
  scale_color_viridis_c(
    option = "magma", 
    name = "Lo-Intensity",
    guide = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5, 
      barwidth = unit(10, "lines"), 
      barheight = unit(0.5, "lines")
    )
  ) +
  
  labs(
    title = "Log Intensity PA"
  ) +
  
  annotation_scale(location = "bl", width_hint = 0.2) +
  
  theme_minimal(base_size = 15) + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey30")
  )


marks_prob <- ggplot(lambda_out$marks) +
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


log_pa + log_int + marks_prob
w_pa <- ggplot(lambda_out$w_pa) +
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


w_mark <- ggplot(lambda_out$w_pa_copy) +
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

w_po <- ggplot(lambda_out$w_po) +
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
w_pa + w_po + w_mark


