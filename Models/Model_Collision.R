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
# target_classes <- c("motorway", "primary", "residential", "secondary",
#                      "tertiary", "track", "trunk")
# collision_dogs|>
#   filter(road_class %in% target_classes) |>
#   fm_transform(crs_km$wkt) |>
#   distinct(geometry) |>
#   st_filter(boundary_sf, .predicate = st_within)-> collisions_sf

collision_dogs |>
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

# mesh <- fm_mesh_2d(
#   boundary = list(boundary_sf),
#   max.edge = c(5, 25),
#   cutoff = 5,
#   offset = c(10, 20),
#   crs = fm_crs(boundary_sf)
# )
# mesh <- fm_mesh_2d(
#   boundary = list(boundary_sf),
#   max.edge = c(10, 30),
#   cutoff = 8 ,
#   offset = c(10, 35),
#   crs = fm_crs(boundary_sf)
# )

load(file.path(data_path, "Camera Trap/PA.RData"))
presence_sites <- presence_absence_dogs %>%
  filter(Npres == 1) %>%
  mutate(years = year(Timestamp))|>
  group_by(years, POINT_STANDARD) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(Presence = 1) |>
  filter(years %in% c(2019:2023))# as first step we consider just the points with Npres = 1

# we have decided to just consider one if it was overall present there.

absence_sites <- presence_absence_dogs %>%
  filter(Npres == 0) %>% 
  mutate(Presence = 0, years = year(INSTALLATION)) |>
  group_by(years, POINT_STANDARD) %>%
  slice(1) %>%
  ungroup()

# table(absence_sites$Presence)
pa_final <- rbind(presence_sites, absence_sites)
table(pa_final$Presence, pa_final$years)
pa_final <- pa_final |>
  fm_transform(crs_km$wkt) |>
  arrange(years, geometry, desc(Npres)) |>   # priorità a Npres = 1
  distinct(years, geometry, .keep_all = TRUE)

pa_2022 <- pa_final|>
  filter(years == 2022)|>
  distinct(geometry, .keep_all = TRUE)

####### Let's first of all build a model that works for the collisions #######
outline <-  st_simplify(st_as_sf(boundary_sf), dTolerance = 3)
mesh <- fm_mesh_2d(
  boundary = list(outline),
  max.edge = c(5, 25),
  cutoff = 5,
  offset = c(10, 20),
  crs = fm_crs(outline)
)

spde_co <- inla.spde2.pcmatern(
  mesh = mesh,
  prior.range = c(10, 0.5),
  prior.sigma = c(1, 0.05)
)
# dimension of portugal are more less: 561 x 218


cov_scaled <- cov_ext
hfp <- scale(cov_scaled$hfp)
hfp[is.na(hfp)] <- 0
hfp <-  focal(hfp, w=15, fun="mean", 
              expand = TRUE, na.rm = T)


forest <- scale(cov_scaled$forest)
forest[is.na(forest)] <- 0
forest <-  focal(forest, w=15, fun="mean", 
                 expand = TRUE, na.rm = T)

agfor <- scale(cov_scaled$agfor)
agfor[is.na(agfor)] <- 0
agfor <-  focal(agfor, w=15, fun="mean", 
                expand = TRUE, na.rm = T)

plot(forest)
plot(hfp)
plot(agfor)
cor(values(hfp), values(density_km))
density_km <- scale(cov_scaled$density_km)
density_km[is.na(density_km)] <- 0
density_km <-  focal(density_km, w=17, fun="mean", 
                     expand = TRUE, na.rm = T)
plot(density_km)
min_dist <- mask(cov_scaled$min_dist_to_any_road/1000, outline)
min_dist <- scale(cov_scaled$min_dist_to_any_road/1000)
min_dist[is.na(min_dist)] <- 0
min_dist <- focal(min_dist, w=19, fun="mean", 
                  expand = TRUE, na.rm = T)

plot(cov_scaled$min_dist_to_any_road/1000)


plot(mesh)
plot(mesh_bis)
plot(min_dist)
plot(hfp)
plot(density_km)



cmp <-  ~ 
  beta0_po(1, model = "linear", prec.linear = 0.01) +
  u(geometry, model = spde_co) + 
  hfp_cov(hfp, model = "linear") +  
  forest_cov(forest, model = "linear") +  
  agfor_cov(agfor, model = "linear") +
  min_dist_cov(density_km, model = "linear")

bru_options_set(
  bru_verbose = TRUE,
  verbose = TRUE,
  bru_max_iter = 5,
  control.inla = list(int.strategy = "eb")
)


lik_po <- bru_obs(
  formula = geometry ~ beta0_po + u  + hfp_cov + agfor_cov + forest_cov,
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
    lambda_po =  beta0_po + u  +  hfp_cov + agfor_cov + forest_cov ,
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
    title = "PP GP"
  ) +
  
  annotation_scale(location = "bl", width_hint = 0.2) +
  
  theme_minimal(base_size = 15) + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey30")
  )


ggplot() +
  geom_sf(data = collisions_sf, col = "firebrick", size = 0.5) + 
  geom_sf(data = outline, fill = NA, col = "black", size = 1) -> dat_plot
hfp.plot <- plot(fit, "hfp_cov") +
  ggtitle("Posterior of hfp") +
  theme(legend.position = "bottom")

agro_for.plot <- plot(fit, "agfor_cov") +
  ggtitle("Posterior of afgfor") +
  theme(legend.position = "bottom")

forest.plot <- plot(fit, "forest_cov") +
  ggtitle("Posterior of forest") +
  theme(legend.position = "bottom")

cov_int_plot <- log_int + w_po + dat_plot
ggsave(file.path(plot_path, "collisions_PP.png"), cov_int_plot, width = 40, height = 23, dpi = 100, units = "cm", 
       bg = "white")

spde.range <- spde.posterior(fit, "u", what = "range")
spde.logvar <- spde.posterior(fit, "u", what = "log.variance")
range.plot <- plot(spde.range)
var.plot <- plot(spde.logvar)
(range.plot / var.plot)
cov_int_plot <- hfp.plot + agro_for.plot + forest.plot + range.plot + var.plot


ggsave(file.path(plot_path, "collisions_other.png"), cov_int_plot, width = 40, height = 23, dpi = 100, units = "cm", 
       bg = "white")


###### combined methodology #####
outline <-  st_simplify(st_as_sf(boundary_sf), dTolerance = 3)
mesh <- fm_mesh_2d(
  boundary = list(outline),
  max.edge = c(5, 25),
  cutoff = 5,
  offset = c(10, 20),
  crs = fm_crs(outline)
)
spde_pa <- inla.spde2.pcmatern(
  mesh = mesh,
  prior.range = c(10, 0.5),
  prior.sigma = c(1, 0.05)
)

spde_co <- inla.spde2.pcmatern(
  mesh = mesh,
  prior.range = c(50, 0.5),
  prior.sigma = c(1, 0.05)
)
# dimension of portugal are more less: 561 x 218


cov_scaled <- cov_ext
hfp <- scale(cov_scaled$hfp)
hfp[is.na(hfp)] <- 0
hfp <-  focal(hfp, w=15, fun="mean", 
              expand = TRUE, na.rm = T)


forest <- scale(cov_scaled$forest)
forest[is.na(forest)] <- 0
forest <-  focal(forest, w=15, fun="mean", 
              expand = TRUE, na.rm = T)

agfor <- scale(cov_scaled$agfor)
agfor[is.na(agfor)] <- 0
agfor <-  focal(agfor, w=15, fun="mean", 
                 expand = TRUE, na.rm = T)

heter <- scale(cov_scaled$heter)
heter[is.na(heter)] <- 0
heter <-  focal(heter, w=15, fun="mean", 
                expand = TRUE, na.rm = T)

plot(forest)
plot(hfp)
plot(agfor)
plot(heter)




cmp <-  ~ 
  beta0_po(1, model = "linear", prec.linear = 0.01) +
  beta0_pa(1, model = "linear", prec.linear = 0.01) +
  beta0_paa(1, model = "linear", prec.linear = 0.01) +
  u(geometry, model = spde_co) + 
  v(geometry, model = spde_pa) +  
  u_copy(geometry, copy = "u", fixed = FALSE, 
         hyper = list(beta = list(prior = "normal", param = c(0, 1))))+
  hfp_cov(hfp, model = "linear") +  
  forest_cov(forest, model = "linear") +  
  agfor_cov(agfor, model = "linear") +
  hfp_cov_pa(hfp, model = "linear") +  
  forest_cov_pa(forest, model = "linear") +  
  agfor_cov_pa(agfor, model = "linear") +
  heter_cov(heter, model = "linear")

bru_options_set(
  bru_verbose = TRUE,
  verbose = TRUE,
  bru_max_iter = 1,
  control.inla = list(int.strategy = "eb")
)


lik_po <- bru_obs(
  formula = geometry ~ beta0_po + u  + hfp_cov + agfor_cov + forest_cov,
  family = "cp",
  data = collisions_sf,
  domain = list(geometry = mesh),
  samplers = outline
)

lik_pa <- bru_obs(
  formula = geometry ~  beta0_pa + v  + hfp_cov_pa + agfor_cov_pa + forest_cov_pa,
  family = "cp",
  data = pa_2022,
  domain = list(geometry = mesh),
  samplers = outline
)

lik_int <- bru_obs(
  formula = Presence ~ beta0_paa + u_copy + heter_cov  ,
  family = "binomial",
  data = pa_2022,
  domain = list(geometry = mesh),
  samplers = outline,
  control.family = list(link = "probit")
)

fit <- bru(
  cmp,lik_po, lik_pa, lik_int
)
summary(fit)

ppxl_out <- fm_pixels(mesh, mask = boundary_sf, format = "sf")
# ppxl_out <- fm_cprod(ppxl_out, data.frame(Presence = c(0, 1)))
lambda_out <- predict(
  fit,
  ppxl_out,
  ~ data.frame(
    lambda_po =  beta0_po + u  +  hfp_cov + agfor_cov + forest_cov ,
    lambda_pa = beta0_pa + v  + hfp_cov_pa + agfor_cov_pa + forest_cov_pa,
    w_po = u,
    w_pa = v, 
    w_copy  = u_copy,
    prob = pnorm( beta0_paa + u_copy + heter_cov )
  )
)


log_int <- ggplot(lambda_out$lambda_po) +
  geom_sf(aes(color = mean), size = 2) + 
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.9) +
  scale_color_viridis_c(
    option = "magma", 
    name = "Log-Intensity Presence only",
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



log_int_bis <- ggplot(lambda_out$lambda_pa) +
  geom_sf(aes(color = mean), size = 2) + 
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.9) +
  scale_color_viridis_c(
    option = "magma", 
    name = "Log-Intensity Presence Absence",
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



prob_int_bis <- ggplot(lambda_out$prob) +
  geom_sf(aes(color = mean), size = 2) + 
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.9) +
  scale_color_viridis_c(
    option = "magma", 
    name = "Prob",
    guide = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5, 
      barwidth = unit(10, "lines"), 
      barheight = unit(0.5, "lines")
    )
  ) +
  
  labs(
    title = "Prob"
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
    name = "Mean GP Presence Only",
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

w_pa <- ggplot(lambda_out$w_pa) +
  geom_sf(aes(color = mean), size = 2) + 
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.5) +
  scale_color_viridis_c(
    option = "magma", 
    name = "Mean GP Presence Absence",
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

w_pa_prob <- ggplot(lambda_out$w_copy) +
  geom_sf(aes(color = mean), size = 2) + 
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.5) +
  scale_color_viridis_c(
    option = "magma", 
    name = "Mean GP Presence Absence Copy",
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



log_int + w_po
ggplot() +
  geom_sf(data = collisions_sf, col = "firebrick", size = 0.1) + 
  geom_sf(data = outline, fill = NA, col = "black", size = 1) -> dat_plot_collisions

hfp.plot <- plot(fit, "hfp_cov") +
  ggtitle("Posterior of hfp PO") +
  theme(legend.position = "bottom")

agp.plot <- plot(fit, "agfor_cov") +
  ggtitle("Posterior of agfor_cov PO") +
  theme(legend.position = "bottom")

forest_cov.plot <- plot(fit, "forest_cov") +
  ggtitle("Posterior of forest_cov PO") +
  theme(legend.position = "bottom")


spde.range <- spde.posterior(fit, "u", what = "range")
spde.logvar <- spde.posterior(fit, "u", what = "log.variance")
range.plot <- plot(spde.range)
var.plot <- plot(spde.logvar)

presence_only <- (log_int |w_po |dat_plot_collisions |range.plot|var.plot)/
  (hfp.plot|agp.plot|forest_cov.plot)+ 
  plot_layout(guides = "collect") &
  theme(
    plot.title = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0)
  )

ggsave(file.path(plot_path, "Collisions_big_model.png"), presence_only, width = 40, height = 23, dpi = 100, units = "cm", 
       bg = "white")

ggplot() +
  geom_sf(data = pa_2022, aes(col = Npres), size = 1.1) + 
  geom_sf(data = outline, fill = NA, col = "black", size = 1) -> dat_plot_presence
layout <- "
AB
CD
"



hfp.plot <- plot(fit, "hfp_cov_pa") +
  ggtitle("Posterior of hfp PA") +
  theme(legend.position = "bottom")

agp.plot <- plot(fit, "agfor_cov_pa") +
  ggtitle("Posterior of agfor_cov PA") +
  theme(legend.position = "bottom")

forest_cov.plot <- plot(fit, "forest_cov_pa") +
  ggtitle("Posterior of forest_cov PA") +
  theme(legend.position = "bottom")


spde.range <- spde.posterior(fit, "v", what = "range")
spde.logvar <- spde.posterior(fit, "v", what = "log.variance")
range.plot <- plot(spde.range)
var.plot <- plot(spde.logvar)

fit$summary.hyperpar
forest_cov.plot <- plot(fit, "forest_cov_pa") +
  ggtitle("Posterior of forest_cov PA") +
  theme(legend.position = "bottom")

presenece_absence_plot <- (log_int_bis | w_pa|hfp.plot|agp.plot) /
  (range.plot | var.plot| forest_cov.plot| dat_plot_presence) +
  plot_layout(guides = "collect") &
  theme(
    plot.title = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0)
  )

ggsave(file.path(plot_path, "Point Process Presence Absence.png"), presenece_absence_plot, width = 40, height = 23, dpi = 100, units = "cm", 
       bg = "white")
summary(fit)
heter_cov.plot <- plot(fit, "heter_cov") +
  ggtitle("Posterior of heter_cov PA") +
  theme(legend.position = "bottom")

presence_absence_plot <- (prob_int_bis | w_pa_prob|heter_cov.plot)  +
  plot_layout(guides = "collect") &
  theme(
    plot.title = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0)
  )
table(pa_2022$Presence)
mean(pa_2022$Presence)
ggsave(file.path(plot_path, "prob_joint_model.png"), presence_absence_plot, width = 40, height = 23, dpi = 100, units = "cm", 
       bg = "white")
spde.posterior(fit, "u_copy")
spde.range <- spde.posterior(fit, "u", what = "range")
spde.logvar <- spde.posterior(fit, "u", what = "log.variance")
range.plot <- plot(spde.range)
var.plot <- plot(spde.logvar)
(range.plot / var.plot)
cov_int_plot <- hfp.plot + agro_for.plot + forest.plot + range.plot + var.plot


ggsave(file.path(plot_path, "collisions_other.png"), cov_int_plot, width = 40, height = 23, dpi = 100, units = "cm", 
       bg = "white")

plot(forest)
plot(agfor)
