# In this RScript is contained the exploratory data analysis for all the data that we have.
setwd("C:/Users/gmsan/Documenti/GitHub/Street_Dog_Cats")
utils_path <- file.path(getwd(), "Utils") # utils path
source(file.path(utils_path, "Packages.R")) # source the packages
data_path <- file.path(getwd(), "Data") # define data path
boundary_path <- file.path(data_path, "Boundary_sf.RData") # import boundaries --> it can be done smoother
eda_path <- file.path(getwd(), "EDA") # define eda path
plot_path <- file.path(eda_path, "Plots") # define plot_path
crs <- "EPSG:3035" # CRS:  EPSG:3035 - ETRS89-extended / LAEA Europe --> crs to project to every data
load(boundary_path)
Conteos_folder <- file.path(data_path, "Conteos") # define conteos folder path
Camera_folder <- file.path(data_path, "Camera Trap") # define camera folder path
Database_Folder <- file.path(data_path, "Database_Dogs&Cats") # define Database Folder

###### old code unuused ####
# save(boundary_sf, file = file.path(data_path, "Boundary.RData"))
# boundary_vect <- as.polygons(ext(density_pos), crs = crs(density_pos))
# 
# data_mask <- density_pos
# values(data_mask) <- ifelse(!is.na(values(density_pos)), 1, NA)
# boundary_vect <- as.polygons(data_mask, dissolve = TRUE)
# boundary_sf <- st_as_sf(boundary_vect)
# boundary_sf <- boundary_sf[, "geometry"]
# boundary_sf <- st_transform(boundary_sf, crs = crs)

###### Conteos_Folder  #######
# density_por <- rast(file.path(Conteos_folder, "denspop_2.tif")) # population density
# crs <- crs(density_por) 
# hfp <- rast(file.path(Database_Folder, "hfp_2.tif")) # human foot print

# crs(density_por) <- crs
cov_por <- rast(file.path(Conteos_folder, "covs_updated.tif"))
crs(cov_por) <- crs
cov_names_all <- names(cov_por)
cov_names <- c("temp", "prec", "elev", "urban", "dry", "irrig", "wood", "heter", "agfor", "forest",
               "mix", "bare", "hfp", "density")
# sample_pts <- spatSample(cov_por, size = 10000, method = "random", na.rm = TRUE)
# 
# cor_matrix <- cor(sample_pts, use = "complete.obs")
# 
# plot_file <- file.path(plot_path, "Covariate_Correlation_Matrix.jpeg")

# jpeg(plot_file, width = 20, height = 20, units = "cm", res = 100)
# corrplot(cor_matrix, 
#          method = "ellipse",             
#          type = "lower",               
#          order = "hclust", 
#          addrect = 5,
#          number.cex = 0.8,            
#          tl.col = "black", 
#          tl.srt = 45,                
#          col = brewer.pal(n = 10, name = "RdBu"), 
#          diag = FALSE,                
#          mar = c(0, 0, 1, 0),        
#          title = "Corr Environmental Covariates")
# 
# dev.off()

# names(cov_por)[1:12] <- cov_names
# cov_por[["hfp"]] <- hfp
# cov_por[["density"]] <- density_por
# output_file <- file.path(data_path, "Conteos", "covs_updated_new.tif")
# writeRaster(cov_por,
#             filename = output_file,
#             overwrite=TRUE)

network_por  <- read_sf(file.path(Conteos_folder, "network.shp")) |>
  st_as_sf() |>
  st_transform(crs)



quadriculas_por  <- read_sf(file.path(Conteos_folder, "Quadriculas_transectos_Portugal.shp")) |>
  st_as_sf() |>
  st_transform(crs) |>
  mutate(area_quadriculas = st_area(geometry))



p1 <- ggplot() +
  geom_spatraster(data = density_por) +
  scale_fill_viridis_c(
    option = "magma", 
    trans = "sqrt", 
    na.value = "transparent" 
  ) +
  geom_sf(data = boundary_sf, fill = NA, color = "green", linewidth = 0.5)+
  coord_sf() +
  labs(
    title = "Population Density Portugal in 2021",
    fill = "Density"
  ) +
  theme_minimal()
# ggsave(p1, filename = file.path(plot_path, "Density.jpeg"), dpi = 100)
p1
p2 <- ggplot()+
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.5)+
  coord_sf()+
  labs(
    title = "Domain Boundary"
  ) + 
  theme_minimal()

# ggsave(p2, filename = file.path(plot_path, "boundary.jpeg"), dpi = 100)

p_hfp <- ggplot() +
  geom_spatraster(data = hfp) +
  scale_fill_viridis_c(
    option = "viridis",
    name = "HFP",
    na.value = "transparent",
    guide = guide_colorbar(
      barheight = unit(3, "cm"),
      barwidth = unit(0.8, "cm"),
      title.position = "top"
    )) +
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.5)+
  coord_sf() +
  labs(
    title = "",
    fill = "HFP"
  ) +
  theme_minimal(base_size = 20) +
  theme(legend.key.size = unit(0.4, "cm")) 

# cov_names_other <- cov_names_all %in% cov_names
# plot_list <- lapply(cov_names_all[cov_names_other == FALSE], function(lyr_name) {
#   masked_lyr <- cov_por[[lyr_name]] |> 
#     mask(vect(boundary_sf))
#   
#   ggplot() +
#     geom_spatraster(data = masked_lyr) + 
#     geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.5) +
#     scale_fill_viridis_c(
#       option = "viridis",
#       name = lyr_name,
#       na.value = "transparent",
#       guide = guide_colorbar(
#         barheight = unit(3, "cm"),
#         barwidth = unit(0.8, "cm"),
#         title.position = "top"
#       )
#     ) +
#     coord_sf() +
#     theme_minimal(base_size = 20) +
#     theme(
#       legend.key.size = unit(0.4, "cm"),
#       panel.grid = element_blank() # Optional: removes grid lines for a cleaner "cut" look
#     )
# })
# p3 <- wrap_plots(plot_list, ncol = 5) + 
#   plot_annotation(title = "Portugal Covariates",  theme = theme(plot.title = element_text(size = 30)))
# 
# pdist <- wrap_plots(plot_list, ncol = 5) + 
#   plot_annotation(title = "Portugal Distances",  theme = theme(plot.title = element_text(size = 30)))
# 
# ggsave(pdist, filename = file.path(plot_path, "dist.jpeg"), dpi = 100, height = 50, width = 85,
#        units = "cm")

ggplot(data = network_por) +
  geom_sf(aes(color = fclass), linewidth = 0.4) +
  geom_sf(data = boundary_sf, fill = NA, color = "black") + 
  scale_color_viridis_d(option = "plasma") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Road Network by Class",
       color = "Road Type") +
  theme(legend.position = "right")
  
network_por |>
  group_by(fclass) |>
  summarise(size = n()) |>
  arrange(desc(size))|>
  print(n = 25)

unique(network_por$fclass)

ggplot(data = quadriculas_por) +
  geom_sf(aes(geometry = geometry), color = "red", linewidth = 2) +
  geom_sf(data = boundary_sf, fill = NA, color = "black")+
scale_color_viridis_d(option = "plasma") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Road Network by Class",
       color = "Road Type") +
  theme(legend.position = "right")


##### Camera Trap Folder ####
load(file.path(Camera_folder, "Camera_dogs.RData"))
# camera_trap_dogs <- read_excel(file.path(Camera_folder, "dogs_records_final.xlsx"))|>
#   st_as_sf(coords = c("X", "Y")) |>
#   st_set_crs(4326) |>
#   st_transform(crs)
# 
# test_coords <- read_excel(file.path(Camera_folder, "dogs_records_final.xlsx"))
# summary(test_coords$X)
# summary(test_coords$Y)
# overlap <- st_intersects(camera_trap_dogs, boundary_sf, sparse = FALSE)
# camera_trap_dogs$in_out <- overlap
# test_coords[!overlap, ]
# 
# cat("Points inside boundary:", sum(overlap), "out of", nrow(camera_trap_dogs))
# camera_trap_dogs_clean <- camera_trap_dogs %>%
#   st_filter(boundary_sf) %>%
#   mutate(
#     timestamp = dmy_hm(Timestamp),    
#     year = year(timestamp),            
#     month = month(timestamp)           
#   )

p4 <- ggplot() + 
  geom_sf(data = boundary_sf, fill = "grey95", color = "black", linewidth = 0.5) +
  geom_sf(data = camera_trap_dogs, color = "firebrick", size = 1, alpha = 0.6) 

#ggsave(p4, filename = file.path(plot_path, "Camera_traps_not_cleaned.jpeg"), dpi = 100)
p5 <- ggplot() +
  geom_sf(data = boundary_sf, fill = "grey95", color = "black", linewidth = 0.5) +
  geom_sf(data = camera_trap_dogs_clean, color = "firebrick", size = 1, alpha = 0.6) +
  facet_wrap(~year) +
  
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Dog Camera Trap Occurrences by Year",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold")
  )
# ggsave(p5, filename = file.path(plot_path, "Camera_traps_cleaned.jpeg"), dpi = 100)
# save(camera_trap_dogs_clean, file = file.path(Camera_folder, "Camera_dogs.RData"))


coll_values <- terra::extract(cov_por, vect(camera_trap_dogs_clean)) %>%
  select(-ID) %>%
  pivot_longer(cols = cov_names, names_to = "variable", values_to = "value") %>%
  na.omit()

env_col_plot <- ggplot(coll_values, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(outlier.shape = 1, alpha = 0.7, color = "black") +
  facet_wrap(~variable, scales = "free") +
  scale_fill_viridis_d(option = "mako", guide = "none") + # Clean colors
  labs(
    title = "Environmental Profile of Camera Trap Sites",
    x = NULL,
    y = "Extracted Value"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(env_col_plot, filename = file.path(plot_path, "Env_Camera_Trap.jpeg"), dpi = 100,
       height = 30, width = 50, units = "cm")





###### Road Collisions ##### 

collision_dogs <- read_sf(file.path(Database_Folder, "Collisions_dogs_new.shp")) |>
  st_filter(boundary_sf)|>
  st_transform(crs)

# coll_values <- terra::extract(cov_por, vect(collision_dogs)) %>%
#   select(-ID) %>%
#   pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
#   na.omit()
# 
# env_col_plot <- ggplot(coll_values, aes(x = variable, y = value, fill = variable)) +
#   geom_boxplot(outlier.shape = 1, alpha = 0.7, color = "black") +
#   facet_wrap(~variable, scales = "free") +
#   scale_fill_viridis_d(option = "mako", guide = "none") + # Clean colors
#   labs(
#     title = "Environmental Profile of Dog Collision Sites",
#     x = NULL,
#     y = "Extracted Value"
#   ) +
#   theme_minimal(base_size = 20) +
#   theme(
#     strip.text = element_text(face = "bold"),
#     panel.grid.minor = element_blank()
#   )
# ggsave(env_col_plot, filename = file.path(plot_path, "Env_Col.jpeg"), dpi = 100,
#        height = 30, width = 50, units = "cm")


street_gis <- st_read(file.path(Database_Folder, "gis_osm_roads_free_1.shp")) |>
  st_transform(crs)

nearest_road_indices <- st_nearest_feature(collision_dogs, street_gis)
collision_dogs$road_class <- street_gis$fclass[nearest_road_indices]
relevant_roads <- street_gis[unique(nearest_road_indices), ]
map_bounds <- st_bbox(collision_dogs)
# 
# 
target_classes <- c("motorway", "primary", "residential", "secondary",
                    "tertiary", "track", "trunk", "unclassified")
# 
# plot_collisions <- collision_dogs %>%
#   filter(road_class %in% target_classes)
# 
# plot_roads <- relevant_roads %>%
#   filter(fclass %in% target_classes)
# 
# road_col_plot <- ggplot() +
#   geom_sf(data = boundary_sf, fill = "grey95", color = "black") +
#   geom_sf(data = plot_roads, color = "grey90", size = 1.2) +
#   geom_sf(data = plot_collisions, aes(color = road_class), size = 1.2, alpha = 0.8) +
#   
#   facet_wrap(~road_class, ncol = 4) +
#   coord_sf(xlim = c(map_bounds["xmin"], map_bounds["xmax"]), 
#            ylim = c(map_bounds["ymin"], map_bounds["ymax"])) +
#   
#   scale_color_viridis_d(option = "turbo", guide = "none") +
#   labs(
#     title = "Dog Collision Hotspots by Road Classification",
#     x = NULL, y = NULL
#   ) +
#   theme_minimal(base_size = 25) +
#   theme(
#     strip.text = element_text(face = "bold", size = 12)
#   )


# ggsave(road_col_plot, filename = file.path(plot_path, "Road_col_plot.jpeg"), dpi = 100,
#        height = 30, width = 50, units = "cm")

# raster_template <- cov_por[[1]] 
# values(raster_template) <- NA 
# 
# dist_layers <- list()
# 
# for (r_class in target_classes) {
#   message("Processing distance to: ", r_class)
#   
#   temp_roads <- street_gis %>% 
#     filter(fclass == r_class) %>%
#     st_filter(st_as_sfc(st_bbox(cov_por)))
# 
#   road_rast <- rasterize(vect(temp_roads), raster_template, field = 1)
#   
#   dist_layers[[r_class]] <- distance(road_rast)
#   
#   names(dist_layers[[r_class]]) <- paste0("dist_", r_class)
# }
# 
# road_distances_stack <- rast(dist_layers)
# min_dist_raster <- min(road_distances_stack, na.rm = TRUE)
# names(min_dist_raster) <- "min_dist_to_any_road"
# cov_por_extended <- c(cov_por, min_dist_raster)
# plot(min_dist_raster, main = "Minimum Distance to Nearest Target Road (m)")
# cov_por_extended <- c(cov_por_extended, road_distances_stack)


# output_file <- file.path(data_path, "Conteos", "covs_updated.tif")
# writeRaster(cov_por_extended,
#             filename = output_file,
#             overwrite=TRUE)

####### Presence Absence ##### 

presence_absence_dogs <-  read_sf(file.path(Database_Folder, "PA_dog.shp")) |>
  st_filter(boundary_sf)
presence_absence_dogs$Npres_factor <- as.factor(presence_absence_dogs$Npres)

table(presence_absence_dogs$Npres)
cov_por_std <- terra::scale(cov_por)
pa_env_values <- terra::extract(cov_por_std, vect(presence_absence_dogs))

pa_comparison <- presence_absence_dogs %>%
  st_drop_geometry() %>%
  bind_cols(pa_env_values) %>%
  select(Npres, names(cov_por))

pa_long <- pa_comparison %>%
  pivot_longer(cols = -Npres, names_to = "Variable", values_to = "Value") %>%
  mutate(Npres = ifelse(Npres == 1, "Presence", "Absence"))
pa_long_clean <- pa_long %>%
  drop_na(Value)

cov_env_pres <- ggplot(pa_long_clean, aes(x = Npres, y = Value, fill = Npres)) +
  geom_violin(alpha = 0.3, color = NA) + # Shows the distribution density
  geom_boxplot(width = 0.2, outlier.shape = NA, lwd = 0.7) +
  facet_wrap(~Variable, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c("Absence" = "#E69F00", "Presence" = "#56B4E9")) +
  labs(title = "Environmental Cov: Presence vs. Absence",
       x = "", y = "Variable Value") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

# ggsave(cov_env_pres, filename = file.path(plot_path, "PA_Niche_Comparison.jpeg"), dpi = 100, width = 30, height = 25, units = "cm")

p_abs <- ggplot() +
  geom_sf(data = boundary_sf, fill = "grey95", color = "black") +
  geom_sf(data = presence_absence_dogs[presence_absence_dogs$Npres == 0, ], aes(geometry = geometry),
          fill = "black", size = 0.8) +
  labs(title = "Known Absences") +
  theme_minimal()

p_pres <- ggplot() +
  geom_sf(data = boundary_sf, fill = "grey95", color = "black") +
  geom_sf(data = subset(presence_absence_dogs, Npres == 1), 
          color = "black") +
  labs(title = "Known Presences") +
  theme_minimal()

p_dots <- ggplot() +
  geom_sf(data = boundary_sf, fill = "grey95", color = "black") +
  geom_sf(data = camera_trap_dogs_clean, color = "#0072B2", size = 0.9, alpha = 0.4) +
  labs(title = "Raw Records") +
  theme_minimal()


camera_plot <- (p_abs | p_pres | p_dots) + 
  plot_annotation(theme = theme(plot.title = element_text(size = 20, face = "bold")))
# ggsave(camera_plot, filename = file.path(plot_path, "Camera_traps_compared.jpeg"), dpi = 100, 
#        height = 30, width = 50, units = "cm")




ggplot() + 
  geom_sf(data = boundary_sf, fill = "grey95", color = "black", linewidth = 0.5) +
  geom_sf(data = presence_absence_dogs, aes(color = Npres), size = 2, alpha = 0.8) +
  scale_color_viridis_c(option = "magma", name = "Dog Presence") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Dog Presence/Absence across Portugal")
###### Counts ######



# count_dogs <- read_sf(file.path(Database_Folder, "Counts_dogs_new.shp"))|>
#   st_transform(crs)|>
#   st_filter(boundary_sf)|>
#   st_filter(quadriculas_por)|>
#   st_join(quadriculas_por |> select(area_quadriculas, Area), left = FALSE)
# count_dogs$Effrt_m <- gsub(count_dogs$Effrt_m, pattern = ",", replacement = ".")
# count_dogs$Effrt_m <- as.numeric(count_dogs$Effrt_m )
# write_sf(count_dogs, dsn = file.path(Database_Folder, "Counts_dogs_new.shp"))
count_dogs <- read_sf(file.path(Database_Folder, "Counts_dogs_new.shp"))




p_effort <- ggplot() + 
  geom_sf(data = boundary_sf, fill = "grey98", color = "grey40", linewidth = 0.3) +
  geom_sf(data = count_dogs, 
          aes(color = (Dog_obs / Effrt_m *1000), 
              size = Dog_obs), 
          alpha = 0.8) +
  scale_color_viridis_c(option = "mako", 
                        name = "Dogs / km") +
  scale_size_continuous(range = c(3, 12),
                        name = "Total Count") +

  labs(title = "Dog Relative Abundance") +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 20),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2)
  )

# ggsave(p_effort, filename = file.path(plot_path, "effort.jpeg"), dpi = 100,
#        height = 30, width = 50, units = "cm")

cor(count_dogs$Effrt_m/1000, count_dogs$Dog_obs)

count_values <- terra::extract(cov_por, vect(count_dogs))

eda_counts <- count_dogs %>%
  st_drop_geometry() %>%
  bind_cols(count_values %>% select(-ID)) %>%
  mutate(rai_km = (Dog_obs / Effrt_m) * 1000)

eda_long <- eda_counts %>%
  select(rai_km, all_of(cov_names)) %>%
  pivot_longer(cols = all_of(cov_names), 
               names_to = "variable", 
               values_to = "value")


eda_long <- eda_long %>%
  mutate(rai_level = cut(rai_km, 
                         breaks = quantile(rai_km, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                         labels = c("Low RAI", "Med RAI", "High RAI"),
                         include.lowest = TRUE))

p_rai <- ggplot(eda_long, aes(x = rai_level, y = value, fill = rai_level)) +
  geom_boxplot(outlier.shape = 1, alpha = 0.7) +
  facet_wrap(~variable, scales = "free_y", ncol = 4) +
  scale_fill_viridis_d(option = "plasma", name = "Dog Density") +
  labs(title = "Covariate Distributions Across Dog Abundance Levels",
       x = "Relative Abundance Level",
       y = "Covariate Value") +
  theme_minimal(base_size = 20) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


ggsave(p_rai, filename = file.path(plot_path, "cov_rai.jpeg"), dpi = 100,
       height = 30, width = 50, units = "cm")

####### Leafleet map #####
boundary_l  <- st_transform(boundary_sf, 4326)
collision_l <- st_transform(collision_dogs, 4326)
camera_l    <- st_transform(camera_trap_dogs_clean, 4326)
counts_l    <- st_transform(count_dogs, 4326) # Uses the eda_counts we made with RAI
counts_l$rai_km <- (counts_l$Dog_obs/ count_dogs$Effrt_m)*1000 # this represents the amount of dogs captured per km
names(collision_l)
roads_l <- st_transform(relevant_roads, 4326)
quad_l <- quadriculas_por %>% 
  st_transform(4326) %>%
  filter(id %in% counts_l$id)
quad_l$counts <- round((counts_l$Dog_obs/ count_dogs$Effrt_m)*1000, 3)
grid_id_pal <- colorFactor(palette = "Set3", domain = quad_l$id)

network_l <- network_por %>% 
  st_transform(4326)


# save(boundary_l, collision_l, camera_l, counts_l, roads_l, quad_l, network_l,  file = "leaflet.RData")
load("leaflet.RData")
target_classes <- c("motorway", "primary", "residential", "secondary", 
                    "tertiary", "track", "trunk", "unclassified")
road_pal <- colorFactor(palette = "viridis", domain = target_classes)
quad_centroids <- st_centroid(quad_l)
camera_years <- split(camera_l, camera_l$year)
available_years <- names(camera_years)
map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = boundary_l, color = "black", weight = 2, fillOpacity = 0, group = "Study Area")
  
for(yr in available_years) {
  map <- map %>%
    addCircleMarkers(
      data = camera_years[[yr]],
      color = "#2980b9", 
      radius = 5, 
      stroke = TRUE, 
      weight = 1, 
      fillOpacity = 0.8,
      group = paste("Camera Traps -", yr), # Unique group name per year
      label = ~paste("Year:", yr)
    )
}
map <- map %>%  
  addPolygons(data = quad_l, 
              color = "white", 
              weight = 1, 
              fillColor = "grey", 
              fillOpacity = 0.4,
              group = "Sampling Grids",
              label = ~paste("Grid ID:", id)) %>%
  addLabelOnlyMarkers(data = quad_centroids,
                      label = ~counts,
                      group = "RAI Values",
                      labelOptions = labelOptions(
                        noHide = TRUE, 
                        direction = 'center', 
                        textOnly = TRUE,
                        style = list(
                          "color" = "black",
                          "font-weight" = "bold",
                          "font-size" = "14px"
                        )
                      )) %>%
  addPolylines(data = network_l,
               color = "#2c3e50", 
               weight = 2, 
               opacity = 0.8,
               group = "Local Network",
               label = "Sampling Transect") %>%
  
  addPolylines(data = roads_l,
               color = ~road_pal(fclass),
               weight = 1.2, opacity = 0.4,
               group = "OSM Roads",
               label = ~fclass) %>%
  
  addCircleMarkers(data = collision_l,
                   color = "#e74c3c", radius = 4, stroke = FALSE, fillOpacity = 0.7,
                   group = "Collisions",
                   label = ~paste("Roadkill - Near:", road_class)) %>%
  
  # addCircleMarkers(data = camera_l,
  #                  color = "#2980b9", radius = 5, stroke = TRUE, weight = 1, fillOpacity = 0.8,
  #                  group = "Camera Traps") %>%
  
  addCircleMarkers(data = counts_l,
                   color = "#f39c12", radius = ~rai_km * 4 + 3, stroke = TRUE, weight = 1, fillOpacity = 0.6,
                   group = "Counts (RAI)",
                   popup = ~paste0("<b>RAI:</b> ", round(rai_km, 2))) %>%
  
  addLayersControl(
    overlayGroups = c(
      "Sampling Grids", 
      "RAI Values",     
      "Local Network", 
      "Collisions Roads", 
      "Collisions", 
      "Counts (RAI)",
      paste("Camera Traps -", available_years) 
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  addLegend(position = "bottomright", pal = road_pal, values = target_classes, title = "OSM Road Types") %>%
  addLegend(position = "bottomleft", 
            colors = c("#e74c3c", "#2980b9", "#f39c12", "#2c3e50", "#95a5a6"), 
            labels = c("Collisions", "Camera Traps", "Counts (RAI)", "Transect Network", "Grids"),
            title = "Survey Components")

map
####### Portugal Boundary #####

# pt.gadm <- gadm(country='Portugal', level=0)
# pt.lim = data.frame(ylim=c(36.6, 43), xlim=c(-10, -4.0))
# pt.bbox <- st_bbox(c(xmin=pt.lim$xlim[1],
#                      xmax=pt.lim$xlim[2],
#                      ymin=pt.lim$ylim[1],
#                      ymax=pt.lim$ylim[2]))
# 
# loc.lim = data.frame(ylim=c(40.6, 40.9), xlim=c(-8.8, -8.4))
# loc.bbox <- st_bbox(c(
#   xmin=loc.lim$xlim[1],
#   xmax=loc.lim$xlim[2],
#   ymin=loc.lim$ylim[1],
#   ymax=loc.lim$ylim[2]))
# pt.gadm <- sf::st_as_sf(pt.gadm) %>% 
#   st_crop(pt.bbox)
# boundary_sf
# pt.gadm |>
#   st_transform(crs) -> boundary_sf
#save(boundary_sf, file = file.path(data_path, "Boundary_sf.RData"))
# ggplot() + 
#   geom_sf(data = boundary_sf, fill = "grey95", color = "black", linewidth = 0.5) +
#   geom_sf(data = pt.gadm, color = "red", linewidth = 0.5) 
