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
cov_por <- rast(file.path(Conteos_folder, "covs_updated_new.tif"))
crs(cov_por) <- crs
cov_names_all <- names(cov_por)
cov_names <- c("temp", "prec", "elev", "urban", "dry", "irrig", "wood", "heter", "agfor", "forest",
               "mix", "bare", "hfp", "density", "density_km", "min_dist_to_any_road")
# sample_pts <- spatSample(cov_por[[cov_names]], size = 10000, method = "random", na.rm = TRUE)
# 
# cor_matrix <- cor(sample_pts, use = "complete.obs")
# 
# plot_file <- file.path(plot_path, "Covariate_Correlation_Matrix.jpeg")
# 
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
output_file <- file.path(data_path, "Conteos", "covs_updated_new.tif")
writeRaster(cov_por,
            filename = output_file,
            overwrite=TRUE)

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



# Record_table <- read_xlsx(path = file.path(Camera_folder, "RecordTable_CTs.xlsx")) # real presence absence
# Database_Dog <- read_xlsx(path = file.path(Camera_folder, "DatabaseDog_CTs.xlsx")) # here we have instead when dogs have been taken!
# Database_Dog$Timestamp <- as.POSIXct(Database_Dog$Timestamp)
# Database_Dog[Database_Dog$POINT_STANDARD == "DARIO_149 _ 04", ]
# Database_Dog_Filtered <- Database_Dog %>%
#   arrange(POINT_STANDARD, Timestamp) %>%
#   group_by(POINT_STANDARD) %>%
#   mutate(
#     time_diff = as.numeric(difftime(Timestamp, lag(Timestamp), units = "mins"))
#   ) %>%
#   filter(is.na(time_diff) | time_diff >= 30) %>%
#   select(-time_diff)
# 
# print(Database_Dog_Filtered, n = 20)
# Record_table %>%
#   select(POINT_STANDARD, EFFORT, X_COORD, Y_COORD, `Dog (P/A)`, INSTALLATION) |>
#   distinct(POINT_STANDARD, .keep_all = TRUE) -> Record_table_new
# 
# Database_Dog_Filtered %>%
#   left_join(Record_table_new, by = "POINT_STANDARD") -> prova
# 
# 
# Absence_Rows <- Record_table %>%
#   filter(`Dog (P/A)` == "no")
# # colnames(Final_Dataset)
# Final_Dataset <- bind_rows(prova, Absence_Rows) |>
#   select(-c( "Cat (P/A)" ))
# 
# 
# Final_Dataset <- Final_Dataset %>%
#   arrange(POINT_STANDARD, Timestamp) -> Final_Dataset
# Final_Dataset$EFFORT <- as.numeric(Final_Dataset$EFFORT)
# 
# Final_Dataset |>
#   filter(!is.na(EFFORT))|>
#   st_as_sf(coords = c("X_COORD", "Y_COORD"))|>
#   st_set_crs(4326) |>
#   st_transform(crs) -> Final_Dataset
# Final_Dataset$Timestamp <- as.POSIXct(Final_Dataset$Timestamp)
# Final_Dataset$EFFORT
# 
# write_sf(Final_Dataset, dsn = file.path(Camera_folder, "Presence_Absence.shp"))
PA_dogs <- read_sf( file.path(Camera_folder, "Presence_Absence.shp"))
PA_dogs$Timstmp
p_prova <- ggplot() + 
  geom_sf(data = boundary_sf, fill = "grey95", color = "black", linewidth = 0.5) +
  geom_sf(data = PA_dogs, aes(color = `Dg(P/A)`), size = 1, alpha = 0.6) 
p_prova
p4 <- ggplot() + 
  geom_sf(data = boundary_sf, fill = "grey95", color = "black", linewidth = 0.5) +
  geom_sf(data = camera_trap_dogs_clean, color = "firebrick", size = 1, alpha = 0.6) 

p4 + p_prova

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
                    "tertiary", "track", "trunk")


street_gis <- st_transform(street_gis, crs(cov_por))

street_gis %>%
  filter(fclass %in% target_classes) -> street_gis_relevant
grid_polys <- as.polygons(cov_por, dissolve = FALSE) |>
  st_as_sf()
grid_polys$id <- 1:nrow(grid_polys)
st_area(grid_polys)
road_segments <- st_intersection(street_gis_relevant, grid_polys)
road_segments$len_m <- as.numeric(st_length(road_segments))
# road_segments$cell_id <- 1:nrow(road_segments) 
table(road_segments$id)
density_data <- road_segments |>
  group_by(id) |>
  summarise(total_road_m = sum(len_m, na.rm = TRUE))

density_data_tab <- density_data |>
  st_drop_geometry()

density_data <- grid_polys |>
  left_join(density_data_tab, by = "id") |>
  mutate(
    total_road_m = replace_na(total_road_m, 0),
    density_km = (total_road_m/(4*1e6))*1000
  )



# density_data$density_km <- (density_data$total_road_m/(4*1e6))*1000
summary(density_data$density_km)
density_raster <- rast(cov_por, nlyrs = 1)
values(density_raster) <- 0
density_raster <- rasterize(density_data, density_raster, field = "density_km", fun = mean)
# density_filled <- focal(density_raster, w = 3, fun = mean, NAonly = TRUE)
# density_filled <- mask(density_filled, boundary_sf)
# density_final <- cover(density_raster, density_filled)

cov_por$density_km <- density_raster$density_km

p1 <- ggplot() +
  geom_spatraster(data = cov_por$density_km) +
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
# names(density_raster) <- "road_density_km_km2"
p1

ggsave(p1, filename = file.path(plot_path, "road_density.jpeg"), dpi = 100,
       height = 30, width = 50, units = "cm")
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
# presence_absence_dogs <- Final_Dataset
# presence_absence_dogs <-  read_sf( file.path(Camera_folder, "Presence_Absence.shp")) |>
#   st_filter(boundary_sf)
# presence_absence_dogs$Npres <- as.factor(presence_absence_dogs$`Dog (P/A)`)
# levels(presence_absence_dogs$Npres) <- c(0, 1)
# presence_absence_dogs$INSTALLATION <- dmy(presence_absence_dogs$INSTALLATION)
# presence_absence_dogs<- presence_absence_dogs[-which(year(presence_absence_dogs$INSTALLATION) == 2018), ]
# # write_sf(presence_absence_dogs, dsn = file.path(Camera_folder, "Presence_Absence.shp"))
# save(presence_absence_dogs, file = file.path(Camera_folder, "PA.RData"))
load(file = file.path(Camera_folder, "PA.RData"))
# plot_data <- presence_absence_dogs %>%
#   mutate(Year = year(INSTALL)) %>%
#   group_by(Year, Npres) %>%
#   summarise(Count = n(), .groups = 'drop')
# 
# pa_plot <- ggplot(plot_data, aes(x = as.factor(Year), y = Count, fill = as.factor(Npres))) +
#   geom_bar(stat = "identity") +
#   scale_fill_manual(values = c("0" = "#999999", "1" = "#E69F00"), 
#                     labels = c("Absence (0)", "Presence (1)")) +
#   labs(title = "Animal Presence Records by Year",
#        x = "Year",
#        y = "Number of Observations",
#        fill = "Record Status") +
#   theme_minimal()

# ggsave(pa_plot, filename = file.path(plot_path, "PA_barplot.jpeg"), dpi = 100, width = 30, height = 25, units = "cm")
presence_absence_dogs[presence_absence_dogs$POINT_STANDARD == "DARIO_149 _ 04", ]
summary(presence_absence_dogs$EFFORT)



station_summaries <- presence_station_summaries <- presence_station_summaries <- presence_absence_dogs %>%
  mutate(
    Year = year(INSTALLATION),
    Npres_num = as.numeric(as.character(Npres))
  ) %>%
  group_by(Year, POINT_STANDARD) %>%
  summarise(Total_Detections = sum(Npres_num, na.rm = TRUE), .groups = 'drop')

fit_data <- station_summaries %>%
  group_by(Year) %>%
  summarise(
    lambda = mean(Total_Detections),
    n_stations = n()
  ) %>%
  rowwise() %>%
  do(data.frame(
    Year = .$Year,
    x = 0:20,
    # Calculate Poisson PMF * total stations to match the bar heights
    y = dpois(0:20, .$lambda) * .$n_stations
  ))

# 3. Build the plot
det_plot <- ggplot(station_summaries, aes(x = Total_Detections)) +
  geom_bar(fill = "steelblue", color = "white", alpha = 0.7) +
  geom_line(data = fit_data, aes(x = x, y = y), 
            color = "red", size = 1) +
  geom_point(data = fit_data, aes(x = x, y = y), 
             color = "red", size = 1.5) +
  facet_wrap(~Year) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 550)) + 
  labs(
    title = "Frequency of Animal Detections per Station",
    subtitle = "Blue bars: Observed counts | Red line: Poisson distribution (MLE)",
    x = "Number of Detections",
    y = "Number of Stations"
  ) +
  theme_minimal()

print(det_plot)

ggplot(station_summaries, aes(x = Total_Detections)) +
  geom_bar(fill = "steelblue", color = "white") +
  facet_wrap(~Year) +
  labs(
    title = "Frequency of Animal Detections per Station",
    subtitle = "Number of stations categorized by how many times they recorded animals",
    x = "Number of Detections (Sightings)",
    y = "Number of Stations"
  ) +
  theme_minimal()
table(station_summaries$Total_Detections)

det_plot <-ggplot(station_summaries, aes(x = Total_Detections)) +
  geom_bar(fill = "steelblue", color = "white") +
  facet_wrap(~Year) +
  # This zooms the plot to 0-30 without deleting the outlier from the dataset
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 550)) + 
  labs(
    title = "Frequency of Animal Detections per Station",
    subtitle = "Reduced Number of stations categorized by detections",
    x = "Number of Detections",
    y = "Number of Stations"
  ) +
  theme_minimal()

det_plot_r <-ggplot(station_summaries, aes(x = Total_Detections)) +
  geom_bar(fill = "steelblue", color = "white") +
  facet_wrap(~Year) +
  # This zooms the plot to 0-30 without deleting the outlier from the dataset
  coord_cartesian(xlim = c(21, 175), ylim = c(0, 15)) + 
  labs(
    title = "Frequency of Animal Detections per Station",
    subtitle = "Reduced Number of stations categorized by detections",
    x = "Number of Detections",
    y = "Number of Stations"
  ) +
  theme_minimal()
ggsave(det_plot, filename = file.path(plot_path, "Barplot_Reduced.jpeg"), dpi = 100, width = 30, height = 25, units = "cm")
ggsave(det_plot_r, filename = file.path(plot_path, "Barplot_Reduced_right.jpeg"), dpi = 100, width = 30, height = 20, units = "cm")

station_summaries[which(station_summaries$Total_Detections == 174), ]

table(station_summaries$Total_Detections)

cov_por_std <- terra::scale(cov_por)
pa_env_values <- terra::extract(cov_por_std, vect(presence_absence_dogs))

pa_comparison <- presence_absence_dogs %>%
  st_drop_geometry() %>%
  bind_cols(pa_env_values) %>%
  select(`Dg(P/A)`, cov_names)

pa_long <- pa_comparison %>%
  pivot_longer(cols = -`Dg(P/A)`, names_to = "Variable", values_to = "Value") %>%
  mutate(Npres = ifelse(`Dg(P/A)` == "yes", "Presence", "Absence"))
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

p_prova <- ggplot() + 
  geom_spatraster(data = cov_por[["forest"]]) +
  #geom_sf(data = boundary_sf, fill = "grey95", color = "black", linewidth = 0.5) +
  geom_sf(data = presence_absence_dogs, aes(color = `Dog (P/A)`), size = 1, alpha = 0.6) 
p_prova

library(mapview)
mapview(presence_absence_dogs, zcol = "Npres")
# p_prova2 <- ggplot() + 
#   geom_sf(data = boundary_sf, fill = "grey95", color = "black", linewidth = 0.5) +
#   geom_sf(data = presence_absence_dogs, aes(color = as.factor(Npres)), size = 1, alpha = 0.6) 
# p_prova + p_prova2
cov_names_all

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
presence_absence_l <- st_transform(presence_absence_dogs, 4326)
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

# 
# save(boundary_l, collision_l, camera_l, counts_l, roads_l, quad_l, network_l, 
#      presence_absence_l,file = "leaflet.RData")
load("leaflet.RData")
target_classes <- c("motorway", "primary", "residential", "secondary", 
                    "tertiary", "track", "trunk", "unclassified")
road_pal <- colorFactor(palette = "viridis", domain = target_classes)
quad_centroids <- st_centroid(quad_l)
camera_years <- split(camera_l, camera_l$year)
available_years <- names(camera_years)
pa_pal <- colorFactor(
  palette = c("grey70", "#27ae60"),  # 0 = assenza, 1 = presenza
  domain = c("no", "yes")
)
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
  addCircleMarkers(
    data = presence_absence_l,
    color = ~pa_pal(`Dg(P/A)`),
    radius = 5,
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.7,
    group = "Presence / Absence")%>%
  
  
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
      "Presence / Absence",
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
# 
# pt.gadm <- st_as_sf(gadm(country='Portugal', level=0))
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
# 
# 
# pt.crop <- st_crop(pt.gadm, pt.bbox)
# pt.clean <- pt.crop |>
#   st_union() |>
#   st_make_valid()
# 
# # boundary_sf
# pt.clean |>
#   st_transform(crs) -> boundary_sf
# save(boundary_sf, file = file.path(data_path, "Boundary_sf.RData"))
# ggplot() +
#   geom_sf(data = pt.clean, fill = "grey95", color = "black", linewidth = 0.5) 
