# ==========================================================================
# SETUP: packages, paths, boundary, covariates, CRS definitions and Import Data
# ==========================================================================

path <- "C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats"
setwd(path)

data_path  <- file.path(path, "Data")
utils_path <- file.path(path, "Utils")
plot_path  <- file.path(path, "Figures/Figure Data Collection")
Database_Folder  <- file.path(data_path, "Database_Dogs&Cats")
Conteos_folder   <- file.path(data_path, "Conteos")

source(file.path(utils_path, "Packages.R"))

# Study area boundary (already an sf object)
load(file.path(data_path, "Boundary_sf.RData"))
crs_km <- fm_crs_set_lengthunit(fm_crs("EPSG:3035"), "km")

# Reproject boundary into the km-unit CRS used for modeling
boundary_sf <- fm_transform(boundary_sf, crs_km)
boundary_sf <- boundary_sf |>
  st_make_valid() |>
  st_union() |>
  st_cast("POLYGON")

boundary_sf <- boundary_sf[which.max(st_area(boundary_sf)), ]

boundary_union <- st_union(boundary_sf) 
boundary_clean <- st_buffer(boundary_union, dist = 0)
boundary_final <- st_concave_hull(boundary_clean, ratio = 0.0025) 
boundary_sf <- boundary_final
load(file = file.path(data_path, "Model_Data.RData"))
colnames(count_dogs)[6] <- "Counts"
theme_pub_fit_bis <- theme_minimal(base_size = 15) +
  theme(
    plot.title      = element_text(face = "bold", size = 18, hjust = 0.5,
                                   margin = margin(b = 6)),
    axis.title      = element_text(size = 15),
    axis.text       = element_text(size = 9, colour = "black"),
    legend.title    = element_text(size = 13),
    legend.text     = element_text(size = 10)
  )


# Projection for the plot  ------------------------------------------------
boundary_wgs   <- st_transform(boundary_sf, 4326)
collisions_wgs <- st_transform(collisions_2022, 4326)
pa_wgs         <- st_transform(pa_2022, 4326)
counts_wgs     <- st_transform(count_dogs, 4326)

final_map <- ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0, progress = "none") +
  
  # Study area outline
  geom_sf(data = boundary_wgs, fill = NA, color = "black", linewidth = 0.4) +
  
  # Collisions (presence-only)
  geom_sf(data = collisions_wgs, aes(shape = "Collision"),
          color = "darkgreen", size = 1.3, alpha = 0.85) +
  
  # Camera-trap presence/absence
  geom_sf(data = pa_wgs, aes(color = factor(Presence)), size = 1.3) +
  scale_color_manual(
    name   = "Camera trap",
    values = c("0" = "#4575B4", "1" = "#D73027"),
    labels = c("0" = "Absence", "1" = "Presence")
  ) +
  
  new_scale_color() + 
  
  # Transect counts
  geom_sf(data = counts_wgs, aes(size = Counts),
          alpha = 0.65) +
  scale_color_viridis_c(name = "Transect count", option = "C") +
  
  scale_shape_manual(name = NULL, values = c("Collision" = 17)) +
  
  coord_sf(crs = st_crs(4326)) +
  annotation_scale(location = "bl", width_hint = 0.2)+
  labs(title = "Free-Roaming Dogs", x = NULL, y = NULL) +
  theme(legend.box = "vertical") +
  theme_pub_fit_bis

final_map

ggsave(file.path(plot_path, "Data_Collection.png"), final_map, width = 6, height = 8, dpi = 300, bg = "white")
