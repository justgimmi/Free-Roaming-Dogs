# In this RScript is contained the exploratory data analysis for all the data that we have.
setwd("C:/Users/gmsan/Documenti/GitHub/Street_Dog_Cats")
utils_path <- file.path(getwd(), "Utils")
source(file.path(utils_path, "Packages.R"))
data_path <- file.path(getwd(), "Data")
boundary_path <- file.path(data_path, "Boundary.RData")
eda_path <- file.path(getwd(), "EDA")
plot_path <- file.path(eda_path, "Plots")
crs <- "EPSG:3035" # CRS:  EPSG:3035 - ETRS89-extended / LAEA Europe --> crs to project to every data
load(boundary_path)

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
Conteos_folder <- file.path(data_path, "Conteos")
density_por <- rast(file.path(Conteos_folder, "denspop_2.tif"))
crs(density_pos) <- crs
cov_por <- rast(file.path(Conteos_folder, "covs.tif"))
crs(cov_por) <- crs
cov_names <- names(cov_por)

network_por  <- read_sf(file.path(Conteos_folder, "network.shp")) |>
  st_as_sf() |>
  st_transform(crs)

quadriculas_por  <- read_sf(file.path(Conteos_folder, "Quadriculas_transectos_Portugal.shp")) |>
  st_as_sf() |>
  st_transform(crs)


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
ggsave(p1, filename = file.path(plot_path, "Density.jpeg"), dpi = 100)

p2 <- ggplot()+
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.5)+
  coord_sf()+
  labs(
    title = "Domain Boundary"
  ) + 
  theme_minimal()

ggsave(p2, filename = file.path(plot_path, "boundary.jpeg"), dpi = 100)



plot_list <- lapply(names(cov_por), function(lyr_name) {
  ggplot() +
    geom_spatraster(data = cov_por[[lyr_name]]) +
    geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.2) +
    scale_fill_viridis_c(
      option = "viridis",
      name = lyr_name,
      na.value = "transparent",
      guide = guide_colorbar(
        barheight = unit(3, "cm"),
        barwidth = unit(0.8, "cm"),
        title.position = "top"
      )
    ) +
    coord_sf() +
    theme_minimal(base_size = 30) +
    theme(legend.key.size = unit(0.4, "cm")) 
})

p3 <- wrap_plots(plot_list, ncol = 4) + 
  plot_annotation(title = "Portugal Covariates",  theme = theme(plot.title = element_text(size = 30)))

ggsave(p3, filename = file.path(plot_path, "cov.jpeg"), dpi = 100, height = 70, width = 70, 
       units = "cm")

ggplot(data = network_por) +
  geom_sf(aes(color = fclass), linewidth = 0.4) +
  geom_sf(data = boundary_sf, fill = NA, color = "black")
  scale_color_viridis_d(option = "plasma") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Road Network by Class",
       color = "Road Type") +
  theme(legend.position = "right")
  
network_por |>
  group_by(fclass) |>
  summarise(n())

unique(network_por$fclass)

ggplot(data = quadriculas_por) +
  geom_sf(aes(geometry = geometry), color = "red") +
  geom_sf(data = boundary_sf, fill = NA, color = "black")
scale_color_viridis_d(option = "plasma") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Road Network by Class",
       color = "Road Type") +
  theme(legend.position = "right")

##### Camera Trap Folder ####
Camera_folder <- file.path(data_path, "Camera Trap")
load(file.path(Camera_folder, "Camera_dogs.RData"))
camera_trap_dogs <- read_excel(file.path(Camera_folder, "dogs_records_final.xlsx"))|>
  st_as_sf(coords = c("X", "Y")) |>
  st_set_crs(4326) |>
  st_transform(crs)

camera_trap_dogs_clean <- camera_trap_dogs %>%
  st_filter(boundary_sf) %>%
  mutate(
    timestamp = dmy_hm(Timestamp),    
    year = year(timestamp),            
    month = month(timestamp)           
  )

p4 <- ggplot() + 
  geom_sf(data = boundary_sf, fill = "grey95", color = "black", linewidth = 0.5) +
  geom_sf(data = camera_trap_dogs, color = "firebrick", size = 1, alpha = 0.6) 

ggsave(p4, filename = file.path(plot_path, "Camera_traps_not_cleaned.jpeg"), dpi = 100)
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
ggsave(p5, filename = file.path(plot_path, "Camera_traps_cleaned.jpeg"), dpi = 100)
# save(camera_trap_dogs_clean, file = file.path(Camera_folder, "Camera_dogs.RData"))

