# ==========================================================================
# SETUP: packages, paths, boundary, covariates, and CRS definitions
# ==========================================================================

path <- "C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats"
setwd(path)

data_path  <- file.path(path, "Data")
utils_path <- file.path(path, "Utils")
plot_path  <- file.path(path, "Models/Plot_PA")
Database_Folder  <- file.path(data_path, "Database_Dogs&Cats")
Conteos_folder   <- file.path(data_path, "Conteos")

source(file.path(utils_path, "Packages.R"))

# Study area boundary (already an sf object)
load(file.path(data_path, "Boundary_sf.RData"))

# Covariate raster stack for Portugal (used later in the IDM)
cov_por <- rast(file.path(data_path, "Conteos/covs_updated_new.tif"))

# CRS definitions:
# - crs: standard EPSG:3035 (meters), used for raw spatial joins/filters
# - proj_string: LAEA projection with units in km (for reference / potential
#   use with terra::project if needed)
# - crs_km: fmesher-compatible EPSG:3035 with length unit set to km, used
#   throughout for mesh-based (inlabru) modeling
crs <- "EPSG:3035"
proj_string <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=km +no_defs"
crs_km <- fm_crs_set_lengthunit(fm_crs("EPSG:3035"), "km")

# Reproject boundary into the km-unit CRS used for modeling
boundary_sf <- fm_transform(boundary_sf, crs_km)


# ==========================================================================
# COUNT DATA 
# ==========================================================================

count_data <- read_xlsx(path = file.path(data_path, "Quadriculas_transectos_dogs.xlsx"))

# Survey grid (quadriculas) used to spatially join transect counts
quadriculas_por <- read_sf(file.path(Conteos_folder, "Quadriculas_transectos_Portugal.shp")) |>
  st_as_sf() |>
  st_transform(crs) |>
  mutate(area_quadriculas = st_area(geometry))

# count_data |>
#   filter(Year == 2022) -> count_data_2022

# Join transect count records to their grid cell geometry via the shared
# "layer" key, then reproject to the standard meter-based CRS
# count_dogs <- count_data |>
#   distinct(id, .keep_all = TRUE) |>
#   left_join(
#     quadriculas_por |> select(layer, geometry),
#     by = "layer"
#   ) |>
#   st_as_sf() |>
#   st_transform(crs)

count_dogs <- count_data |>
  left_join(
    quadriculas_por |> select(layer, geometry),
    by = "layer"
  ) |>
  st_as_sf() |>
  st_transform(crs)

# Survey effort = road length surveyed, converted from meters to km
count_dogs$effort_walked <- count_dogs$Length_roads_m / 1000
#count_dogs$effort_time <- as.numeric(count_dogs$Effort_minutes)/60

count_dogs[count_dogs$id == 169, ]
# Collapse each transect's geometry to its centroid 
count_dogs <- st_centroid(count_dogs)
table(count_dogs$id)
length(unique(count_dogs$id)) # 78 as the sampled cells


# ==========================================================================
# COLLISION DATA
# ==========================================================================

collision_dogs <- read_xlsx(path = file.path(data_path, "Coordenadas_Dogs_points.xlsx"))

# Parse dates (day-month-year format) and extract survey year
collision_dogs$Data <- dmy(collision_dogs$Data)
collision_dogs$year <- year(collision_dogs$Data)

# Convert to sf: raw coordinates are in WGS84 (EPSG:4326), then reproject
# to the standard meter-based CRS. Drop exact-duplicate locations.
collision_dogs |>
  select(c(year, X, Y)) |>
  st_as_sf(coords = c("X", "Y")) |>
  st_set_crs(4326) |>
  st_transform(crs) |>
  distinct(geometry, .keep_all = TRUE) -> collision_sf

collision_dogs <- collision_sf

# Reproject into the km-unit modeling CRS, drop duplicates again after
# reprojection (to be safe), and restrict to points falling within the
# study area boundary
collision_sf |>
  fm_transform(crs_km$wkt) |>
  distinct(geometry, .keep_all = TRUE) |>
  st_filter(boundary_sf, .predicate = st_within) -> collisions_sf


# ==========================================================================
# CAMERA TRAP DATA — biased presence-absence source
# ==========================================================================

load(file.path(data_path, "Camera Trap/PA.RData"))

# --- Presence sites -------------------------------------------------------
# As a first step we only consider points with Npres == 1, i.e. sites that
# were confirmed present at least once. Within each (year, point) group we
# take the first record and flag it as Presence = 1. We decided to treat a
# site as "present" for a given year if it was present at any point during
# that year (overall presence), restricted to survey years 2019-2023.
presence_sites <- presence_absence_dogs %>%
  filter(Npres == 1) %>%
  mutate(years = year(Timestamp)) %>%
  group_by(years, POINT_STANDARD) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(Presence = 1) %>%
  filter(years %in% c(2019:2023))

# --- Absence sites ----------------------------------------------------------
# Sites with Npres == 0 are absences; year is taken from the installation
# date instead of the (non-existent) detection timestamp.
absence_sites <- presence_absence_dogs %>%
  filter(Npres == 0) %>%
  mutate(Presence = 0, years = year(INSTALLATION)) %>%
  group_by(years, POINT_STANDARD) %>%
  slice(1) %>%
  ungroup()

# table(absence_sites$Presence)  # sanity check (all zeros expected)

# Combine presence and absence sites into a single PA dataset
pa_final <- rbind(presence_sites, absence_sites)
table(pa_final$Presence, pa_final$years)

# Reproject to km-unit CRS. When both an absence and a presence record
# exist at the same (year, geometry), prioritize keeping the presence
# (Npres = 1) record via the arrange() + distinct() combination.
pa_final <- pa_final |>
  fm_transform(crs_km$wkt) |>
  arrange(years, geometry, desc(Npres)) |>   # priority to Npres = 1
  distinct(years, geometry, .keep_all = TRUE)

table(pa_final$years)
table(collision_dogs$year)

# --- Subset both biased sources to the common reference year 2022 ---------
pa_2022 <- pa_final |>
  filter(years == 2022) |>
  distinct(geometry, .keep_all = TRUE)

collisions_2022 <- collision_sf |>
  filter(year == 2022) |>
  distinct(geometry, .keep_all = TRUE)
#save(pa_2022, collisions_2022, count_dogs, file = file.path(data_path, "Model_Data.RData"))
