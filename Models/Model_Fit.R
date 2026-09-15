# ==========================================================================
# SETUP: packages, paths, boundary, covariates, CRS definitions and Import Data
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




# ==========================================================================
# Covariates Smoothing for Inference purposes
# ==========================================================================
cov_por <- rast(file.path(data_path, "Conteos/covs_updated_new.tif"))
cov_ext <- extend(
  cov_por,
  ext(cov_por) + 40000)
cov_ext <- terra::project(cov_ext, crs_km$wkt)

cov_scaled <- cov_ext

hfp <- scale(cov_scaled$hfp)
hfp[is.na(hfp)] <- 0
hfp <-  focal(hfp, w=3, fun="mean", 
              expand = TRUE, na.rm = T)


forest <- scale(cov_scaled$forest)
forest[is.na(forest)] <- 0
forest <-  focal(forest, w=3, fun="mean", 
                 expand = TRUE, na.rm = T)

agfor <- scale(cov_scaled$agfor)
agfor[is.na(agfor)] <- 0
agfor <-  focal(agfor, w=3, fun="mean", 
                expand = TRUE, na.rm = T)

heter <- scale(cov_scaled$heter)
heter[is.na(heter)] <- 0
heter <-  focal(heter, w=3, fun="mean", 
                expand = TRUE, na.rm = T)

mix <- scale(cov_scaled$mix)
mix[is.na(mix)] <- 0
mix <-  focal(mix, w=3, fun="mean", 
              expand = TRUE, na.rm = T)



Density <- scale(cov_scaled$density_km)
Density[is.na(Density)] <- 0
Density <-  focal(Density, w=3, fun="mean", 
                  expand = TRUE, na.rm = T)

wood <- scale(cov_scaled$wood)
wood[is.na(wood)] <- 0
wood <-  focal(wood, w=3, fun="mean", 
                  expand = TRUE, na.rm = T)
plot(wood)
plot(agfor)
plot(forest)
plot(heter)
# ==========================================================================
# Model Fitting for 2022
# ==========================================================================

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
  prior.range = c(100, 0.5),
  prior.sigma = c(1, 0.05)
)
# dimension of portugal are more less: 561 x 218

cmp <-  ~ 
  beta0_po(1, model = "linear", prec.linear = 0.01) +
  beta0_count(1, model = "linear", prec.linear = 0.01) +
  beta0_pa(1, model = "linear", prec.linear = 0.01) +
  beta0_thin(1, model ="linear", prec.linear = 0.01) +
  beta1_thin(Density, model  = "linear", prec.linear = 0.001) + 
  u(geometry, model = spde_co) + 
  hfp_cov(hfp, model = "linear") +  
  forest_cov(forest, model = "linear") +  
  agfor_cov(agfor, model = "linear") +
  heter_cov(heter, model = "linear") +
  mix_cov(mix, model = "linear") + 
  Density_cov(Density, model = "linear") + 
  wood_cov(wood, model = "linear")



bru_options_set(
  bru_verbose = TRUE,
  verbose = TRUE,
  bru_max_iter = 100,
  control.inla = list(int.strategy = "eb")
)

log_detect = function(p1, cov) {
  ldet = log(plogis(p1 + cov))
  return(ldet)
}

st_geometry(outline) <- "geometry"
lik_po <- bru_obs(
  formula = geometry ~ beta0_po + u + hfp_cov + agfor_cov+  mix_cov   + log_detect(beta0_thin, beta1_thin)  ,
  family = "cp",
  data = collisions_2022,
  domain = list(geometry = mesh),
  samplers = outline
)

lik_int <- bru_obs(
  formula = Presence ~ beta0_pa  + u  +forest_cov+ mix_cov + agfor_cov + hfp_cov + log(.data.$EFFORT) ,
  family = "binomial",
  data = pa_2022,
  domain = list(geometry = mesh),
  samplers = outline,
  control.family = list(link = "cloglog")
)

lik_count <- bru_obs(
  formula =
    observation_dogs ~ beta0_count +
    u  + hfp_cov+
    mix_cov +
    agfor_cov  +  log(.data$effort_walked) + log(.data$area) ,
  family = "poisson",
  data = count_dogs,
  domain = list(geometry = mesh)
)

fit <- bru(
  cmp, lik_int, lik_po,lik_count,options = list(verbose = TRUE, bru_verbose = 4, safe = TRUE,bru_max_iter = 100)
)

# save(fit, file = file.path(path, "Models/fit.RData"))
load(file = file.path(path, "Models/fit.RData"))
summary(fit)
Lambda_elev_field <- predict(
  fit,
  fmesher::fm_int(mesh, boundary_sf),
  ~ sum(weight * exp(beta0_count + u  + mix_cov + agfor_cov + hfp_cov ))
) # estimate of the relative abundance
ppxl_out <- fm_pixels(mesh, mask = boundary_sf, format = "sf")
lambda_out <- predict(
  fit,
  ppxl_out,
  ~ data.frame(
    lambda_true =  beta0_count + u  + mix_cov + agfor_cov + hfp_cov ,
    field = u
  )
)


