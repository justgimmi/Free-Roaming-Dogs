path <- "C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats"
setwd(path)

data_path  <- file.path(path, "Data")
utils_path <- file.path(path, "Utils")
plot_path  <- file.path(path, "Figures/Figure Fit")
Database_Folder  <- file.path(data_path, "Database_Dogs&Cats")
Conteos_folder   <- file.path(data_path, "Conteos")

source(file.path(utils_path, "Packages.R"))

# Study area boundary (already an sf object)
load(file.path(data_path, "Boundary_sf.RData"))

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
outline <-  st_simplify(st_as_sf(boundary_sf), dTolerance = 3)
mesh <- fm_mesh_2d(
  boundary = list(outline),
  max.edge = c(5, 25),
  cutoff = 5,
  offset = c(10, 20),
  crs = fm_crs(outline)
)



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
hfp <-  focal(hfp, w=5, fun="mean", 
              expand = TRUE, na.rm = T)


forest <- scale(cov_scaled$forest)
forest[is.na(forest)] <- 0
forest <-  focal(forest, w=5, fun="mean", 
                 expand = TRUE, na.rm = T)

agfor <- scale(cov_scaled$agfor)
agfor[is.na(agfor)] <- 0
agfor <-  focal(agfor, w=5, fun="mean", 
                expand = TRUE, na.rm = T)

heter <- scale(cov_scaled$heter)
heter[is.na(heter)] <- 0
heter <-  focal(heter, w=5, fun="mean", 
                expand = TRUE, na.rm = T)

mix <- scale(cov_scaled$mix)
mix[is.na(mix)] <- 0
mix <-  focal(mix, w=5, fun="mean", 
              expand = TRUE, na.rm = T)



Density <- scale(cov_scaled$density_km)
Density[is.na(Density)] <- 0
Density <-  focal(Density, w=5, fun="mean", 
                  expand = TRUE, na.rm = T)
####### Log-Intensity Plot #####
load(file = file.path(path, "Models/fit.RData"))
ppxl_out <- fm_pixels(mesh, mask = boundary_sf, format = "sf")
lambda_out <- predict(
  fit,
  ppxl_out,
  ~ data.frame(
    lambda_true =  beta0_count + u  + mix_cov + agfor_cov + hfp_cov ,
    field = u
  )
)

log_int <- ggplot(lambda_out$lambda_true) +
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
  
  labs(title = expression(log(widehat(lambda)(s) )))+
  annotation_scale(location = "bl", width_hint = 0.2) +
  theme_pub_fit
ggsave(file.path(plot_path, "Intensity.png"), log_int, width = 5, height = 7.5, dpi = 600)

###### Covariates Plot ###### 
## ---------------------------------------------------------------
## 1. Pull the fixed-effects summary out of the bru() fit
## ---------------------------------------------------------------
fx <- fit$summary.fixed |>
  as.data.frame() |>
  tibble::rownames_to_column("term") |>
  rename(
    lower = `0.025quant`,
    mean  = mean,
    upper = `0.975quant`
  ) |>
  select(term, mean, lower, upper)

## ---------------------------------------------------------------
## 2. Ordering: shared covariates first, then PO, PA, Count
##    correction terms (edit these vectors to match your cmp)
## ---------------------------------------------------------------
shared_terms <- c("hfp_cov", "agfor_cov", "mix_cov")
po_terms     <- c("beta0_po", "beta0_thin", "beta1_thin")
pa_terms     <- c("beta0_pa", "forest_cov")
count_terms  <- c("beta0_count")

term_order <- c(shared_terms, po_terms, pa_terms, count_terms)
term_order <- term_order[term_order %in% fx$term]   # drop any that aren't fixed effects

## number of terms in each block, in the same order, for spacer placement
block_sizes <- c(
  sum(shared_terms %in% term_order),
  sum(po_terms     %in% term_order),
  sum(pa_terms     %in% term_order),
  sum(count_terms  %in% term_order)
)
block_sizes <- block_sizes[block_sizes > 0]

## ---------------------------------------------------------------
## 3. Plotmath labels — edit to taste
## ---------------------------------------------------------------
term_labels <- c(
  hfp_cov      = "HFP",
  agfor_cov    = "Agriculture~and~forest",
  mix_cov      = "Mixed~cover",
  beta0_po     = "beta[0]^{PO}",
  beta0_thin   = "beta[0]^{thin}",
  beta1_thin   = "Road~Density",
  beta0_pa     = "beta[0]^{PA}",
  forest_cov   = "Forest",
  beta0_count  = "beta[0]^{count}"
)



label_fun <- function(x) {
  lab <- ifelse(x %in% names(term_labels), term_labels[x], "\" \"")
  do.call(c, lapply(lab, function(l) parse(text = l)))
}

## ---------------------------------------------------------------
## 5. Publication-style forest plot
## ---------------------------------------------------------------
p_forest <- ggplot(fx, aes(x = mean, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_pointrange(
    aes(xmin = lower, xmax = upper),
    size = 0.45, linewidth = 0.7, fatten = 2.5,
    colour = "black", na.rm = TRUE
  ) +
  scale_y_discrete(labels = label_fun , drop = FALSE) +
  theme_pub_fit +
  labs(
    title = "95% credible interval",
    x = "Values",
    y = "Coefficients"
  )

p_forest

ggsave(file.path(plot_path, "Forest_Plot.png"), p_forest, width = 7, height = 7.5, dpi = 600)




################ Excursion sets ###############à
load(file = file.path(path, "Models/fit.RData"))

# single, consistent prediction grid over the study area
pred_pix <- fm_pixels(mesh, dims = c(300, 300), mask = boundary_sf, format = "sf")
# quantity of interest: log-density surface underlying the count model
# (drop the log(effort) offset -- that's a survey-effort term, not part of
# the intrinsic density we want to threshold)
pred_formula <- ~ beta0_count + u + hfp_cov + mix_cov + agfor_cov

# posterior mean surface, used only to set a data-driven threshold
pred_mean <- predict(fit, pred_pix, pred_formula, probs = 0.90)

# threshold: 90th percentile of the fitted intensity surface across the
# study area (relative-abundance threshold, consistent with the text)
u_thresh <- mean(pred_mean$q0.9)

# posterior samples of the same linear predictor, for the excursion set
samps <- generate(fit, newdata = pred_pix, formula = pred_formula, n.samples = 2000)

exc <- excursions.mc(
  samps,
  alpha = 0.05,       # 95% simultaneous confidence
  u     = u_thresh,   # threshold on the predictor's own scale (log-density)
  type  = ">"         # positive excursion: significantly *above* threshold
)

pred_pix$F <- exc$F   # excursion function: confidence level of exceedance at s
pred_pix$E <- exc$E   # -1/0/1 excursion set indicator (1 = confidently above u)

# --- plot ---------------------------------------------------------------

theme_pub <- theme_minimal(base_size = 15) +
  theme(
    plot.title      = element_text(face = "bold", size = 18, hjust = 0.5,
                                   margin = margin(b = 6)),
    axis.title      = element_text(size = 15),
    axis.text       = element_text(size = 9, colour = "black"),
    legend.title    = element_text(size = 13),
    legend.text     = element_text(size = 10),
    legend.key.width  = unit(0.35, "cm"),
    legend.key.height = unit(0.9, "cm"),
    plot.margin     = margin(5, 8, 5, 5)
  )



plotfun <- ggplot(pred_pix) +
  geom_sf(aes(color = F), size = 0.6) +
  scale_color_viridis_c(
    option = "inferno",
    name   = "Excursion\nconfidence",
    limits = c(0, 1)
  ) +
  geom_sf(
    data  = st_boundary(boundary_sf),
    color = "grey20", linewidth = 0.4, inherit.aes = FALSE
  ) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  labs(title = expression(F[u]^"+" * (s)))+
  theme_pub
plotfun
ggsave(file.path(plot_path, "Excursus_Function.png"), plotfun, width = 8, height = 7.5, dpi = 600)
