## =============================================================================
## Simulation study: integrating biased presence-only (PO), biased
## presence-absence (PA) and unbiased count data to estimate free-roaming
## dog relative abundance.
##
## Generative design (matches Section 4 of the manuscript):
##   log(lambda(s)) = beta0 + beta1 * x(s) + w(s)
## where x(s) is an environmental covariate and w(s) is a mean-zero Gaussian
## random field (Matern covariance, sigma = 1, range = 100) on a 600x600
## square study area. Three independent observation mechanisms are applied
## to (independent) realizations of the same intensity surface:
##   - Presence-only (PO):    thinned via logit(p(s)) = alpha0 + alpha1*z(s)
##   - Presence-absence (PA): preferentially sampled 10x10-unit cells
##   - Count:                 randomly sampled 25x25-unit blocks (unbiased anchor)
##
## Design rationale:
## Unlike the PA design used in prior work (e.g. Simmonds et al. 2020), here we
## need a binary/preferential covariate to multiply into the field so that we
## simulate genuine sampling bias in the presence-absence source (rather than
## assuming an unbiased PA source, as most existing IDM papers do).
##
## For the presence-only source, the bias is instead represented as a second
## covariate that thins a realization of the point process (rather than
## governing which grid cells get surveyed, as in the PA case).
## =============================================================================





###### load useful packages####
source("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Utils/Packages.R")
set.seed(1234)

win <- owin(c(0,600), c(0,600))   # 600 x 600 study area
npix <- 2000                      # raster resolution for computing fields

Domain <- rast(nrows=npix, ncols=npix,
               xmax=win$xrange[2], xmin=win$xrange[1],
               ymax = win$yrange[2], ymin=win$yrange[1])

values(Domain) <- 1:ncell(Domain)
xy <- crds(Domain)   # pixel centroid coordinates, used throughout to project fields

# --- Fine grid used for the PA design ------------------------------------
# cell_size = 10 means cells of dimension 10x10 spatial units (600/10 = 60
# cells per side => 3600 cells total).

cell_size = 10
customGrid <- st_make_grid(Domain,cellsize = c(cell_size,cell_size)) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(cellid = row_number())

ncells <- nrow(customGrid)   # total number of fine (PA) cells = 3600
# --- Coarse grid used for the Count design --------------------------------
# block_size = 25: cells of dimension 25x25 units.
block_size <- 25
countGrid <- st_make_grid(Domain, cellsize = c(block_size, block_size)) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(blockid = row_number())


# Spatial boundary polygon.
boundary_sf = st_bbox(c(xmin = 0, xmax = 600, ymax = 0, ymin = 600)) |>
  st_as_sfc()

boundary_coords <- st_coordinates(boundary_sf)[, 1:2]

# --- SPDE mesh and precision matrices for the latent Gaussian fields ------
mesh_sim <- fm_mesh_2d(
  loc.domain = boundary_coords,
  max.edge = c(10, 50),
  offset = c(50, 120)
)

matern_sim <- inla.spde2.pcmatern(mesh_sim,
                                  prior.range = c(100, 0.5),
                                  prior.sigma = c(1, 0.5))

range_spde = 100   # Matern range for the TRUE latent field w(s)
sigma_spde = 1     # Matern marginal sd for w(s)

# Precision matrices at three different (range, sigma) settings:
#   Q1 -> the TRUE latent field w(s) entering log(lambda(s))
#   Q2 -> used only to build the (uncorrelated) PO sampling-bias surface "s"
#   Q3 -> environmental covariate x(s), AND (re-used) for the PA sampling
#         surface construction below.
Q1 = inla.spde.precision(matern_sim, theta = c(log(range_spde),
                                               log(sigma_spde)))
Q2 = inla.spde.precision(matern_sim, theta = c(log(range_spde-20),
                                               log(sigma_spde - 0.3)))
Q3 = inla.spde.precision(matern_sim, theta = c(log(range_spde-30),
                                               log(sigma_spde -0.3)))

# --- Draw the Gaussian random fields --------------------------------------
# sim_field_true has 3 columns: independent draws from the SAME GMRF (Q1).
# Column 1 -> omega_s (the true spatial random effect in log-lambda)
# Column 2 -> delta_s_PA (used below to build the PA surface)

sim_field_true  = inla.qsample(n = 3, Q = Q1, seed = 123)
sim_field_SB_PO = inla.qsample(n = 1, Q = Q2, seed = 231)   # PO bias field (uncorrelated variant)
# sim_field2_SB_PA = inla.qsample(n = 1, Q = Q3, seed = seed)  # unused/leftover
sim_field3_X    = inla.qsample(n = 1, Q = Q3, seed = 1234)  # environmental covariate x(s)

# sampling_bias_function = function(x,y) Original uncorrelated thinning mechanism
# {
#   alpha = -0.5
#   beta = 1/max(x)
#   res <- alpha + beta * x
#   return(res)
# }

# Covariate z(s) constructed to have a correlation  with x_s.
# This is the mechanism described in the manuscript for the PO thinning probability. 


sampling_bias_function_correlated <- function(x_s, rho) {
  x_std <- scale(x_s)[,1]
  delta_std <- scale(delta_s_PO)[,1]
  
  sampling_bias <- rho * x_std +
    sqrt(1 - rho^2) * delta_std
  
}


# --- Project the mesh-level fields onto the raster pixel locations --------
A_proj = inla.spde.make.A(mesh_sim, loc = xy)

omega_s        = (A_proj %*% sim_field_true)[,1]     # true spatial random effect w(s)
x_s            = (A_proj %*% sim_field3_X)[,1]        # environmental covariate x(s)
delta_s_PO     = (A_proj %*% sim_field_SB_PO)[,1]/3   # PO bias field (scaled down by /3)
delta_s_PA = (A_proj %*% sim_field_true)[,2]      # independent noise source feeding the PA accessibility surface below 

sampling_bias <- data.frame(x = xy[,1], y = xy[,2]) %>%
  mutate(s_cor = sampling_bias_function_correlated(x_s, rho = 0.5))  # correlated PO bias (-> det2, matches text mechanism)


cor(x_s, sampling_bias$s_cor) # 0.41 


# Convert fields to SpatRasters

x_rast_PO  = rast(data.frame(x = xy[,1], y = xy[,2], x_s))
omega_rast = rast(data.frame(x = xy[,1], y = xy[,2], omega_s))
g_rast     = rast(data.frame(x = xy[,1], y = xy[,2], sampling_bias$s))
g_rast_cor = rast(data.frame(x = xy[,1], y = xy[,2], sampling_bias$s_cor))
x_rast_PA  = rast(data.frame(x = xy[,1], y = xy[,2], delta_s_PA))

# Simulate the shared point process ----------------------------------------

n_data <- 100   # number of independent replicate datasets

### Focal species intensity: log(lambda(s)) = beta0 + beta1 * x(s) + w(s)
beta_f <- c(-5, 0.5)   
lambda_f <- exp(beta_f[1] + beta_f[2]*x_s + omega_s)

lambda_im <- data.frame(z = lambda_f,
                        x = xy[,1],
                        y = xy[,2]) %>% as.im()  

lambda_im_plot <- data.frame(z = lambda_f,
                             x = xy[,1],
                             y = xy[,2],
                             omega = omega_s,
                             delta_s_PO = delta_s_PO,
                             delta_s_PA = delta_s_PA,
                             x_cov = x_s) %>% as.im()   # extra layers kept for plotting

# Simulate n_data*3 = 300 INDEPENDENT Poisson process realizations from the
# same intensity surface lambda_f. The first 100 feed the PO source, the
# next 100 feed the PA source, and the last 100 feed the Count source 

pp_f = lambda_im %>% rpoispp(nsim = n_data*3)

pp_f_lst <- list()
for(j in 1:(n_data*3)){
  pp_f_lst[[j]] <- data.frame(x = pp_f[[j]]$x, y = pp_f[[j]]$y, sim = j)
}
pp_set_0 <- do.call("rbind", pp_f_lst)

 
# Expected abundance
mean(lambda_f) * st_area(boundary_sf)   # expected total abundance over the study area
mean(lambda_f) * cell_size**2           # expected count per fine (PA) cell
mean(lambda_f)* 25**2                   # expected count per more fine (count) cell
# Presence only data ----------------------------------------------------------

# Presence-only data ---------------------------------------------------------
# sim 1:100 -> the PO realizations
po_sf = pp_set_0[pp_set_0$sim %in% c(1:100), ] %>% st_as_sf(coords = c("x","y"))
alpha    = c(-1, 1.3)     # coefficients for the "correlated" PO thinning


ps_rast <- data.frame(x = xy[,1],
                      y = xy[,2],
                      p_s2 = plogis(alpha[1]  + alpha[2]*sampling_bias$s_cor)) %>%
  rast()

n <- nrow(po_sf)
po_sf_new = po_sf %>%
  mutate(p_s2 = extract(ps_rast, .)$p_s2,
         det2 = rbinom(n = n, size = 1, p_s2))   # thinning indicator 



# Sampling cells (for PA design) --------------------------------------------

grids_centroid <- suppressWarnings(customGrid %>% st_centroid()) %>% st_coordinates()

n_samp <- round(ncells * .10)   # 10% of fine cells surveyed for PA -> 360 cells 

# PA sampling-probability surface, built from
# delta_s_PA (an independent draw from the same GMRF model as omega_s
# Only the top 15% of cells (by delta_s_PA) get nonzero sampling probability,
# proportional to exp(delta_s_PA) among those cells.
BS = delta_s_PA
BS[BS < quantile(BS, probs = c(0.85))] = 0
BS[BS >= quantile(BS, probs = c(0.85))] =
  exp(BS[BS >= quantile(BS, probs = c(0.85))]) / sum(exp(BS[BS >= quantile(BS, probs = c(0.85))]))

bs_rast = rast(data.frame(x = xy[,1], y = xy[,2], BS))
delta_s_PA_rast = rast(data.frame(x = xy[,1], y = xy[,2], delta_s_PA))

# Cell-level mean sampling probability 
bs_p <- terra::extract(bs_rast, terra::vect(customGrid), fun = mean)[,2]

Biased_Sampling_10cellID <- matrix(NA, nrow = ncells, ncol = n_data)
sum(bs_p > 0)   #number of cells with nonzero PA sampling probability
for(j in 1:n_data){
  bs_i10 <- sample(1:ncells, size = n_samp, replace = FALSE, prob = bs_p)
  Biased_Sampling_10cellID[,j] <- 1:ncells %in% bs_i10
}

# True presence-absence records at surveyed cells ---------------------------

True_Pres_lst <- list()
for(j in (n_data + 1):(2*n_data)){
  
  # True presence/absence of the focal-species point pattern in every fine cell
  pres <- ifelse(pp_set_0 %>% filter(sim==j) %>% select(c(x,y)) %>%
                   st_as_sf(coords = c("x", "y")) %>%
                   st_intersects(x = customGrid) %>%
                   lengths() > 0, 1, 0)
  
  # Subset to the cells actually surveyed under the biased design
  True_Pres_lst[[j]] <- data.frame(pres = pres[Biased_Sampling_10cellID[, j - n_data]]) %>%
    bind_cols(grids_centroid[Biased_Sampling_10cellID[, j - n_data], ],
              sim = j - n_data,
              scheme = "BS")
}
True_Pres_all <- do.call("rbind", True_Pres_lst)


# Count data design ----------------------------------------------------------

nblocks <- nrow(countGrid)
n_samp_count <- 25   

# Randomly select 25 coarse blocks per replicate, conditional on NOT
# overlapping any fine cell already selected for the PA design in that same
# replicate
SRS_Count_BlockID <- matrix(FALSE, nrow = nblocks, ncol = n_data)
for(j in 1:n_data){
  sampled_pa_cells <- customGrid[Biased_Sampling_10cellID[, j], ]
  overlaps <- lengths(st_intersects(countGrid, sampled_pa_cells))
  candidate_blocks <- which(overlaps == 0)
  srs_count <- sample(candidate_blocks, size = n_samp_count, replace = FALSE)
  SRS_Count_BlockID[srs_count, j] <- TRUE
}


# True point counts at surveyed blocks
Count_Data_lst <- list()
for(j in (2*n_data + 1):(3*n_data)){
  sim_idx <- j - 2*n_data
  
  sim_pts <- pp_set_0 %>%
    filter(sim == j) %>%
    st_as_sf(coords = c("x", "y"), crs = st_crs(countGrid))
  
  counts_all_blocks <- lengths(st_intersects(countGrid, sim_pts))
  sampled_blocks_idx <- which(SRS_Count_BlockID[, sim_idx])
  
  sampled_blocks_sf <- countGrid[sampled_blocks_idx, ] %>%
    mutate(
      count = counts_all_blocks[sampled_blocks_idx],
      sim = sim_idx,
      scheme = "SRS",
      block_area = as.numeric(st_area(geometry))   # exposure term for the Poisson count model
    )
  
  Count_Data_lst[[sim_idx]] <- sampled_blocks_sf
}
Count_Data_all <- do.call("rbind", Count_Data_lst)

# save(boundary_sf, sim_field_true, sim_field3_X, pp_set_0, po_sf, po_sf_new, Biased_Sampling_10cellID,
#      True_Pres_all, customGrid, xy, x_s, delta_s_PA, delta_s_PO, sampling_bias, lambda_f, omega_s,Count_Data_all, file = "Sim_Def.RData")
# load("Sim_Def.RData")



# Generate a PDF report -----------------
# This is not inclued in the pubblication but it is freely available on GitHub

load("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Sim/Sim_Def.RData")
alpha    = c(-1, 1.3)     # coefficients for the "correlated" PO thinning
ps_rast <- data.frame(x = xy[,1],
                      y = xy[,2],
                      p_s2 = plogis(alpha[1]  + alpha[2]*sampling_bias$s_cor)) %>%
  rast()
BS = delta_s_PA
BS[BS < quantile(BS, probs = c(0.85))] = 0
BS[BS >= quantile(BS, probs = c(0.85))] =
  exp(BS[BS >= quantile(BS, probs = c(0.85))]) / sum(exp(BS[BS >= quantile(BS, probs = c(0.85))]))

bs_rast = rast(data.frame(x = xy[,1], y = xy[,2], BS))
delta_s_PA_rast = rast(data.frame(x = xy[,1], y = xy[,2], delta_s_PA))
x_rast_both = rast(data.frame(x = xy[,1], y = xy[,2], x_s))
x_rast_PA   = rast(data.frame(x = xy[,1], y = xy[,2], delta_s_PA))
x_rast_PO   = rast(data.frame(x = xy[,1], y = xy[,2], sampling_bias$s_cor))
omega_rast  = rast(data.frame(x = xy[,1], y = xy[,2], omega_s))
log_lambda  = rast(data.frame(x = xy[,1], y = xy[,2], lambda_f))
n_data <- 100
win <- owin(c(0,600), c(0,600))   # 600 x 600 study area
npix <- 2000                      # raster resolution for computing fields

Domain <- rast(nrows=npix, ncols=npix,
               xmax=win$xrange[2], xmin=win$xrange[1],
               ymax = win$yrange[2], ymin=win$yrange[1])

values(Domain) <- 1:ncell(Domain)
block_size <- 25
countGrid <- st_make_grid(Domain, cellsize = c(block_size, block_size)) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(blockid = row_number())

pdf(file = "Simulation_Intensity.pdf", width = 11, height = 8.5)

# ---------------------------------------------------------
# Section 1: Baseline rasters (true intensity, covariates, bias surfaces)
# ---------------------------------------------------------
raster_list <- list(
  "LOG-lambda"                    = log_lambda,      # UNDEFINED -- see note above
  "PA Covariate proxy"            = x_rast_PA,
  "PO Covariate proxy"            = x_rast_PO,     # UNDEFINED -- see note above
  "PO Sampling"                   = ps_rast$p_s2,
  "Spatial Latent Effect (Omega)" = omega_rast,
  "Combined Effects"              = x_rast_both,
  "Sampling PA Surface"           = bs_rast
)

cat("Rendering baseline rasters...\n")
for (name in names(raster_list)) {
  p_rast <- ggplot() +
    geom_spatraster(data = raster_list[[name]]) +
    scale_fill_viridis_c(option = "viridis", na.value = "transparent") +
    labs(title = paste("Baseline Field:", name), fill = "Value") +
    theme_minimal(base_family = "serif") +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      panel.grid = element_line(color = "grey90")
    )
  print(p_rast)
}
dev.off()

# ---------------------------------------------------------
# Section 2: Per-replicate diagnostic pages (1 to 100)
# ---------------------------------------------------------
cat("Rendering simulation pages...\n")
pdf(file = "Simulation_Process.pdf", width = 11, height = 8.5)
for (i in 1:100) {
  
  # --- Plot A: PO detection/observation map for replicate i ---
  sim_po_data <- po_sf_new[po_sf_new$sim == i, ]
  
  p_detection <- ggplot() +
    geom_sf(data = boundary_sf, fill = "grey98", color = "black", linewidth = 0.6) +
    geom_sf(data = sim_po_data, aes(col = as.factor(det2)), size = 1.5, alpha = 0.8) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = paste("Simulation", i, "- Presence-Only Detection State"),
      color = "Detected (det1)"
    ) +
    theme_minimal(base_family = "serif") +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    )

  print(p_detection)
  
  # --- Plot B: PA biased sampling grid & truth point pattern for replicate i ---
  customGrid$tst <- Biased_Sampling_10cellID[, i]
  
  sim_pp_set <- pp_set_0 %>%
    filter(sim == (i + n_data)) %>%
    select(x, y) %>%
    st_as_sf(coords = c("x", "y"), crs = st_crs(customGrid))
  
  sim_true_pres <- True_Pres_all %>%
    filter(sim == i & scheme == "BS") %>%
    st_as_sf(coords = c("X", "Y"), crs = st_crs(customGrid))
  
  p_grid <- ggplot() +
    geom_sf(data = customGrid, aes(fill = factor(tst)), color = "white", linewidth = 0.1, alpha = 0.6) +
    scale_fill_viridis_d(option = "mako", name = "Sampling Bias Level") +
    geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.7) +
    geom_sf(data = sim_true_pres, aes(color = factor(pres)), size = 2, alpha = 0.8) +
    scale_color_manual(values = c("0" = "#ee6c4d", "1" = "#3d5a80"), name = "True Presence Status") +
    geom_sf(data = sim_pp_set, color = "purple", shape = 17, size = 2.5) +
    labs(
      title = paste("Simulation", i, "- Biased Sampling Scheme & Process State"),
      subtitle = "Purple triangles represent sampling locations"
    ) +
    theme_minimal(base_family = "serif") +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, face = "italic", color = "grey30"),
      legend.position = "right"
    )
  print(p_grid)
  
  # --- Plot C: Count design & counts for replicate i ---
  countGrid$tst <- as.factor(countGrid$blockid %in% Count_Data_all$blockid[Count_Data_all$sim == i])
  p_count <- ggplot()+
    geom_sf(data=countGrid,aes(fill=factor(tst)))+
    geom_sf(data= pp_set_0 %>% filter(sim==i + 2*n_data) %>% select(c(x,y)) %>%
              st_as_sf(coords = c("x", "y")), color="purple", shape=17)+
    geom_sf(data= Count_Data_all %>%
              filter(sim==i & scheme=="BS") %>%
              st_as_sf(coords = c("X", "Y")), aes(color=factor(count)))+
    scale_color_viridis(discrete = T, na.value = "grey50")
  
  print(p_count)
  
}

dev.off()
cat("Finished! PDF saved safely as 'Simulation_Spatial_Plots.pdf'\n")