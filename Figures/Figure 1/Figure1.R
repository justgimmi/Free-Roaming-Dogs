###### load useful packages to plot and the useful objects ####
source("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Utils/Packages.R")
library(tidyterra)
library(ggplot2)
library(patchwork)   # for combining panels into one figure
library(scales)      # for nicer legend labels

load("Sim_Def.RData") # load all the simulated data and environmental covariates

# ---------------------------------------------------------------------------
# 0. Shared publication theme
# ---------------------------------------------------------------------------
# One theme applied to every panel so the figure set reads as a single,
# coherent figure (equal font sizes, no gridlines competing with the raster,
# consistent margins/legend placement).
theme_pub <- theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 13, hjust = 0.5,
                                   margin = margin(b = 6)),
    axis.title      = element_text(size = 11),
    axis.text       = element_text(size = 9, colour = "black"),
    legend.title    = element_text(size = 10),
    legend.text     = element_text(size = 9),
    legend.key.width  = unit(0.35, "cm"),
    legend.key.height = unit(0.9, "cm"),
    panel.grid      = element_blank(),
    panel.border    = element_rect(colour = "black", fill = NA, linewidth = 0.4),
    plot.margin     = margin(5, 8, 5, 5)
  )

# common x/y labels — swap for "Longitude"/"Latitude" if xy are lon/lat
xy_labs <- labs(x = "Easting", y = "Northing")

# ---------------------------------------------------------------------------
# 1. Environmental covariate x(s)
# ---------------------------------------------------------------------------
x_rast_PO <- rast(data.frame(x = xy[,1], y = xy[,2], x_s))

p1 <- ggplot() +
  geom_spatraster(data = x_rast_PO) +
  scale_fill_viridis_c(option = "viridis", name = expression(x(s)),
                       na.value = "transparent") +
  labs(title = expression("Environmental covariate " * x(s))) +
  xy_labs +
  theme_pub

# ---------------------------------------------------------------------------
# 2. Presence-only thinning probability p(s)
# ---------------------------------------------------------------------------
beta_f  <- c(-5, 0.5)
alpha   <- c(-1, 1.3)

thinning_prob <- rast(data.frame(
  x = xy[,1], y = xy[,2],
  p = plogis(alpha[1] + alpha[2] * sampling_bias$s_cor)
))

p2 <- ggplot() +
  geom_spatraster(data = thinning_prob) +
  scale_fill_viridis_c(option = "viridis", name = expression(p(s)),
                       limits = c(0, 1), na.value = "transparent") +
  labs(title = expression("Presence-only thinning probability " * p(s))) +
  xy_labs +
  theme_pub

# ---------------------------------------------------------------------------
# 3. Log-intensity log(lambda(s))
# ---------------------------------------------------------------------------
lambda_f  <- beta_f[1] + beta_f[2] * x_s + omega_s
lambda_im <- rast(data.frame(x = xy[,1], y = xy[,2], z = lambda_f))

p3 <- ggplot() +
  geom_spatraster(data = lambda_im) +
  scale_fill_viridis_c(option = "viridis", name = expression(log(lambda(s))),
                       na.value = "transparent") +
  labs(title = expression("Log-intensity " * log(lambda(s)))) +
  xy_labs +
  theme_pub

# ---------------------------------------------------------------------------
# 4. Presence-absence sampling bias p*(s)
# ---------------------------------------------------------------------------
# BS is thresholded at its 85th percentile and only the upper tail is
# renormalised via softmax -> the field is exact 0 almost everywhere, with a
# small set of nonzero "hotspot" cells whose values are tiny relative to 0.
# A plain continuous viridis scale over [0, max] makes the hotspots
# invisible because 0 dominates the colour range.
#
# Fix: set the zero cells to NA. The fill scale is then computed only over
# the nonzero values, so the hotspots get the full colour resolution, and NA
# (background/unsampled) cells are drawn in a neutral grey so the sampled
# footprint is still clearly visible.

BS <- delta_s_PA
thr <- quantile(BS, probs = 0.85)
BS[BS < thr] <- 0
BS[BS >= thr] <- exp(BS[BS >= thr]) / sum(exp(BS[BS >= thr]))

BS_plot <- BS
BS_plot[BS_plot == 0] <- NA   # <- key change

bs_rast <- rast(data.frame(x = xy[,1], y = xy[,2], value = BS_plot))

p4 <- ggplot() +
  geom_spatraster(data = bs_rast) +
  scale_fill_viridis_c(
    option    = "viridis",
    name      = expression(p^"*"*(s)),
    na.value  = "grey88",              # 0-cells shown as neutral background
    labels    = label_scientific(digits = 2)  # small numbers -> readable
  ) +
  labs(title = expression(p^"*"*(s)~"(presence-absence sampling bias)")) +
  xy_labs +
  theme_pub

# Alternative if you'd rather keep the zeros visible in-scale rather than as
# background (e.g. to show exactly how sparse the >0 region is), use a
# sqrt/asinh transform instead of NA-masking:
#
# p4_alt <- ggplot() +
#   geom_spatraster(data = rast(data.frame(x = xy[,1], y = xy[,2], value = BS))) +
#   scale_fill_viridis_c(option = "viridis", name = expression(p^"*"*(s)),
#                         trans = "sqrt", na.value = "transparent") +
#   labs(title = expression(p^"*"*(s))) + xy_labs + theme_pub

# ---------------------------------------------------------------------------
# 5. Combined publication figure (2x2 panel, common legend widths handled
#    individually since scales differ)
# ---------------------------------------------------------------------------
fig <- (p1 | p2) / (p3 | p4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 11, face = "bold"))

fig

# ---------------------------------------------------------------------------
# 6. Export at publication resolution
# ---------------------------------------------------------------------------
ggsave("fig_covariates_bias.pdf", fig, width = 9, height = 7.5, device = cairo_pdf)
ggsave("fig_covariates_bias.png", fig, width = 9, height = 7.5, dpi = 600)
