###### load useful packages to plot and the useful objects ####
source("C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Utils/Packages.R")
load("Sim_Def.RData") # load all the simulated data and environmental covariates
block_size <- 25 # size of the block for count data
Count_data <- Count_Data_all|> # consider just realization 1
  filter(sim == 1)

win <- owin(c(0,600), c(0,600))
npix <- 2000

Domain <- rast(nrows=npix, ncols=npix,
               xmax=win$xrange[2],xmin=win$xrange[1],
               ymax = win$yrange[2],ymin=win$yrange[1]) # Define the domain

countGrid <- st_make_grid(Domain, cellsize = c(block_size, block_size)) %>% 
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(blockid = row_number())

# ---------------------------------------------------------------------------
# Shared theme
# ---------------------------------------------------------------------------
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
    panel.grid      = element_blank(),
    plot.margin     = margin(5, 8, 5, 5)
  )

xy_labs <- labs(x = "South", y = "West")
n_data <- 100

# ---------------------------------------------------------------------------
# Plot A: Presence-only detection state
# ---------------------------------------------------------------------------
sim_po_data <- po_sf_new[po_sf_new$sim == 1, ]

p_detection <- ggplot() +
  geom_sf(data = boundary_sf, fill = "grey98", color = "black", linewidth = 0.6) +
  geom_sf(data = sim_po_data, aes(col = as.factor(det1)), size = 1.5, alpha = 0.8) +
  scale_color_manual(values = c("0" = "#ee6c4d", "1" = "#3d5a80"), name = "Detected") +
  labs(title = "Presence-Only Realization") +
  xy_labs +
  theme_pub

p_detection

# ---------------------------------------------------------------------------
# Plot B: Biased sampling grid & true presence pattern
# ---------------------------------------------------------------------------
customGrid$tst <- Biased_Sampling_10cellID[, 1]

sim_pp_set <- pp_set_0 %>% 
  filter(sim == 1 + n_data) %>% 
  st_as_sf(coords = c("x", "y"), crs = st_crs(customGrid))

sim_true_pres <- True_Pres_all %>% 
  filter(sim == 1 & scheme == "BS") %>% 
  st_as_sf(coords = c("X", "Y"), crs = st_crs(customGrid))

p_grid <- ggplot() +
  geom_sf(data = customGrid, aes(fill = factor(tst)), color = "white", linewidth = 0.1, alpha = 0.6) +
  scale_fill_viridis_d(option = "mako", name = "Sampled Cell") +
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.7) +
  geom_sf(data = sim_true_pres, aes(color = factor(pres)), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("0" = "#ee6c4d", "1" = "#3d5a80"), name = "Presence") +
  labs(title = "Presence-Absence Realization") +
  xy_labs +
  theme_pub

p_grid

# ---------------------------------------------------------------------------
# Plot C: Count data — sampled blocks with observed counts
# ---------------------------------------------------------------------------
p_count <- ggplot() +

  geom_sf(data = countGrid, fill = "grey95", color = "grey80", linewidth = 0.15) +
  geom_sf(data = Count_data, aes(fill = count), color = "black", linewidth = 0.3) +
  scale_fill_viridis_c(option = "viridis", name = "Counts") +
  geom_sf_text(
    data = Count_data,
    aes(label = count,
        color = count > (max(count) / 2)),
    size = 3.2, fontface = "bold", show.legend = FALSE
  ) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "white")) +
  
  geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.6) +
  
  labs(title = "Count Data Realization") +
  xy_labs +
  theme_pub

p_count

# ---------------------------------------------------------------------------
# Export
# ---------------------------------------------------------------------------
plot_path <- "C:/Users/gmsan/Documents/GitHub/Street_Dog_Cats/Figures/Figure Appendix/"
ggsave(filename = file.path(plot_path, "PO.png"), p_detection, width = 8.5, height = 7.5, dpi = 300)
ggsave(filename = file.path(plot_path, "PA.png"), p_grid, width = 8.5, height = 7.5, dpi = 300)
ggsave(filename = file.path(plot_path, "Count.png"), p_count, width = 8.5, height = 7.5, dpi = 300)
