# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------
require(sf)
require(dplyr)
require(tidyverse)
require(raster)
require(inlabru)
require(INLA)
require(ggplot2)
require(viridis)
require(RColorBrewer)
require(patchwork)
require(spatstat)
require(terra)
require(tidyterra)
require(data.table)
require(readxl)
require(corrplot)
require(RColorBrewer)
require(leaflet)
require(geodata)
require(fmesher)
require(ggspatial)
require(cowplot)
require(gridExtra)
require(grid)
require(stars)
require(purrr)
require(ggthemes)
require(scico)
require(sp)
require(scales)
require(grDevices)
require(excursions)
require(latex2exp)

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

# ---------------------------------------------------------------------------
# Boxplot Code for the simulation
# ---------------------------------------------------------------------------
theme_box <- theme_minimal(base_size = 15) +
  theme(
    plot.title        = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 6)),
    axis.title       = element_text(size = 15),
    axis.text        = element_text(size = 12, colour = "black"),
    legend.title     = element_text(size = 13),
    legend.text      = element_text(size = 10),
    legend.key.width  = unit(0.35, "cm"),
    legend.key.height = unit(0.9, "cm"),
    plot.margin      = margin(5, 8, 5, 5)
  )
create_boxplot <- function(metric_column, 
                           title_label, 
                           y_axis_label, 
                           true_par = NA, 
                           df = trues_prs_abs_df) {
  
  # Specify the exact models to keep and display in order
  target_models <- c("A", "B", "C", "D", "E", "F", "G")
  
  pp <- ggplot(df, aes(x = mod, y = .data[[metric_column]], fill = par)) +
    geom_boxplot(
      alpha = 0.8, 
      outlier.size = 1.5, 
      linewidth = 0.6, 
      fatten = 1
    ) +
    scale_fill_brewer(palette = "Set2") +
    scale_x_discrete(limits = target_models) + # Shows strictly "A".."G" with their true data
    theme_box +
    theme(legend.position = "none") +
    labs(
      title = title_label,
      x = "Model",
      y = y_axis_label
    )
  
  if (!is.na(true_par)) {
    pp <- pp + geom_hline(
      yintercept = true_par, 
      color = "red", 
      linetype = "dashed", 
      linewidth = 0.8
    )
  }
  
  return(pp)
}


# ---------------------------------------------------------------------------
# Shared theme Model fitting
# ---------------------------------------------------------------------------
theme_pub_fit <- theme_minimal(base_size = 15) +
  theme(
    plot.title      = element_text(face = "bold", size = 18, hjust = 0.5,
                                   margin = margin(b = 6)),
    axis.title      = element_text(size = 15),
    axis.text       = element_text(size = 9, colour = "black"),
    legend.title    = element_text(size = 13),
    legend.text     = element_text(size = 10),
    legend.key.width  = unit(0.1, "cm"),
    legend.key.height = unit(5, "cm"),
    plot.margin     = margin(5, 8, 5, 5),
    legend.position = "bottom"
  )

xy_labs <- labs(x = "South", y = "West")
