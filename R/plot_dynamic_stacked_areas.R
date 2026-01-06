# perplexity made
# library(ggplot2)
# library(data.table)

plot_dynamic_stacked_areas <- function(
    data,
    time_var   = "year",
    group_var  = "group",
    value_var  = "value",
    title      = "Top groups over time",
    subtitle   = "Dynamic stacking – order changes by rank each year",
    x_lab      = "Year",
    y_lab      = "Value",
    legend_title = NULL,
    legend_position = "right",     # "none", "bottom", "top", "left"
    palette    = scale_fill_manual, # or scale_fill_brewer, scale_fill_viridis_d, …
    palette_args = list(values = NULL) # e.g. list(values = RColorBrewer::brewer.pal(8,"Set2"))
) {
  
  dt <- as.data.table(data)
  t  <- time_var
  g  <- group_var
  v  <- value_var
  
  # order groups within each time by descending value
  dt[, rank_within_t := frank(-get(v), ties.method = "first"), by = t]
  
  # use that rank as stacking order
  p <- ggplot(dt, aes(
    x   = .data[[t]],
    y   = .data[[v]],
    fill = .data[[g]],
    group = interaction(.data[[g]], rank_within_t, drop = TRUE)
  )) +
    geom_area(color = NA, position = "stack") +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = x_lab,
      y        = y_lab,
      fill     = legend_title
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = legend_position,
      panel.grid.minor = element_blank()
    )
  
  # apply palette scale (function + args)
  p <- p + do.call(palette, c(list(aesthetics = "fill"), palette_args))
  
  return(p)
}


# df_long: columns year, company, market_cap
# plot_dynamic_stacked_areas(
#   data = df_long,
#   time_var  = "year",
#   group_var = "company",
#   value_var = "market_cap",
#   title     = "Top 20 species by biomass",
#   x_lab     = "Year",
#   y_lab     = "Biomass (kg)",
#   legend_title = "Species",
#   legend_position = "right",
#   palette = scale_fill_manual,
#   palette_args = list(values = RColorBrewer::brewer.pal(8, "Set2"))
# )
