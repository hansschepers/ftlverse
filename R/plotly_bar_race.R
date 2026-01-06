# perplexity made
library(data.table)
library(plotly)

# data: data.frame / data.table with cols time, category, value
# bar_race_plotly
plotly_bar_race<- function(data,
                            time_var    = "time",
                            category_var= "category",
                            value_var   = "value",
                            title       = "Bar chart race",
                            x_lab       = "Value",
                            y_lab       = "",
                            max_bars    = 20,
                            palette_fun = function(n) RColorBrewer::brewer.pal(max(3, min(8, n)), "Set2")) {
  
  dt <- as.data.table(data)
  t  <- time_var
  c  <- category_var
  v  <- value_var
  
  # keep top N per time
  dt <- dt[order(get(t), -get(v))]
  dt[, rank := rank(-get(v), ties.method = "first"), by = t]
  dt <- dt[rank <= max_bars]
  
  # order factor by value (largest on top) within each time
  dt[, cat_ordered := factor(get(c), levels = rev(unique(get(c))), ordered = TRUE), by = t]
  
  # color palette
  cats <- unique(dt[[c]])
  cols <- setNames(palette_fun(length(cats)), cats)
  
  fig <- plot_ly(
    data = dt,
    x    = ~get(v),
    y    = ~cat_ordered,
    frame= ~get(t),
    type = "bar",
    orientation = "h",
    text = ~round(get(v), 1),
    hovertemplate = paste0(
      "%{y}<br>",
      time_var, ": %{frame}<br>",
      value_var, ": %{x}<extra></extra>"
    ),
    marker = list(color = ~cols[get(c)])
  )
  
  # layout with dynamic x‑axis (largest bar fixed length)
  fig <- fig %>%
    layout(
      title  = list(text = title),
      xaxis  = list(title = x_lab),
      yaxis  = list(title = y_lab, autorange = "reversed"),
      bargap = 0.15
    ) %>%
    animation_opts(
      frame = 1000, transition = 500, easing = "linear", redraw = FALSE
    ) %>%
    animation_slider(
      currentvalue = list(prefix = paste0(time_var, ": "))
    )
  
  fig
}

# df_long: time, species, biomass
# fig <- plotly_bar_race(
#   data         = df_long,
#   time_var     = "year",
#   category_var = "species",
#   value_var    = "biomass",
#   title        = "Dominant species over time",
#   x_lab        = "Biomass (kg)"
# )
# 
# fig

