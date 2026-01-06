library(data.table)
library(ggplot2)
library(plotly)

## 1. Download & read mammal masses data ----
url_mammals <- "https://ndownloader.figshare.com/files/5593343"  # Mammal masses [web:24]
mammals_raw <- fread(url_mammals)   # columns: species, mass, order, etc.

str(mammals_raw)

set.seed(1)

# 2. Create pseudo time series of body mass per species ----
years <- 2000:2020

mammals_ts <- as.data.table(mammals_raw)[
  !is.na(mass) & mass > 0
][
  sample(.N, 40)  # take 40 species to keep plots readable
][
  , .(species, mass0 = mass)
]

# Expand over years and add simple growth + noise
mammals_ts <- mammals_ts[
  , .(year = years,
      mass = mass0 * (1 + 0.01 * (year - min(years))) *  # 1%/yr trend
        exp(rnorm(length(years), mean = 0, sd = 0.05))),
  by = species
]

# 3. Aggregate to “order” level for stacked areas ----
mammals_ord <- as.data.table(mammals_raw)[
  , .(species, order)
][
  mammals_ts,
  on = "species"
]

mammals_order_year <- mammals_ord[
  , .(biomass = sum(mass, na.rm = TRUE)),
  by = .(year, order)
]

# Clean up names for plotting functions
df_area <- mammals_order_year[
  !is.na(order)
][
  , .(time = year,
      group = order,
      value = biomass)
]






# assumes plot_dynamic_stacked_areas() from earlier answer is in your session
p_area <- plot_dynamic_stacked_areas(
  data         = df_area,
  time_var     = "time",
  group_var    = "group",
  value_var    = "value",
  title        = "Order‑level mammal biomass over time",
  subtitle     = "Toy time series derived from Mammal masses dataset",
  x_lab        = "Year",
  y_lab        = "Total body mass (g)",
  legend_title = "Order"
)

p_area






# 4. Prepare species‑level data for bar race ----
df_race <- mammals_ts[
  , .(time = year,
      category = species,
      value = mass)
]

# Optionally keep top N species per year to reduce clutter
df_race <- df_race[
  , .SD[order(-value)][1:15],
  by = time
]




# assumes bar_race_plotly() from earlier answer is in your session
fig_race <- bar_race_plotly(
  data         = df_race,
  time_var     = "time",
  category_var = "category",
  value_var    = "value",
  title        = "Body‑mass race among selected mammal species",
  x_lab        = "Body mass (g)"
)

fig_race





###############################################################

## install and load BioWorldR ----
# install.packages("BioWorldR")   # run once
# install.packages("BioWorldR", type = "binary")
library(BioWorldR)
library(data.table)
library(plotly)

## 1. Load birds dataset ----
?BioWorldR

data("birds_bw")   # name as documented in BioWorldR help [web:39]
dt <- as.data.table(birds_bw)

str(dt)
# assume it has at least: species (factor/character),
# year (integer or numeric), and count / abundance variable

## 2. Prepare data for bar-race ----
# harmonise column names; adapt if the dataset uses different ones
setnames(dt,
         old = c("species", "year", "abundance"),
         new = c("species", "year", "count"),
         skip_absent = TRUE)

# aggregate to species × year
df_race <- dt[
  , .(value = sum(count, na.rm = TRUE)),
  by = .(time = year, category = species)
]

# optional: keep top 15 species per year
df_race <- df_race[
  , .SD[order(-value)][1:15],
  by = time
]

## 3. Use your existing bar_race_plotly() function ----
fig <- bar_race_plotly(
  data         = df_race,
  time_var     = "time",
  category_var = "category",
  value_var    = "value",
  title        = "Bird abundances over time (BioWorldR birds dataset)",
  x_lab        = "Abundance (count)"
)

fig

