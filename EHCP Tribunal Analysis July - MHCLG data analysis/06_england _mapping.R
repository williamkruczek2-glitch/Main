
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)

# --- Boundary file ---------------------------------------------------
# OPTION A (no download needed): read_sf() can pull the boundaries
# straight from the ONS Open Geography Portal's ArcGIS FeatureServer.
# This is the CTYUA December 2024 release, BGC variant (generalised
# 20m, clipped to coastline) - ONS's own guidance recommends
# generalised boundaries for maps (full-resolution BFC is for
# geospatial operations, and is much slower to load/plot).
# f=geojson makes the server return GeoJSON, which sf reads natively.
boundary_url <- paste0(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
  "Counties_and_Unitary_Authorities_December_2024_Boundaries_UK_BGC/",
  "FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
)
boundary_df <- read_sf(boundary_url)

boundary_clean_df <- boundary_df %>%
  rename(
    ons_code       = CTYUA24CD,
    authority_name = CTYUA24NM
  ) %>%
  select(ons_code, authority_name, geometry) %>%
  filter(str_starts(ons_code, "E"))   # England only

# --- Basic map of England, no data joined yet ------------------------
ggplot(boundary_clean_df) +
  geom_sf(fill = "#4C72B0", colour = "white", linewidth = 0.2) +
  theme_void() +
  labs(title = "England - local authority boundaries")

# --- London inset -----------------------------------------------------
# London boroughs are too small to read on a national map - colleague's
# script pulls them into a separate zoomed-in plot. E09 = London boroughs.
boundary_london_df <- boundary_clean_df %>%
  filter(str_starts(ons_code, "E09"))

ggplot(boundary_london_df) +
  geom_sf(fill = "#DD8452", colour = "white", linewidth = 0.3) +
  theme_void() +
  labs(title = "London boroughs (inset)")

