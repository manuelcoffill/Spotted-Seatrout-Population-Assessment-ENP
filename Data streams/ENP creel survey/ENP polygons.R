# ============================================================
# EVERGLADES NATIONAL PARK ZONE MAP
# ============================================================
#
# PURPOSE:
#   Read polygon geometries stored as WKT in an Excel file,
#   convert them to sf polygons, assign an appropriate CRS,
#   create a high-resolution map of the Everglades region,
#   overlay the fishing/management zones, label the zones,
#   and export publication-quality figures.
#
# ASSUMED SOURCE CRS:
#
#   NAD83 / UTM zone 17N
#   EPSG:26917
#
# WHY EPSG:26917?
#
#   Your coordinates look approximately like:
#
#       X = 450,000 to 520,000
#       Y = 2,780,000 to 2,865,000
#
#   These are meter-based projected coordinates, not longitude
#   and latitude. Their magnitude and location are consistent
#   with South Florida in UTM Zone 17N.
#
#   We therefore initially interpret the WKT coordinates as:
#
#       NAD83 / UTM zone 17N (EPSG:26917)
#
#   If the resulting map places the polygons correctly over
#   Everglades National Park, the CRS assignment is appropriate.
#
# ============================================================


# ============================================================
# 1. INSTALL PACKAGES IF NECESSARY
# ============================================================

# Run this section once if you don't already have the packages.

packages <- c(
  "readxl",
  "dplyr",
  "sf",
  "ggplot2",
  "rnaturalearth",
  "rnaturalearthdata",
  "ggspatial",
  "viridis",
  "scales",
  "here"
)

installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}


# ============================================================
# 2. LOAD PACKAGES
# ============================================================

library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(viridis)
library(scales)
library(here)

# ============================================================
# 3. SET YOUR FILE PATH
# ============================================================

# Change this to the location/name of your Excel file.

file_path <- here(
  "EVER_creel_fishing_areas.csv"
)

# ============================================================
# 4. READ THE EXCEL FILE
# ============================================================

# Your Excel file should contain a column called "wkt".
#
# Other columns in your example include:
#
#   zone
#   num
#   zoneName
#   unitCode
#   unitName
#   groupCode
#   groupName
#   wkt
#
# read_excel() imports the WKT column as character text.

dat <- read.csv(file_path)


# ============================================================
# 5. CHECK THE DATA
# ============================================================

# Look at the first few records.

head(dat)

# Check the column names.

names(dat)

# Check how many records you have.

nrow(dat)

# Check the geometry type represented in the WKT column.

table(substr(dat$wkt, 1, 15))


# ============================================================
# 6. CONVERT WKT TO SF POLYGONS
# ============================================================

# The WKT column contains:
#
#   POLYGON(...)
#
# and
#
#   MULTIPOLYGON(...)
#
# sf can read both geometry types.
#
# IMPORTANT:
#
# We assign EPSG:26917 here because this is the CRS that
# appears most consistent with the coordinate values you
# provided.
#
# EPSG:26917 =
# NAD83 / UTM zone 17N
#
# The coordinates remain in meters at this stage.

enp <- st_as_sf(
  dat,
  wkt = "wkt",
  crs = 26917
)


# ============================================================
# 7. CHECK THE CRS
# ============================================================

st_crs(enp)


# You should see something corresponding to:
#
# NAD83 / UTM zone 17N
#
# EPSG:26917


# ============================================================
# 8. CHECK THE POLYGONS
# ============================================================

# This is a very useful first diagnostic.
#
# It will plot the polygons using base R.

plot(
  st_geometry(enp),
  border = "black"
)


# ============================================================
# 9. CHECK THE SPATIAL EXTENT
# ============================================================

# This shows the minimum and maximum X/Y coordinates.

st_bbox(enp)


# The values should be approximately in the range you
# supplied:
#
# X ~ 450,000-520,000
# Y ~ 2,780,000-2,865,000
#
# These are meters in UTM Zone 17N.


# ============================================================
# 10. VALIDATE THE POLYGONS
# ============================================================

# Polygon datasets sometimes contain small geometry problems,
# especially when imported from GIS/WKT.
#
# st_make_valid() repairs many common issues.

# enp <- enp %>%
#   mutate(
#     geometry = st_make_valid(geometry)
#   )


# Check whether any geometries are still invalid.

sum(!st_is_valid(enp))


# Ideally this returns:
#
# [1] 0


# ============================================================
# 11. CALCULATE POLYGON CENTROIDS / LABEL POINTS
# ============================================================

# We want labels such as:
#
#   6N
#   6C
#   6S
#   5
#   4
#
# st_point_on_surface() is preferable to a simple centroid
# because it guarantees that the label point falls within
# the polygon (or on its surface).

label_points <- enp %>%
  st_point_on_surface()


# ============================================================
# 12. TRANSFORM EVERYTHING TO LONGITUDE/LATITUDE
# ============================================================

# EPSG:4326 is WGS84 longitude/latitude.
#
# We don't need to keep the data in this CRS permanently.
# We use it here because geographic background datasets are
# commonly provided in longitude/latitude.

enp_ll <- st_transform(
  enp,
  crs = 4326
)

label_points_ll <- st_transform(
  label_points,
  crs = 4326
)


# ============================================================
# 13. GET HIGH-RESOLUTION SOUTH FLORIDA LAND DATA
# ============================================================

# Natural Earth provides a relatively high-resolution
# 1:10-million scale dataset.
#
# This is substantially better than using a simple
# low-resolution world map and is sufficient for a regional
# overview map.
#
# Importantly, this is VECTOR data, not a raster screenshot.
#
# Therefore the coastline remains sharp when exported
# to PDF.

florida <- ne_states(
  country = "United States of America",
  returnclass = "sf"
) %>%
  filter(name_en == "Florida") %>%
  st_transform(4326)


# ============================================================
# 14. CREATE A SOUTH FLORIDA MAP EXTENT
# ============================================================

# Get the geographic extent of your ENP polygons.

bbox <- st_bbox(enp_ll)

# Inspect the bounding box.
bbox


# Convert each value explicitly to numeric.
#
# This prevents problems caused by the named vector returned
# by st_bbox() being passed directly into st_crop().

xmin_map <- as.numeric(bbox["xmin"]) - 0.15
xmax_map <- as.numeric(bbox["xmax"]) + 0.15
ymin_map <- as.numeric(bbox["ymin"]) - 0.10
ymax_map <- as.numeric(bbox["ymax"]) + 0.10


# Check that none of the values are NA.

c(
  xmin_map,
  xmax_map,
  ymin_map,
  ymax_map
)


# ============================================================
# 15. CROP FLORIDA TO THE MAP EXTENT
# ============================================================

# Create the crop extent as an sf bounding box.
#
# This is more robust than supplying xmin/xmax/ymin/ymax
# individually to st_crop().

map_extent <- st_bbox(
  c(
    xmin = xmin_map,
    ymin = ymin_map,
    xmax = xmax_map,
    ymax = ymax_map
  ),
  crs = st_crs(florida)
)


# Crop the Florida polygon.

florida_crop <- st_crop(
  florida,
  map_extent
)


# Check the result.

florida_crop




##############################################################################
# ============================================================
# ADD IMPORTANT ACCESS LOCATIONS
# ============================================================

# Create a data frame containing the locations you want
# to show on the map.
#
# Coordinates are in decimal degrees:
#
#   Flamingo Boat Ramp:
#       Longitude = -80.9227
#       Latitude  = 25.1440
#
#   Chokoloskee Boat Ramp:
#       Longitude = -81.35890
#       Latitude  = 25.81855

access_points <- data.frame(
  location = c(
    "Flamingo",
    "Chokoloskee"
  ),
  
  longitude = c(
    -80.9227,
    -81.35890
  ),
  
  latitude = c(
    25.1440,
    25.81855
  )
)


# Convert the data frame to an sf point object.
#
# The coordinates are currently longitude/latitude, so we
# assign WGS84 (EPSG:4326), matching enp_ll.

access_points_sf <- st_as_sf(
  access_points,
  coords = c("longitude", "latitude"),
  crs = 4326
)




# ============================================================
# 16. CREATE THE MAIN MAP
# ============================================================

# geom_sf() draws the actual polygons.
#
# aes(fill = zoneName) gives every management zone its own
# fill color.
#
# color = "black" draws the zone boundaries.
#
# linewidth controls the boundary thickness.

p <- ggplot() +
  
  # ----------------------------------------------------------
# FLORIDA LAND / BACKGROUND
# ----------------------------------------------------------

geom_sf(
  data = florida_crop,
  fill = "grey90",
  color = "grey30",
  linewidth = 0.4
) +
  
  # ----------------------------------------------------------
# ENP ZONES
# ----------------------------------------------------------

geom_sf(
  data = enp_ll,
  aes(fill = zoneName),
  color = "black",
  linewidth = 0.6,
  alpha = 0.75
) +
  
# ============================================================
# ACCESS LOCATION POINTS
# ============================================================

geom_sf(
  data = access_points_sf,
  shape = 21,
  size = 4,
  fill = "white",
  color = "black",
  stroke = 1.2
) +
# ============================================================
# ACCESS LOCATION LABELS
# ============================================================

geom_sf_text(
  data = access_points_sf,
  aes(label = location),
  nudge_y = 0.03,
  size = 4,
  fontface = "bold"
) +
  # ----------------------------------------------------------
# ZONE LABELS
# ----------------------------------------------------------

geom_sf_text(
  data = label_points_ll,
  aes(label = zone),
  size = 4.5,
  fontface = "bold"
) +
  
# ----------------------------------------------------------
# MAP EXTENT
# ----------------------------------------------------------

coord_sf(
  xlim = c(xmin_map, xmax_map),
  ylim = c(ymin_map, ymax_map),
  expand = FALSE
) +
  
  # ----------------------------------------------------------
# COLOR PALETTE
# ----------------------------------------------------------

scale_fill_brewer(
  palette = "Set3",
  name = "ENP Zone"
) +
  
  # ----------------------------------------------------------
# NORTH ARROW
# ----------------------------------------------------------

annotation_north_arrow(
  location = "tr",
  which_north = "true",
  style = north_arrow_fancy_orienteering
) +
  
  # ----------------------------------------------------------
# SCALE BAR
# ----------------------------------------------------------

annotation_scale(
  location = "bl",
  width_hint = 0.25
) +
  
  # ----------------------------------------------------------
# LABELS
# ----------------------------------------------------------

labs(
  title = "Everglades National Park",
  subtitle = "Creel Survey Defined Fishing Areas",
  x = "Longitude",
  y = "Latitude"
) +
  
  # ----------------------------------------------------------
# THEME
# ----------------------------------------------------------

theme_bw() +
  
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 13
    ),
    
    axis.title = element_text(
      size = 11
    ),
    
    axis.text = element_text(
      size = 9
    ),
    
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 9
    ),
    
    legend.position = "right",
    
    panel.grid.major = element_line(
      linewidth = 0.25
    ),
    
    panel.grid.minor = element_blank()
  )


# ============================================================
# 17. DISPLAY THE MAP
# ============================================================

p


# ============================================================
# 18. SAVE A HIGH-RESOLUTION PNG
# ============================================================

# 600 DPI is appropriate for a high-resolution manuscript
# figure.
#
# Width and height are specified in inches.
#
# The resulting PNG will be substantially larger than a
# normal screen-resolution image.

ggsave(
  filename = "ENP_zones.png",
  plot = p,
  width = 10,
  height = 8,
  units = "in",
  dpi = 1000
)


# ============================================================
# 19. SAVE A VECTOR PDF
# ============================================================

# I strongly recommend also creating a PDF.
#
# Unlike a PNG, the polygon boundaries are stored as vectors.
#
# Consequently, the map can be enlarged substantially without
# pixelation.
#
# This is usually the better format for a scientific
# publication.

# ggsave(
#   filename = "ENP_zones_vector.pdf",
#   plot = p,
#   width = 10,
#   height = 8,
#   units = "in"
# )


# ============================================================
# 20. OPTIONAL: CREATE A MAP WITH ONLY ZONE BOUNDARIES
# ============================================================

# This version is useful if you want the ENP zones to be
# shown as outlines rather than colored polygons.

p_outline <- ggplot() +
  
  geom_sf(
    data = florida_crop,
    fill = "grey90",
    color = "grey30",
    linewidth = 0.4
  ) +
  
  geom_sf(
    data = enp_ll,
    fill = NA,
    color = "black",
    linewidth = 0.8
  ) +
  # ============================================================
# ACCESS LOCATION POINTS
# ============================================================

geom_sf(
  data = access_points_sf,
  shape = 21,
  size = 4,
  fill = "white",
  color = "black",
  stroke = 1.2
) +
  # ============================================================
# ACCESS LOCATION LABELS
# ============================================================

geom_sf_text(
  data = access_points_sf,
  aes(label = location),
  nudge_y = 0.03,
  size = 4,
  fontface = "bold"
) +
  geom_sf_text(
    data = label_points_ll,
    aes(label = zone),
    size = 4.5,
    fontface = "bold"
  ) +
  
  coord_sf(
    xlim = c(xmin_map, xmax_map),
    ylim = c(ymin_map, ymax_map),
    expand = FALSE
  ) +
  
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.25
  ) +
  
  labs(
    title = "Everglades National Park",
    subtitle = "Creel Survey Defined Fishing Areas",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_bw() +
  
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 13
    ),
    
    axis.title = element_text(
      size = 11
    ),
    
    axis.text = element_text(
      size = 9
    ),
    
    panel.grid.minor = element_blank()
  )


# Display outline version.

p_outline


# Save outline version.

ggsave(
  filename = "ENP_zone_boundaries.png",
  plot = p_outline,
  width = 10,
  height = 8,
  units = "in",
  dpi = 1000
)

# ggsave(
#   filename = "ENP_zone_boundaries_vector.pdf",
#   plot = p_outline,
#   width = 10,
#   height = 8,
#   units = "in"
# )


# ============================================================
# 21. OPTIONAL: CHECK THE CRS AGAIN
# ============================================================

# This confirms the original and transformed coordinate
# systems.

st_crs(enp)

st_crs(enp_ll)


# ============================================================
# 22. OPTIONAL: CHECK THAT THE POLYGONS ARE IN THE
#     EXPECTED EVERGLADES LOCATION
# ============================================================

# Calculate the overall geographic extent after transforming
# to longitude/latitude.

st_bbox(enp_ll)


# You should see something approximately in the vicinity of:
#
# longitude: ~ -81 to -80
# latitude:  ~ 25 to 26
#
# Exact values will depend on the full polygon dataset.
#
# If you see coordinates in a completely different part of
# the world, STOP and revisit the CRS assumption.
#
# If the polygons appear over South Florida/Everglades
# National Park, EPSG:26917 is behaving as expected.


# ============================================================
# END OF SCRIPT
# ============================================================