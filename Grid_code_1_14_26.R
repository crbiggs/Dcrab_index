library(sf)
library(tidyverse)
library(sdmTMB)
utm_crs <- st_crs(32610) 
# Define geographic boundaries in lat lon
lat_min <- 45.5
lat_max <- 48.4
lon_max <- -125.0

corners <- data.frame(
  lon = c(-125.0, -125.0, -122.0, -122.0),
  lat = c(lat_min, lat_max, lat_min, lat_max)
)

## create a bounding box and convert to utm
corners_sf <- st_as_sf(corners, coords = c("lon", "lat"), crs = 4326)
corners_utm <- st_transform(corners_sf, utm_crs)
corners_coords <- st_coordinates(corners_utm)

# Get UTM extent
utm_xmin <- min(corners_coords[, "X"])
utm_xmax <- max(corners_coords[, "X"])
utm_ymin <- min(corners_coords[, "Y"])
utm_ymax <- max(corners_coords[, "Y"])

# Create grid with 16km^2 spacing
grid_spacing <- 4000  # 4 km in meters
x_seq <- seq(utm_xmin, utm_xmax, by = grid_spacing)
y_seq <- seq(utm_ymin, utm_ymax, by = grid_spacing)

#fill in points within the grid 
grid_points <- expand.grid(X = x_seq, Y = y_seq)


#Convert to a sf object 
grid_sf <- st_as_sf(grid_points, coords = c("X", "Y"), crs = utm_crs)
ggplot() + geom_sf(data=grid_sf)+geom_sf(data=WA_coast_proj)
#Convert utm back to lat lon so that it can be cropped by map of coastline
grid_latlon <- st_transform(grid_sf, 4326)


## Get map of coastline 
library(rnaturalearth)
map_data <- rnaturalearth::ne_countries(scale="large", returnclass = "sf")




sf_use_s2(FALSE)


#cropping the extent of the map, can use the bounding box limits to know what lat and long to plug in 
WC <-suppressWarnings(suppressMessages( sf::st_crop(map_data, c(xmin=-125.25, ymin=45.5, xmax=-117.3, ymax=48.5), )))

## find points within the map object
within <- st_within(grid_latlon, WC, sparse=F)

## remove points that are within, to be left with points outside the land object.
not_within <- grid_latlon[!within,]

#Data frame with coordinate points and filter out extra points I don't want 
WCgrid16km <- as.data.frame(st_coordinates(not_within))
ggplot()+geom_point(data=WCgrid16km, aes(X,Y))+geom_sf(data=WA_coast)

colnames(WCgrid16km) <- c("Lon","Lat")
WCgrid16km <- WCgrid16km |> filter(!(Lat >48.1 & Lon > -124.71))
WCgrid16km <- WCgrid16km |> filter(Lon < -123.5)

## Add UTM coordinates back in 

GridD16_utm <- add_utm_columns(WCgrid16km, ll_names=c("Lon","Lat") )

## UTM projection should be correct, lat/lon should be slightly skewed 
ggplot() + geom_point(data=WCgrid16km, aes(Lon,Lat)) + geom_sf(data=WA_coast)
ggplot() + geom_point(data=GridD16_utm, aes(X*1000,Y*1000))+ geom_sf(data=WA_coast_proj)

## Next add depths
library(raster)
gebco_df <- as.data.frame(gebco_raster, xy=T)

colnames(gebco_df) <- c("Lon", "Lat", "Depth")

depth_value <- raster::extract(gebco_raster,GridD16_utm[1:2])
depthB <- as.vector(depth_value)
depthC <- as.data.frame(depthB)

G16all <- cbind(GridD16_utm, depthC)
save(G16all, file="G16all.RData")

G16all <- G16all |> filter(depthB < 0)


G16YD <- expand_grid(G16all, Cyear=c(2010:2028))
G16YDI <- left_join(G16YD, I5, by="Cyear")

save(G16YDI, file="G16YDI.RData")
