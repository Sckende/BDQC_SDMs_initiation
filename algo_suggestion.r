library(terra)
library(sf)
library(dplyr)

# Province polygons
prov <- st_as_sf(terra::readRDS("/home/claire/BDQC-GEOBON/data/gadm/gadm41_CAN_1_pk.rds"))

# Selection of Ontario & convertion in UTM
onUTM <- prov %>% 
            filter(NAME_1 == "Ontario") %>% 
            st_transform(st_crs("+init=epsg:2027"))
x11(); plot(st_geometry(onUTM))

# Grid creation
grid_spacing <- 50000  # size of squares, in units of the CRS 

grid_glob <- st_make_grid(
    onUTM,
    square = T,
    cellsize = c(grid_spacing, grid_spacing)) %>% # the grid, covering bounding box
  st_sf()

plot(grid_glob, col = 'white')
plot(st_geometry(onUTM), add = T)

overlay <- st_intersects(
    onUTM,
    grid_glob)

grid_on <- grid_glob[overlay[[1]], ]

plot(st_geometry(grid_on))
plot(st_geometry(onUTM), add = T)

#### see also - Alexandros Karatzoglou & recommender system
