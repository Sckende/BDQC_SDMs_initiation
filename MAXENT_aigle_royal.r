# Production de SDMs avec maxent sur les donnees d'aigle royal
# origine des donnees d'occurences - atlas
# origine des donnees bioclimatiques - WorldClim - https://www.worldclim.org/data/bioclim.html
# package & methode utilises - ENMeval & maxent - vignette https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html

#### Packages ####
# ------------- #
library(ratlas)
library(raster)
library(sf)
library(mapview)
library(geodata)
library(geojsonsf)
library(lubridate)
library(ENMeval)

#### species data ####
# ------------------ #
## --> DL data 
# taxa <- get_taxa(
#     scientific_name = "Aquila chrysaetos"
#     )
# taxa # 10038

# obs <- get_observations(
#     id_taxa = 10038
# )

# saveRDS(
#     obs,
#     "/home/claire/BDQC-GEOBON/data/occurences/BIRD_aigle_royal_occ.rds"
# )

obs <- readRDS("/home/claire/BDQC-GEOBON/data/occurences/BIRD_aigle_royal_occ.rds")

dim(obs)
class(obs)
head(obs)
names(obs)

## --> data treatment

# Obs in Quebec
obss <- obs[obs$within_quebec == TRUE, ]

# Obs during the breeding period - from March to August
table(month(obss$month_obs))

obsss <- obss[obss$month_obs %in% 3:8, ]

# ----
pts <- obsss$geom
pts2 <- st_coordinates(pts) # retrieve the coordinates
head(pts2)

occs <- pts2[!duplicated(pts2), ] # deletion of duplicated coordinates
dim(pts2); dim(occs)

occs <- as.data.frame(occs)
names(occs) <- c("lon", "lat")

#### WorldClim data - bioclimatic data ####
# -------------------------------------- #
# ?geodata
# ?worldclim_country

#bioclim <- worldclim_country(
#    country = "Canada",
#    var = "bio",
#    #res = 2.5,
#    path = "." # failed when changing path
#) # process time ~ 18 min - run the fonction in R cause pb with vsc.

bioc <- terra::rast("/home/claire/BDQC-GEOBON/data/bioclim_data/wc2.1_country/CAN_wc2.1_30s_bio.tif")
names(bioc)

## --> keep annual mean temp (BIO1) & annual precipitation (BIO12)
# ----

bioclim <- bioc[[names(bioc) %in% c("wc2.1_30s_bio_1", "wc2.1_30s_bio_12")]]
x11(); plot(bioclim)

## --> keep Quebec only
rang <- ext(
    -90,
    -50,
    35,
    70
) # min/max lon, min/max lat - definition of SpatExtent

bioclim_qc <- crop(
    bioclim,
    rang
)
plot(bioclim_qc[[2]])
points(occs, pch = 20, alpha = 0.5)

## --> add province delimitation
# ----

#prov <- gadm(country = "CAN",
#level = 1,
#path = ".")

prov <- terra::readRDS("/home/claire/BDQC-GEOBON/data/gadm/gadm41_CAN_1_pk.rds")

plot(prov, add = T, border = "grey")

## --> habitat similarity
# ----

# we extract the climatic variable values at the occurrence points -- these values are our "reference".
# class(occs)
# rast <- raster::stack(bioclim_qc) # necessite de convertir SpatRaster en rasterStack
# occs_comp <- raster::extract(rast, occs)
# head(occs_comp); dim(occs_comp)
# occs_comp <- na.omit(occs_comp)
# summary(occs_comp)

# Now we use the similarity() function (borrowed from the rmaxent package) to calculate environmental similarity metrics of our predictor variable extent compared to the reference points.
# occs_sim <- ENMeval::similarity(rast[[1]], occs_comp) # warning - raster lourd - très grand nombre de pixels
# str(occs_sim, 1)
# occs_mess <- occs_sim$similarity_min

# This is the MESS plot -- increasingly negative values represent increasingly different climatic conditions from the reference (our occurrences), while increasingly positive values are more similar.
# occs_sp <- sp::SpatialPoints(occs)

# Vector data (points, polygons) are added to a levelplot with a "+", like ggplot.
# rasterVis::levelplot(
#     occs_mess,
#     main = "Environmental similarity",
#     margin = FALSE) + 
#   latticeExtra::layer(
#     sp.points(occs.sp,
#     col = "black"))

## --> Specify study extent & sample random points
# ----

class(occs); head(occs)
occs_sf <- sf::st_as_sf(
    occs,
    coords = c("lon", "lat"),
    crs = terra::crs(bioclim_qc)
)

occs_sf_utm <- sf::st_transform(
    occs_sf,
    st_crs("+init=epsg:2031") # conversion en UTM pour la creation d'un buffer
)

# Buffer all occurrences by 500 km (to cconfirm), union the polygons together (for visualization), and convert back to a form that the raster package can use. Finally, we reproject the buffers back to WGS84 (lat/lon).
occs.buf <- sf::st_buffer(occs_sf_utm, dist = 250000) %>% # 250 km
  sf::st_union() %>% 
  sf::st_sf() %>%
  sf::st_transform(crs = raster::crs(bioclim_qc))
plot(bioclim_qc[[1]], main = names(bioclim_qc)[1])
points(occs)
# To add sf objects to a plot, use add = TRUE
plot(occs.buf, border = "blue", lwd = 3, add = TRUE)


# Crop environmental rasters to match the study extent
envs.bg <- raster::crop(bioclim_qc, occs.buf)
# Next, mask the rasters to the shape of the buffers
envs.bg <- raster::mask(envs.bg, occs.buf)

# Temperatures
plot(envs.bg[[1]], main = names(bioclim_qc)[1])
points(occs)
plot(occs.buf, border = "blue", lwd = 3, add = TRUE)

# Precipitations
plot(envs.bg[[2]], main = names(bioclim_qc)[2])
points(occs)
plot(occs.buf, border = "blue", lwd = 3, add = TRUE)


# Sample 10,000 random points (or whatever the desired number --> ****)
# only one per cell without replacement

# bg <- dismo::randomPoints(envs.bg[[9]], n = 10000) %>% as.data.frame()
bg <- raptr::randomPoints(
    envs.bg[[1]],
    n = 10000
) %>% as.data.frame()
head(bg)
colnames(bg) <- colnames(occs)

# Notice how we have pretty good coverage (every cell).
plot(envs.bg[[1]])
points(bg, pch = 20, cex = 0.2)


#### Partitioning occurences for eval ####
# -------------------------------------- #
# allowing cross-validation
# choice of the partitioning method
# can be done manually with the partitioning function or automatically in ENMeval()

# for illustration, test of the block method
block <- get.block(occs, bg, orientation = "lat_lon")
# Let's make sure that we have an even number of occurrences in each partition.
table(block$occs.grp)
# We can plot our partitions on one of our predictor variable rasters to visualize where they fall in space.
# The ENMeval 2.0 plotting functions use ggplot2 (Wickham 2016)
evalplot.grps(pts = occs, pts.grp = block$occs.grp, envs = raster(envs.bg[[1]])) + 
  ggplot2::ggtitle("Spatial block partitions: occurrences")

# PLotting the background shows that the background extent is partitioned in a way that maximizes evenness of points across the four bins, not to maximize evenness of area.
evalplot.grps(pts = bg, pts.grp = block$bg.grp, envs = raster(envs.bg[[1]])) + 
  ggplot2::ggtitle("Spatial block partitions: background")

#### Running ENMeval ####
# --------------------- #

maxL <- ENMevaluate(
    occs = occs,
    envs = bioclim_qc,
    bg = bg,
    algorithm = 'maxnet',
    partitions = 'block',
    tune.args = list(fc = "L", rm = 1:2)
    )
maxL
x11(); plot(maxL@predictions)

maxLQ <- ENMevaluate(
    occs = occs,
    envs = bioclim_qc,
    bg = bg,
    algorithm = 'maxnet',
    partitions = 'block',
    tune.args = list(fc = "LQ", rm = 1:2)
    )
maxLQ
plot(maxLQ@predictions)
class(max)

max2 <- ENMevaluate(
    occs = occs,
    envs = bioclim_qc,
    bg = bg,
    algorithm = 'maxnet',
    partitions = 'block',
    tune.args = list(fc = c("L","LQ"),
    rm = 1:3)
    )
