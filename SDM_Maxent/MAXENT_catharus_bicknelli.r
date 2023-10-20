# Production de SDMs avec maxent sur les donnees du grive de Bickenell - Catharus bicknelli
# origine des donnees d'occurences - donnees utilisees par Vincent Bellavance dans le cadre de sa maitrise
# origine des donnees bioclimatiques - WorldClim - https://www.worldclim.org/data/bioclim.html
# package & methode utilises - ENMeval & maxent - vignette https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html

#### Packages ####
# ------------- #
source("packages_n_data.r")

#### Environmental data raster from Francois Rousseu ####
# ----------------------------------------------------- #
pred <- terra::rast("/home/claire/BDQC-GEOBON/data/predictors.tif")
names(pred)
x11()
plot(pred[[11]])

## --> keep tmean [1], prec [2], xxx_esa [39:51], elevation [20], truggedness [19]
pred2 <- subset(pred, c(1, 2, 19, 20, 39:51))
names(pred2)
pred2

#### species data ####
# ------------------ #

## --> data from Vincent Bellavance ms
obs_sf <- st_read("/home/claire/BDQC-GEOBON/data/Bellavance_data/sf_converted_occ_pres_only2/catharus_bicknelli.gpkg") # obs already during the breeding season

dim(obs_sf)
class(obs_sf)
head(obs_sf)
names(obs_sf)

# crs homogenization to raster CRS
obs_sf2 <- st_transform(obs_sf,
    crs = st_crs(pred2)
)
head(obs_sf2)

# keep only the converted coordinates and deletion of duplicated ones
coord <- as.data.frame(st_coordinates(obs_sf2))
occs <- coord[!duplicated(coord), ]
dim(occs)
dim(coord)

names(occs) <- c("lon", "lat")

#### Study extent ####
# ------------------ #

## --> Use extent of occs

range <- st_bbox(obs_sf2)
exten <- ext(
    range[1],
    range[3],
    range[2],
    range[4]
)
# Crop environmental rasters to match the study extent
# envs_bg <- crop(pred2, occs_buf)
envs_bg <- crop(pred2, exten)

# Tests
# Temperatures
x11()
plot(envs_bg[[1]], main = names(pred2)[1])
points(occs)

# Precipitations
plot(envs_bg[[2]], main = names(pred2)[2])
points(occs)

# Sample 10,000 random points (or whatever the desired number --> ****)
# only one per cell without replacement

bg <- raptr::randomPoints(
    envs_bg[[1]],
    n = 5000
) %>% as.data.frame()
head(bg)
colnames(bg) <- colnames(occs)

# or use of all occurrences in db

bg0 <- st_read("/home/claire/BDQC-GEOBON/data/Bellavance_data/total_occ_pres_only_versionR.gpkg",
    query = "SELECT geom FROM total_occ_pres_only_versionR ORDER BY random() LIMIT 10"
)

bg00 <- as.data.frame(st_coordinates(bg0))

bg <- bg00[!duplicated(bg00), ]

# sample_n(bg, 10)
colnames(bg) <- colnames(occs)

# Visualization
plot(envs_bg[[1]], main = names(pred2)[1])
points(occs)
# plot(occs_buf, border = "blue", lwd = 3, add = TRUE)
points(bg, col = "red")

#### Partitioning occurences for eval ####
# -------------------------------------- #
# allowing cross-validation
# choice of the partitioning method
# can be done manually with the partitioning function or automatically in ENMeval()

# for illustration, test of the block method
block <- get.block(occs, bg, orientation = "lat_lon")
# Let's make sure that we have an even number of occurrences in each partition.
table(block$occs.grp)
table(block$bg.grp)
# We can plot our partitions on one of our predictor variable rasters to visualize where they fall in space.
# The ENMeval 2.0 plotting functions use ggplot2 (Wickham 2016)
x11()
par(mfrow = c(1, 2))

evalplot.grps(pts = occs, pts.grp = block$occs.grp, envs = raster(envs_bg[[1]])) +
    ggplot2::ggtitle("Spatial block partitions: occurrences")

# PLotting the background shows that the background extent is partitioned in a way that maximizes evenness of points across the four bins, not to maximize evenness of area.

evalplot.grps(pts = bg, pts.grp = block$bg.grp, envs = raster(envs_bg[[1]])) +
    ggplot2::ggtitle("Spatial block partitions: background")

#### Running ENMeval ####
# --------------------- #

maxL <- ENMevaluate(
    occs = occs,
    envs = envs_bg,
    bg = bg,
    # algorithm = "maxnet",
    algorithm = "maxent.jar",
    partitions = "block",
    tune.args = list(fc = "L", rm = 1:2)
)
maxL
class(maxL)
# saveRDS(
#     maxL,
#     "/home/claire/BDQC-GEOBON/SDM_Maxent_results/catharus_bicknelli/catharus_bicknelli_L_1-2_QC-buffer.rds"
# )
x11()
plot(maxL@predictions)

maxLQ <- ENMevaluate(
    occs = occs,
    envs = envs_bg,
    bg = bg,
    # algorithm = "maxnet",
    algorithm = "maxent.jar",
    partitions = "block",
    tune.args = list(fc = "LQ", rm = 1:2)
)
maxLQ

# saveRDS(
#     maxLQ,
#     "/home/claire/BDQC-GEOBON/SDM_Maxent_results/catharus_bicknelli/catharus_bicknelli_LQ_1-2_QC-buffer.rds"
# )

x11()
plot(maxLQ@predictions)
# plot(maxLQ@predictions[[1]])
# plot(maxLQ@predictions[[2]])
