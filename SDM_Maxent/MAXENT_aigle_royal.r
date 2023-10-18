# Production de SDMs avec maxent sur les donnees d'aigle royal
# origine des donnees d'occurences - atlas
# origine des donnees bioclimatiques - WorldClim - https://www.worldclim.org/data/bioclim.html
# package & methode utilises - ENMeval & maxent - vignette https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html

#### Packages ####
# ------------- #
source("packages_n_data.r")


#### WorldClim data - bioclimatic data ####
# -------------------------------------- #
# ?geodata
# ?worldclim_country

# bioclim <- worldclim_country(
#    country = "Canada",
#    var = "bio",
#    #res = 2.5,
#    path = "." # failed when changing path
# ) # process time ~ 18 min - run the fonction in R cause pb with vsc.

# https://www.worldclim.org/data/bioclim.html
# bioc <- terra::rast("/home/claire/BDQC-GEOBON/data/bioclim_data/wc2.1_country/CAN_wc2.1_30s_bio.tif")
# names(bioc)

## --> keep annual mean temp (BIO1) & annual precipitation (BIO12)
# ----

# bioclim <- bioc[[names(bioc) %in% c("wc2.1_30s_bio_1", "wc2.1_30s_bio_12")]]
# x11()
# plot(bioclim)

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

## --> DL data from Atlas
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

## --> data from Vincent Bellavance ms
obs <- as.data.frame(readRDS("/home/claire/BDQC-GEOBON/data/EMV_occurences/BIRD_aigle_royal_occ.rds")) # obs already during the breeding season

dim(obs)
class(obs)
head(obs)
names(obs)

## --> occurrence treatment
# retrieve coordinates for conversion to sf object
obs1 <- cbind(obs, st_coordinates(obs$geom))
head(obs1)

# Conversion to sf object
obs_sf <- st_as_sf(obs1,
    coords = c("X", "Y"),
    remove = FALSE,
    crs = 4326
)
head(obs_sf)

# crs homogenization to raster CRS
obs_sf2 <- st_transform(obs_sf,
    crs = st_crs(pred2)
)
head(obs_sf2)

# keep only the converted coordinates and deletion of duplicated ones
coord <- as.data.frame(st_coordinates(obs_sf2))
occs <- coord[!duplicated(coord), ]
dim(occs)

names(occs) <- c("lon", "lat")

#### Specify study extent & sample random points ####
# ------------------------------------------------- #

class(occs)
head(occs)
occs_sf <- sf::st_as_sf(
    occs,
    coords = c("lon", "lat"),
    crs = terra::crs(pred2)
)

# Buffer all occurrences by xxx km (to cconfirm), union the polygons together (for visualization), and convert back to a form that the raster package can use. Finally, we reproject the buffers back to WGS84 (lat/lon).
occs_buf <- sf::st_buffer(occs_sf, dist = 250) %>% # 250 km
    sf::st_union() %>%
    sf::st_sf() %>%
    sf::st_transform(crs = st_crs(pred2))

# Check the result
plot(pred2[[1]], main = names(pred2)[1])
points(occs)
plot(occs_buf, border = "blue", lwd = 3, add = TRUE)


# Crop environmental rasters to match the study extent
envs_bg <- crop(pred2, occs_buf)
# Next, mask the rasters to the shape of the buffers
envs_bg <- mask(envs_bg, occs_buf)

# Tests
# Temperatures
plot(envs_bg[[1]], main = names(pred2)[1])
points(occs)
plot(occs_buf, border = "blue", lwd = 3, add = TRUE)

# Precipitations
plot(envs_bg[[2]], main = names(pred2)[2])
points(occs)
plot(occs_buf, border = "blue", lwd = 3, add = TRUE)


# Sample 10,000 random points (or whatever the desired number --> ****)
# only one per cell without replacement

# bg <- dismo::randomPoints(envs.bg[[9]], n = 10000) %>% as.data.frame()
bg <- raptr::randomPoints(
    envs_bg[[1]],
    n = 10000
) %>% as.data.frame()
head(bg)
colnames(bg) <- colnames(occs)

# Visualization
plot(envs_bg[[1]], main = names(pred2)[1])
points(occs)
plot(occs_buf, border = "blue", lwd = 3, add = TRUE)
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
    algorithm = "maxnet",
    partitions = "block",
    tune.args = list(fc = "L", rm = 1:2)
)
maxL
# x11(); plot(maxL@predictions)

# maxLQ <- ENMevaluate(
#     occs = occs,
#     envs = bioclim_qc,
#     bg = bg,
#     algorithm = 'maxnet',
#     partitions = 'block',
#     tune.args = list(fc = "LQ", rm = 1:2)
#     )
# maxLQ
# plot(maxLQ@predictions[[1]])
# plot(maxLQ@predictions[[2]])





































































































#### Temporal division - 5 ans ####
names(obsss)
table(obsss$year_obs, useNA = "always")
hist(obsss$year_obs)

# From 1981 to 2020
aig <- obsss[obsss$year_obs > 1980, ]
hist(aig$year_obs)

# Function for rounding years
round_to_5 <- function(numb) {
    new <- vector()

    for (i in numb) {
        last_val <- as.numeric(substr(i, nchar(i), nchar(i)))

        if (last_val %in% c(0, 5)) {
            new_val <- i
        } else if (last_val %in% 1:4) {
            new_val <- i + (5 - last_val)
        } else {
            new_val <- i + (10 - last_val)
        }

        new_val

        new <- c(new, new_val)
    }
    new
}

# Data treatment
aig$round_year <- round_to_5(aig$year_obs)
aig_ls <- split(aig, aig$round_year)
length(aig_ls)

aig_occs_ls <- lapply(aig_ls, function(x) {
    pts <- st_coordinates(x$geom) # retrieve the coordinates
    occs <- pts[!duplicated(pts), ] # deletion of duplicated coordinates

    occs <- as.data.frame(occs)
    names(occs) <- c("lon", "lat")
    occs
})

aig_UTM_ls <- lapply(aig_ls, function(x) {
    pts <- st_coordinates(x$geom) # retrieve the coordinates
    occs <- pts[!duplicated(pts), ] # deletion of duplicated coordinates

    occs <- as.data.frame(occs)
    names(occs) <- c("lon", "lat")

    occs_sf <- sf::st_as_sf(
        occs,
        coords = c("lon", "lat"),
        crs = terra::crs(bioclim_qc)
    )

    occs_sf_utm <- sf::st_transform(
        occs_sf,
        st_crs("+init=epsg:2031") # conversion en UTM pour la creation d'un buffer
    )
})

aig_buf_ls <- lapply(aig_UTM_ls, function(x) {
    occs.buf <- sf::st_buffer(x, dist = 250000) %>% # 250 km
        sf::st_union() %>%
        sf::st_sf() %>%
        sf::st_transform(crs = raster::crs(bioclim_qc))

    # Crop environmental rasters to match the study extent
    envs.bg <- raster::crop(bioclim_qc, occs.buf)
    # Next, mask the rasters to the shape of the buffers
    envs.bg <- raster::mask(envs.bg, occs.buf)
})


# Random points

aig_rand_pts <- list()

for (i in seq_along(aig_buf_ls)) {
    bg <- raptr::randomPoints(
        aig_buf_ls[[i]][[1]],
        n = 5000
    ) %>% as.data.frame()

    colnames(bg) <- c("lon", "lat")

    aig_rand_pts[[i]] <- bg
}
names(aig_rand_pts) <- names(aig_buf_ls)

plot(aig_rand_pts[[1]])


# Modelling
sdm_aig <- list()

for (i in seq_along(aig_rand_pts)) {
    print(paste("MODEL # ", i))

    mod <- ENMevaluate(
        occs = aig_occs_ls[[i]],
        envs = bioclim_qc,
        bg = aig_rand_pts[[i]],
        algorithm = "maxnet",
        partitions = "block",
        tune.args = list(fc = "L", rm = 1:2)
    )

    sdm_aig[[i]] <- mod
}

names(sdm_aig) <- names(aig_rand_pts)

x11()
par(mfrow = c(3, 3))
for (i in 1:8) {
    plot(sdm_aig[[i]]@predictions[[1]])
}

x11()
par(mfrow = c(3, 3))
for (i in 1:8) {
    plot(sdm_aig[[i]]@predictions[[2]])
}

saveRDS(
    sdm_aig,
    "/home/claire/BDQC-GEOBON/data/Simple_niche_clim/BIRD_aigle_royal/BIRD_aigle_royal_5.rds"
)

#### Surface niches climatiques - 5 years ####
# ------------------------------------------ #
aig_5 <- readRDS("/home/claire/BDQC-GEOBON/data/Simple_niche_clim/BIRD_aigle_royal/BIRD_aigle_royal_5.rds")

aig_5[[1]]
aig_5[[1]]@predictions
plot(aig_5[[1]]@predictions[[1]])

# Visualisation with occurence prob = 0.5
# ------------------------------------- #
x11()
par(mfrow = c(3, 3))

for (i in 1:8) {
    test <- aig_5[[i]]@predictions[[1]]
    values(test)[values(test) >= 0.5] <- 1
    values(test)[values(test) < 0.5] <- 0
    plot(test)
}

size_niche_5 <- lapply(aig_5, function(x) {
    test <- x@predictions[[1]]
    values(test)[values(test) >= 0.5] <- 1
    values(test)[values(test) < 0.5] <- 0

    sum(values(test), na.rm = T)
})
size_niche_5

# Visualisation with occurence prob = 0.8
# ------------------------------------- #
x11()
par(mfrow = c(3, 3))

for (i in 1:8) {
    test <- aig_5[[i]]@predictions[[1]]
    values(test)[values(test) >= 0.8] <- 1
    values(test)[values(test) < 0.8] <- 0
    plot(test)
}

size_niche_8 <- lapply(aig_5, function(x) {
    test <- x@predictions[[1]]
    values(test)[values(test) >= 0.8] <- 1
    values(test)[values(test) < 0.8] <- 0

    sum(values(test), na.rm = T)
})
size_niche_8

# Visualisation with occurence prob = 0.4
# ------------------------------------- #
x11()
par(mfrow = c(3, 3))

for (i in 1:8) {
    test <- aig_5[[i]]@predictions[[1]]
    values(test)[values(test) >= 0.4] <- 1
    values(test)[values(test) < 0.4] <- 0
    plot(test)
}

size_niche_4 <- lapply(aig_5, function(x) {
    test <- x@predictions[[1]]
    values(test)[values(test) >= 0.4] <- 1
    values(test)[values(test) < 0.4] <- 0

    sum(values(test), na.rm = T)
})
size_niche_4

# Graphical visualisation
# ---------------------- #
x11()
par(mfrow = c(3, 1))
for (i in c(
    size_niche_4,
    size_niche_5,
    size_niche_8
)) {
    df <- as.data.frame(unlist(i))
    df$year <- row.names(df)
    names(df) <- c("pixel_numb", "year")

    print(plot(df$year,
        df$pixel_numb,
        type = "l",
        bty = "n"
    ))
} # fonctionne pas dans la loop mais en manuel oui


# -------------- #
# -------------- #
# IN THE FUTURE #
# -------------- #
# -------------- #

tif_temp <- list.files("/home/claire/BDQC-GEOBON/data/bioclim_data/QC_Moyenne-annuelle-des-temperatures_spatial",
    all.files = TRUE,
    full.names = T,
    pattern = ".tif"
)

tif_prec <- list.files("/home/claire/BDQC-GEOBON/data/bioclim_data/QC_Total-annuel-des-precipitations_spatial",
    all.files = TRUE,
    full.names = T,
    pattern = ".tif"
)

# scenario 2021-2050 - SSP3-7.0 - pecrcentile 50
scenar <- terra::rast(c(
    tif_temp[78],
    tif_prec[78]
))

names(scenar) <- names(bioclim_qc)

prov <- terra::readRDS("/home/claire/BDQC-GEOBON/data/gadm/gadm41_CAN_1_pk.rds")

x11()
plot(scenar)
plot(prov, add = T, border = "darkgrey")

summary(values(scenar))

length(aig_5)
aig_5[[8]]@predictions[[1]]
pred_test <- dismo::predict(aig_5[[8]], scenar)


bioc <- bioclim

bioc_qc <- crop(
    bioc,
    ext(scenar)
)

names(aig_5[[8]]@models)
str(aig_5[[8]]@models)

# https://www.rdocumentation.org/packages/ENMeval/versions/0.3.1/topics/ENMevaluate
e <- ENMevaluate(...)
e@predictions[[1]] # raw output
p <- predict(e@models[[1]], envs)
p # logistic output

# see also : https://rsh249.github.io/bioinformatics/ENMeval.html
