
########## Polatouche - Glaucomys volans ##########
#### Load packages ####
# ------------------- #
source("/home/claire/BDQC-GEOBON/GITHUB/BDQC_SDMs/packages_n_data.r")


#### species data ####
# ------------------ #
## --> DL data 
# obs <- get_EMV_qc("Glaucomys volans"); dim(obs)
# saveRDS(obs, "/home/claire/BDQC-GEOBON/data/occurences/MAMM_polatouche_occ.rds")

obs <- readRDS("/home/claire/BDQC-GEOBON/data/occurences/MAMM_polatouche_occ.rds")
dim(obs)
table(obs$year_obs)

#### WorldClim data - bioclimatic data (temp & prec) ####
# -------------------------------------- #
# --> visualisation
plot(bioclim_qc[[1]])
plot(prov, add = T, border = "grey")
plot(obs$geom, add = T)

#### 1 model ####
# ------------ #

obs_pts <- st_coordinates(obs$geom)
occs <- as.data.frame(obs_pts[!duplicated(obs_pts), ])
dim(occs); class(occs)
names(occs) <- c("lon", "lat")

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

# --> Buffer all occurrences by 250 km 
occs_buf <- sf::st_buffer(occs_sf_utm, dist = 250000) %>% # 250 km
  sf::st_union() %>% 
  sf::st_sf() %>%
  sf::st_transform(crs = raster::crs(bioclim_qc))

# --> visualization
plot(bioclim_qc[[1]], main = names(bioclim_qc)[1])
points(occs)
# To add sf objects to a plot, use add = TRUE
plot(occs_buf, border = "blue", lwd = 3, add = TRUE)


# --> Crop environmental rasters to match the study extent
envs_bg <- raster::crop(bioclim_qc, occs_buf)
# --> Next, mask the rasters to the shape of the buffers
envs_bg <- raster::mask(envs_bg, occs_buf)

# Temperatures
plot(envs_bg[[1]], main = names(bioclim_qc)[1])
points(occs)
plot(occs_buf, border = "blue", lwd = 3, add = TRUE)

# Precipitations
plot(envs_bg[[2]], main = names(bioclim_qc)[2])
points(occs)
plot(occs_buf, border = "blue", lwd = 3, add = TRUE)

# --> Sample 10,000 random points (or whatever the desired number --> ****), only one per cell without replacement

bg <- raptr::randomPoints(
    envs_bg[[1]],
    n = 10000
) %>% as.data.frame()
head(bg)
colnames(bg) <- colnames(occs)

# --> visualization
plot(envs_bg[[1]])
points(bg, pch = 20, cex = 0.2)

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
x11(); plot(maxL@predictions[[1]])
points(occs)
