# Tentative utilisation des donnees presentes dans le catalogue IO
# Utilisation des packages rstac & gdalcubes
# Utilisation des fonctions et vignettes développées par Sarah Valentin
# Accessibles sur le github de Biodiversite Quebec dans le repertoire
# stac-catalogue

#### Packages ####
# ------------- #
source("/home/claire/BDQC-GEOBON/GITHUB/BDQC_SDMs/packages_n_data.r")
# devtools::install_github("ReseauBiodiversiteQuebec/stac-catalogue")
library("stacatalogue")

# The STAC currently contains the following collections:
stac_path <- "https://io.biodiversite-quebec.ca/stac/"
stac(stac_path) %>%
    collections() %>%
    get_request()

### 1. Defining the study extent

# Whatever the chosen method used, the study extent used to interact with the stac catalogue must be a bbox class object in a user-specified projection system. Let's choose a projection system for Québec.

proj_to <- "EPSG:6623"

# We can then create a bbox from available points (for instance, based on a set of observations), from a shapefile or directly from a vector of coordinates.

#### 1.1. From points

# Let's download [Glyptemys insculpta](https://en.wikipedia.org/wiki/Wood_turtle) observations from GBIF. We apply basic tests to ensure there are no 0, non-numeric and not available coordinates using ``CoordinateCleaner`` package.

obs <- rgbif::occ_data(
    scientificName = "Glyptemys insculpta",
    hasCoordinate = TRUE,
    limit = 10000
)$data

obs <- dplyr::select(
    obs,
    decimalLongitude,
    decimalLatitude
) %>%
    dplyr::rename(lon = decimalLongitude) %>%
    dplyr::rename(lat = decimalLatitude)
head(obs)

# We reproject the coordinates to a user-specified projection system and create the box.

proj_from <- "EPSG:4326" # initial observations projection system
buffer <- 0

# Reproject the obs to the data cube projection
obs_pts <-
    stacatalogue::project_coords(obs,
        lon = "lon",
        lat = "lat",
        proj_from = proj_from,
        proj_to = proj_to
    )

# Create the extent (data cube projection)
bbox <- stacatalogue::points_to_bbox(obs_pts,
    buffer = buffer
)
plot(bbox)
