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

#### Functions ####

get_EMV_qc <- function(scientific_name) {

    taxa <- get_taxa(scientific_name = scientific_name)

    obs <- get_observations(id_taxa = taxa$id_taxa_obs)

    obs_qc <- obs[obs$within_quebec == TRUE, ]
    obs_qc
}

#### data ####

# Bioclimatic data
bioc <- terra::rast("/home/claire/BDQC-GEOBON/data/bioclim_data/wc2.1_country/CAN_wc2.1_30s_bio.tif")
bioclim <- bioc[[names(bioc) %in% c("wc2.1_30s_bio_1", "wc2.1_30s_bio_12")]] # --> keep annual mean temp (BIO1) & annual precipitation (BIO12)
## --> keep Quebec only
rang <- ext(
    -90,
    -50,
    35,
    70) # min/max lon, min/max lat - definition of SpatExtent
bioclim_qc <- crop(
    bioclim,
    rang)

