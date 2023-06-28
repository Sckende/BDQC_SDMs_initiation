# Production de SDMs avec maxent sur les donnees d'aigle royal
# origine des donnees d'occurences - atlas
# origine des donnees bioclimatiques - WorldClim - https://www.worldclim.org/data/bioclim.html
# package & methode utilises - ENMeval & maxent - vignette https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html

#### Packages ####
library(ratlas)
library(sf)
library(mapview)
library(geodata)
library(geojsonsf)
library(lubridate)

#### species data ####
## --> DL data 
taxa <- get_taxa(
    scientific_name = "Aquila chrysaetos"
    )
taxa # 10038

obs <- get_observations(
    id_taxa = 10038
)
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
?geodata
?worldclim_country

#bioclim <- worldclim_country(
#    country = "Canada",
#    var = "bio",
#    #res = 2.5,
#    path = "." # failed when changing path
#) # process time ~ 18 min - run the fonction in R cause pb with vsc.

bioc <- terra::rast("/home/claire/BDQC-GEOBON/data/bioclim_data/wc2.1_country/CAN_wc2.1_30s_bio.tif")
names(bioc)

## --> keep annual mean temp (BIO1) & annual precipitation (BIO12)
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
plot(bioclim_qc[[1]])
points(occs, pch = 20, alpha = 0.5)

## --> add province delimitation

#prov <- gadm(country = "CAN",
#level = 1,
#path = ".")

prov <- terra::readRDS("/home/claire/BDQC-GEOBON/data/gadm/gadm41_CAN_1_pk.rds")

plot(prov, add = T)


