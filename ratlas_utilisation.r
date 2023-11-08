# general documentation: https://reseaubiodiversitequebec.github.io/ratlas/
# get_obs() documentation: https://reseaubiodiversitequebec.github.io/ratlas/articles/download-obs.html

# devtools::install_github("ReseauBiodiversiteQuebec/ratlas")
# file.edit("~/.Renviron")

library(ratlas)
library(sf)

taxa <- get_taxa(scientific_name = "Canis lupus")
obs <- get_observations(id_taxa = taxa$id_taxa_obs[1])
dim(obs)
obs
class(obs)
head(obs)

# Returns all observations from 2010 to 2015 for taxa "Leuconotopicus villosus" (`id_taxa` = 6450) from ebird datasets (`id_datasets` = 55:102)

taxa <- get_taxa(scientific_name = "Leuconotopicus villosus") # 8354, 6450

obs2 <- get_observations(
    id_taxa = c(6450, 8354),
    year = 2010:2015,
    id_datasets = 55:102
)

dim(obs2)


obs3 <- get_observations(id_taxa = 8354)
head(obs3)
dim(obs3)

#### Oiseaux ####
# aigle royal - Aquila chrysaetos (QC: vulnérable; CA: s.o.)
# grive de Bicknell - Catharus bicknelli (QC: vulnérable; CA: menacée)
# paruline azurée - Septophaga cerulea (QC: menacée; CA: en voie de disparition)
# petit blongios - Ixobrychus exilis (QC: vulnérable; CA: menacée)
# râle jaune - Coturnicops noveboracensis (QC: menacée; CA: préoccupante)

#### Reptiles ####
# couleuvre d’eau - Nerodia sipedon (QC: aucun; CA: s.o.)
# couleuvre tacheté - Lampropeltis triangulum triangulum (QC: aucun; CA: préoccupante)
# rainette faux-grillon de l’ouest - Pseudacris triseriata (QC: vulnérable; CA: menacée)
# salamandre pourpre - Gyrinophilus porphyriticus (QC: vulnérable; CA: menacée)
# salamandre sombre des montagnes - Desmognathus ochrophaeus (QC: menacée; CA: en voie de disparition)
# salamandre à quatre orteils - Hemidactylium scutatum (QC: susceptible d’être désignée; CA: s.o.)
# tortues des bois - Glyptemys insculpta (QC: vulnérable; CA: menacée)
# tortue mouchetée - Emydoidea blandingii (QC: menacée; CA: en voie de disparition)
#### Mammifère ####
# petit polatouche - Glaucomys volans (QC: susceptible d’être désigné; CA: préoccupante)

library(ratlas)
library(sf)

emv <- c(
    "Aquila chrysaetos",
    "Catharus bicknelli",
    "Setophaga cerulea", 
    "Ixobrychus exilis",
    "Coturnicops noveboracensis",
    "Nerodia sipedon",
    "Lampropeltis triangulum", # triangulum",
    "Pseudacris triseriata",
    "Gyrinophilus porphyriticus",
    "Desmognathus ochrophaeus",
    "Hemidactylium scutatum",
    "Glyptemys insculpta",
    "Emydoidea blandingii",
    "Glaucomys volans"
)
taxa <- get_taxa(scientific_name = emv)
taxa$observed_scientific_name
taxa$id_taxa_obs
taxa$valid_scientific_name

# Nerodia & Gyrinophilus are duplicated

obs <- get_observations(
    id_taxa = taxa$id_taxa_obs
)
dim(obs)
head(obs)
class(obs)
names(obs)
summary(obs$within_quebec)

# obs du Qc
obs_qc <- obs[obs$within_quebec == TRUE,]
dim(obs_qc)
table(
    obs_qc$taxa_valid_scientific_name,
    useNA = "always"
    )

# sf object
qc_sf <- cbind(
    obs_qc$taxa_valid_scientific_name,
    obs_qc$geom
)
class(qc_sf)
names(qc_sf) <- c("name", "geometry")
table(
    qc_sf$name,
    useNA = "always"
)

mapview::mapview(
    qc_sf,
    zcol = "name",
    burst = T
    )

#### Explo origine data ####
# ------------------------ #

obs_ls <- split(
    obs_qc,
    obs_qc$taxa_valid_scientific_name
)
str(obs_ls, 2)
lapply(
    obs_ls,
    function(x){
        #able(x$dataset, useNA = "always")
        table(x$source, useNA = "always")
    }
    )



reg <- get_regions(type = "hex", within_quebec = TRUE, scale = 250)





names(reg)
x11(); plot(reg)
class(reg)
st_as_sf(reg, coords = "geometry")
reg$geom[[1]]
str(reg$geom)

str(reg)
reg$geom$crs
plot(reg$geom$coordinates)

get_observations()

get_taxa(scientific_name = "Antigone canadensis")




#### Atlas local ####
atlas_local <- function(parquet_file,
                        tblname = "atlas") {
  requireNamespace("duckdbfs")
  atlas <- duckdbfs::open_dataset(parquet_file, tblname = tblname)
  atlas
}


# libraries
library(ratlas)
library(dplyr)
library(sf)

# Connect to atlas locally
atlas <- atlas_local(parquet_file = "/home/claire/BDQC-GEOBON/geoparquet-test/atlas.parquet", "atlas") # /home/claire/BDQC-GEOBON/geoparquet-test/atlas.parquet

# Get the polygons of interest (within quebec, scale 250)
# and convert the object to sf
regions_id <- ratlas::get_regions(scale = 250, type = "hex", within_quebec = TRUE) |>
              sf::st_as_sf("geometry")

# Get the observations
data <- atlas |>
        filter(valid_scientific_name == "Antigone canadensis") |>
        collect() |>
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 
        
        test <- st_intersection(data, regions_id[1,])
        test <- st_intersection(data, regions_id[2,])

        for (i in )

        
        
        |>
        sf::st_join(regions_id, join = st_within)


x11(); plot(st_geometry(data))
mapview::mapview(data)

# create square
s <- rbind(c(1, 1), c(10, 1), c(10, 10), c(1, 10), c(1, 1)) %>% list %>% st_polygon %>% st_sfc
# create random points
p <- runif(50, 0, 11) %>% cbind(runif(50, 0, 11)) %>% st_multipoint %>% st_sfc %>% st_cast("POINT")

# intersect points and square with st_intersection
st_intersection(p, s)

st_as_text(st_geometry(regions_id[1, ]))
