# general documentation: https://reseaubiodiversitequebec.github.io/ratlas/
# get_obs() documentation: https://reseaubiodiversitequebec.github.io/ratlas/articles/download-obs.html

# devtools::install_github("ReseauBiodiversiteQuebec/ratlas")
# file.edit("~/.Renviron")

# ------------------------------ #
#### Request to Atlas online ####
# ---------------------------- #

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
obs_qc <- obs[obs$within_quebec == TRUE, ]
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
    function(x) {
        # able(x$dataset, useNA = "always")
        table(x$source, useNA = "always")
    }
)



reg <- get_regions(type = "hex", within_quebec = TRUE, scale = 250)





names(reg)
x11()
plot(reg)
class(reg)
st_as_sf(reg, coords = "geometry")
reg$geom[[1]]
str(reg$geom)

str(reg)
reg$geom$crs
plot(reg$geom$coordinates)

get_observations()

get_taxa(scientific_name = "Antigone canadensis")



# --------------------------------------- #
#### Local Atlas in geoparquet format ####
# ------------------------------------- #
# cf vignette - https://reseaubiodiversitequebec.github.io/BQ_DOCS/Geoparquet_Atlas_Intro.html

library(dplyr)
library(duckdb)
library(duckdbfs)
library(dbplyr)
library(sf)
library(mapview)
library(terra)

base_url <- "https://object-arbutus.cloud.computecanada.ca/bq-io/atlas/parquet/"

atlas_local <- function(parquet_date, destination_folder, tblname = "atlas") {
    file_name <- paste0("atlas_public_", parquet_date, ".parquet")
    url <- paste0(base_url, file_name)
    dest <- paste0(destination_folder, "/", file_name)
    download.file(url, dest)
    requireNamespace("duckdbfs")
    duckdbfs::open_dataset(dest, tblname = tblname)
}

atlas_remote <- function(parquet_date, tblname = "atlas") {
    file_name <- paste0("atlas_public_", parquet_date, ".parquet")
    requireNamespace("duckdbfs")
    duckdbfs::open_dataset(paste0(base_url, file_name), tblname = tblname)
}

atlas_dates <- read.csv(paste0(base_url, "atlas_export_dates.csv"), header = FALSE, col.names = c("dates"))

# download Atlas locally
# options(timeout=500)
# atlas <- atlas_local(tail(atlas_dates$dates, n = 1),
#                      '/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/')

# Use Atlas locally
dest <- "/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/atlas_public_2024-04-02.parquet"
tblname <- "atlas"

# Open the database
atlas <- duckdbfs::open_dataset(dest, tblname = tblname)

# Subset of species
tree_spe <- c(
    "Abies balsamea",
    "Acer rubrum",
    "Acer saccharum",
    "Betula alleghaniensis",
    "Betula papyrifera",
    "Fagus grandifolia",
    "Larix laricina",
    "Picea glauca",
    "Picea mariana",
    "Picea rubens",
    "Pinus banksiana",
    "Pinus resinosa",
    "Pinus strobus",
    "Populus tremuloides",
    "Quercus rubra",
    "Tsuga canadensis",
    "Thuja occidentalis"
)

tree_db <- atlas |>
    filter(valid_scientific_name %in% tree_spe) |>
    collect()
class(tree_db)
names(tree_db)
unique(tree_db$valid_scientific_name)

tree_db$observation_value <- as.numeric(tree_db$observation_value)
summary(tree_db$observation_value)
View(tree_db[tree_db$observation_value > 1000, ])

tree_db |>
    group_by(valid_scientific_name) |>
    summarize(abundance = sum(observation_value)) |>
    arrange(desc(abundance))

tree_db |>
    group_by(valid_scientific_name) |>
    summarize(n_row = n()) |>
    arrange(desc(n_row))

tree_db |>
    group_by(dataset_name) |>
    summarize(total = n())
View(dataset)
dim(dataset)

# Conversion to sf object
tree_sf <- st_as_sf(tree_db,
    coords = c("longitude", "latitude"),
    crs = 4326
)
mapview(tree_sf)

# conversion to environmental raster crs ==>
# Split per species for creating one gpkg per species

env <- rast("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/CROPPED_predictors.tif")
env
plot(env[[1]])
st_crs(env) == st_crs(tree_sf)

tree_sf2 <- st_transform(tree_sf,
    crs = st_crs(env)
)
plot(tree_sf2, add = T)

# Creation of the geopackage
st_write(tree_sf2,
    "/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/Boulanger_tree_species/Boulanger_tree_species.gpkg",
    append = T
)

# Save one file per species
new_tree <- st_read("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/Boulanger_tree_species/Boulanger_tree_species.gpkg")

tree_ls <- split(new_tree, new_tree$valid_scientific_name)
lapply(tree_ls, function(x) {
    sp <- gsub(" ", "_", tolower(unique(x$valid_scientific_name)))
    st_write(
        x,
        paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/Boulanger_tree_species/", sp, ".gpkg"),
        append = T
    )
})

# Index creation for faster queries
system("bash /home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/GITHUB/BDQC_SDMs_initiation/Loop_for_indexation_gpkg.sh")

# Verification des indexations
system("ogrinfo -sql 'PRAGMA index_list('Acer_saccharum')' /home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/Boulanger_tree_species/acer_saccharum.gpkg")
