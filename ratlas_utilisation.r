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

# Nerodia & Gyrinophilus are duplicated

obs <- get_observations(
    id_taxa = taxa$id_taxa_obs
)
dim(obs)
class(obs)
names(obs)
summary(obs$within_quebec)

# obs du Qc
obs_qc <- obs[obs$within_quebec == TRUE,]
dim(obs_qc)

t <- cbind(obs_qc$taxa_vernacular_fr,
obs_qc$geom
)
class(t)
names(t) <- c("name", "geometry")

mapview::mapview(
    t,
    zcol = "name",
    burst = T
    )

