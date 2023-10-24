library(gdalcubes)
library(rstac)
library(sf)

# cf infos ==> https://github.com/brazil-data-cube/rstac

# Connect to the STAC catalog
s_obj_acer <- stac("https://acer.biodiversite-quebec.ca/stac/")
get_request(s_obj_acer)

# Show the collection "oiseaux-nicheurs-qc"
obj <- s_obj_acer %>%
    stac_search(collections = "oiseaux-nicheurs-qc") %>%
    get_request()

# obj %>% items_matched()
# obj %>% items_length()

## See item properties of first item
obj[["features"]][[1]]$properties

## DL on local one item
# dl_ass <- obj %>% assets_download(assets_name = "zonotrichia_leucophrys_range_2017")


## Get one item and send it to STARS
library(stars) # good complement of TERRA packages for manipulating raster
lc1 <- read_stars(paste0("/vsicurl/", obj[["features"]][[2]]$assets$data$href),
    proxy = TRUE
) # by putting "/vsicurl/" and proxy = T it's just using the web, we avoid to read the raster on the computer memory => really faster !
plot(lc1)


input <- "acanthis_flammea"

id_feat <- paste(input, "_range_2017", sep = "")


acan <- stac("https://acer.biodiversite-quebec.ca/stac/") %>%
    collections("oiseaux-nicheurs-qc") %>%
    items(feature_id = id_feat) %>%
    get_request()

tif <- acan$assets$data$href

lc1 <- stars::read_stars(paste0("/vsicurl/", tif),
    proxy = TRUE
)
plot(lc1, axes = T)
