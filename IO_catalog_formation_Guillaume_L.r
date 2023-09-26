#Load required Packages

#remotes::install_git("https://github.com/appelmar/gdalcubes_R")
library(gdalcubes)
library(rstac)
library(sf)

##Connect to STAC catalog

s_obj <- stac("https://io.biodiversite-quebec.ca/stac/") # the pointer for where the stac catalogue is


##List collections
collections <- s_obj %>% collections() %>% get_request() # get the list of collections


##Show collections and descriptions
df <- data.frame(id = character(),
                title = character(),
                description = character())
for (c in collections[['collections']]){
  df <- rbind(df,
              data.frame(id = c$id,
                         title = c$title,
                         description = c$description))
}
df # table with collection names and their description

##Search for a specific collection 

it_obj <- s_obj %>%
  stac_search(collections = "colombia-lc", limit = 100) %>% # *** see the arguments of this function AND by default, limitations in the number of files send (= 10) => see limit argument
  post_request() %>% items_fetch()
it_obj


#Alternatively
it_obj <- s_obj %>%
  collections("colombia-lc") %>% items() %>%
  get_request() %>% items_fetch()
it_obj



##See item properties of first item
it_obj[['features']][[1]]$properties   # datetime amd projection are always necessary => good process to verify before use the item => time dimension is ESSENTIAL

##Summarize items
#=> another way to check if all is fine with items
df <- data.frame(id = character(),
                datetime = character(),
                description = character())
for (f in it_obj[['features']]){
  df <- rbind(df,
              data.frame(id = f$id,
                        datetime = f$properties$datetime,
                        description = f$properties$description))
}
df


##Get one item and send it to STARS

library(stars) # good complement of TERRA packages for manipulating raster
lc1 <- read_stars(paste0('/vsicurl/',it_obj[['features']][[4]]$assets$data$href),
                  proxy = TRUE) # by putting "/vsicurl/" and proxy = T it's just using the web, we avoid to read the raster on the computer memory => really faster !
plot(lc1)

##Crop just a part of the raster
bbox <- st_bbox(c(xmin = 928986,
                xmax = 1074396,
                ymax = 914365,
                ymin = 793894),
                crs = st_crs(3116))
cropped <- lc1 %>% st_crop(bbox)


##Plot it
plot(cropped)


##Save as COG, using parameters appropriate for a categorical raster. 
cropped <- st_crop(bbox) %>% write_stars('/home/glaroc/lc1.tif', driver = 'COG', RasterIO = c('resampling' = 'mode'), options = c('COMPRESS=DEFLATE', 'OVERVIEW_RESAMPLING=MODE', 'LEVEL=6', 'OVERVIEW_COUNT=8', 'RESAMPLING=MODE', 'WARP_RESAMPLING=MODE', 'OVERVIEWS=IGNORE_EXISTING'))


##This is a fix to add the data role to the assets. This is apparently needed for gdalcubes to work properly in newer versions

for (i in seq_along(it_obj$features)){
  it_obj$features[[i]]$assets$data$roles = 'data'
}


##Filter assets by properties and create collection in order to use gdalcube

st <- stac_image_collection(it_obj$features, asset_names = c('data'), property_filter = function(f){f[['year']] == '2010-2012'}, srs = 'EPSG:3116')
st



##CUBE 
#Build a cube for processing or viewing data. Note that this cube can be in a different CRS and resolution as the original items/files. However, the time dimension has to capture the time frame of the item. dt is expressed as a time period. P1D is a period of 1 day, P1M is a period of 1 month, P1Y is a period of one year. Resampling methods have to fit with the data type. For categorical data, use "mode" or "nearest". For continuous data, use "bilinear". Aggregation is only relevant when multiple rasters overlap. 

v <- cube_view(srs = "EPSG:3116",
              extent = list(t0 = "2010-01-01", # the timing start of the cube
                            t1 = "2010-01-01", # the timing end of the cube
                            left = bbox$xmin,
                            right = bbox$xmax,
                            top = bbox$ymax,
                            bottom = bbox$ymin),
               dx = 30, #resolution that I want BUT if I change this, I have to put a resmpling method
               dy = 30,
               dt = "P1D", # time resolution - P1D = Period of 1 Day
               aggregation = "first",
               resampling = "mode") # parameters for what I want to see ! Be carrefull that the bonding box is in the same CRS of data


#Match the collection and the cube view to build a raster cube. 

lc_cube <- raster_cube(st, v)


#For some odd reason, this comes out blank. Seems to be an issue with the tif files or the 3116 CRS. 

lc_cube %>% plot(zlim=c(0,325000))


#Save as tif

lc_cube %>% write_tif('~/', prefix = 'lc2', creation_options = c('COMPRESS=DEFLATE'))



##Use the Accessibility from cities dataset, but keep the same CRS and extent. Note that we need to adjust the times to match the one from the STAC item. 
it_obj <- s_obj %>%
  collections("accessibility_to_cities") %>% items() %>%
  get_request() %>% items_fetch()

v <- cube_view(srs = "EPSG:3116", extent = list(t0 = "2015-01-01", t1 = "2015-01-01",
                                                left = bbox$xmin, right = bbox$xmax, top = bbox$ymax, bottom = bbox$ymin),
               dx = 5000, dy = 5000, dt = "P1D",
               aggregation = "mean",
               resampling = "bilinear")
for (i in seq_along(it_obj$features)){
  it_obj$features[[i]]$assets$data$roles = 'data'
} # for fixing the rows properties

# anames<-c()
# for(i in it_obj$features){
#   anames<-c(anames,names(i$assets))
# }

st <- stac_image_collection(it_obj$features, asset_names = "data")
lc_cube <- raster_cube(st, v)
lc_cube %>% plot(col=heat.colors)



##Use the CHELSA monthly dataset, and create a map of the average for the months on June, July and August from 2010 to 2020
it_obj <- s_obj %>%
  stac_search(collections = "chelsa-monthly", datetime = "2010-06-01T00:00:00Z/2020-08-01T00:00:00Z",limit = 5000) %>% post_request() %>% items_fetch()
for (i in seq_along(it_obj$features)){
  it_obj$features[[i]]$assets[[1]]$roles = 'data'
}

anames <- c()
for(i in it_obj$features){
  anames <- c(anames, names(i$assets))
}

st <- stac_image_collection(it_obj$features, asset_names = anames, property_filter = function(f){f[['variable']] == 'tas' && f[['month']] %in% c(6,7,8)})

v <- cube_view(srs = "EPSG:3116", extent = list(t0 = "2010-06-01", t1 = "2020-08-31",
                                                left = bbox$xmin, right = bbox$xmax, top = bbox$ymax, bottom = bbox$ymin),
               dx = 1000, dy = 1000, dt = "P1M",
               aggregation = "mean",
               resampling = "bilinear")

lc_cube <- raster_cube(st, v)

#Rename all bands to 'data' to simplify the process
ll <- list()
for (n in names(lc_cube)){
  ll[[n]] = 'data'
}
lc_cube2 <- do.call(rename_bands, c(list(lc_cube), ll)) 

# average of 123 time levels of the cube
lc_cube2 %>% reduce_time(c("mean(data)")) %>% plot(col = heat.colors)



##Use the ESA Land cover data and create a map at 5 km resolution showing the proportion of forests within each pixel

it_obj <- s_obj %>%
  stac_search(collections = "esacci-lc", limit = 5000) %>% post_request() %>% items_fetch()
for (i in seq_along(it_obj$features)){
  it_obj$features[[i]]$assets[[1]]$roles = 'data'
}

anames <- c()
for(i in it_obj$features){
  anames <- c(anames, names(i$assets))
}

st <- stac_image_collection(it_obj$features, asset_names = anames, property_filter = function(f){f[['year']] == 2010})


v <- cube_view(srs = "EPSG:3116", extent = list(t0 = "2010-06-01", t1 = "2020-08-31",
                                                left = bbox$xmin, right = bbox$xmax, top = bbox$ymax, bottom = bbox$ymin),
               dx = 1000, dy = 1000, dt = "P1M",
               aggregation = "first",
               resampling = "mode")

lc_cube <- raster_cube(st, v)

#Rename all bands to 'data' to simplify the process
ll <- list()
for (n in names(lc_cube)){
  ll[[n]] = 'data'
}
lc_cube2 <- do.call(rename_bands, c(list(lc_cube), ll)) 

#This might crash
lc_cube2 %>% apply_pixel('1*(data==10)') %>% aggregate_space(method = "mean", dx = 5000, dy = 5000) %>% plot(col = heat.colors)