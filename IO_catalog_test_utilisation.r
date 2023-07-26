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
    get_request() %>%
    print(n = Inf) # to display complete tibble object

### 1. Defining the study extent

# Whatever the chosen method used, the study extent used to interact with the stac catalogue must be a bbox class object in a user-specified projection system. Let's choose a projection system for Québec.

proj_to <- "EPSG:6623"

# We can then create a bbox from available points (for instance, based on a set of observations), from a shapefile or directly from a vector of coordinates.

#### 1.1. From points

# Let's download [Aquila chrysaetos]observations from GBIF. We apply basic tests to ensure there are no 0, non-numeric and not available coordinates using ``CoordinateCleaner`` package.

obs <- rgbif::occ_data(
    scientificName = "Aquila chrysaetos",
    hasCoordinate = TRUE,
    limit = 10000
)$data
names(obs)

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
    stacatalogue::project_coords(
        obs,
        lon = "lon",
        lat = "lat",
        proj_from = proj_from,
        proj_to = proj_to
    )
class(obs_pts)

# Create the extent (data cube projection)
bbox <- stacatalogue::points_to_bbox(obs_pts,
    buffer = buffer
)
plot(bbox)
plot(obs_pts)

#Now, you have to select the variables that you are going to load from a specific collection. Each collection has two types of fields: layers and variables. To understand the difference between the two concepts, let's take the example of the collection ``chelsa_monthly``. We retrieve the list of layers and variables names using the function ``get_info_collection``.

info_chelsa_monthly <- get_info_collection(
                                stac_path =
                                  "http://io.biodiversite-quebec.ca/stac/",
                                collections = c('chelsa-monthly'),
                                bbox = NULL)
names(info_chelsa_monthly)
head(info_chelsa_monthly$layers)
info_chelsa_monthly$variables

#Variables corresponds to each unique variable your collection contains. Each variable can be derived in several layers if you have a temporal series, as in ``chelsa_monthly`` (one layer per month, per year and per variable). If the collection does not include any dimension except the variables, the list of variables and layers will be the same:

info_chelsa <- get_info_collection(
                                stac_path =
                                  "http://io.biodiversite-quebec.ca/stac/",
                                collections = c('chelsa-clim'),
                                bbox = NULL)
print(info_chelsa$layers)
print(info_chelsa$variables)

#Searching using the variables names is the more practical way as you don't need to know the nomenclature of the layer names. 

### Selecting cube parameters

#Eventually, you have to chose the parameters of the cube you want to import, which correspond to common parameter of raster processing.

#**Spatial resolution **

#Spatial resolution corresponds to the size of pixels, in the unit of the chosen spatial reference system. The native resolution of a collection can be accessed with ``get_info_collection`` in the collection reference system unit (here in degree for 'chelsa-clim' collection, corresponding to approximately 1000 meters):

print(info_chelsa$spatial_resolution)

#**Aggregation/resampling **

#You have to select the method used to process the cube values when downscaling/upscaling the spatial resolution.

#Aggregation is the method applied when decreasing the resolution. It can be "min", "max", "mean", "median", or "first" for continuous variables, "mode" for categorical variables.
#Resampling is the method applied when increasing the resolution. It can be "near", "bilinear", "bicubic" or others as supported by gdalwarp (see https://gdal.org/programs/gdalwarp.html).

#**Temporal window **
#The temporal window is chosen with the parameters ``t0`` and ``t1``,  and allows selecting a specific time window for time series collections. ``t0`` and ``t1`` must be in the ISO8601 datetime format (e.g. "2022-07-28"). To ensure that ``t0`` and ``t1`` are included in the collection time span, you can access this time span:

print(info_chelsa_monthly$t0)
print(info_chelsa_monthly$t1)

#Collections which do not contain time series still have a temporal dimension by default, with t0 = t1. 

print(info_chelsa$t0)
print(info_chelsa$t1)

#To request such collections, ``t0`` and ``t1`` can be let to NULL. 


#**Temporal resolution **
#Temporal resolution parameter can be used when dowloading time series data. It corresponds to the size of pixels in time-direction, expressed as ISO8601 period string (only 1 number and unit is allowed) such as "P1Y" for a yearly aggregation, "P1M" for a monthly aggregation.

## Case studies

#### Loading current climate data from chelss-clim

#We load the cube from the collections ``chelsa-climate``,  using the observations points as an extent. 
#CHELSA current climate correspond to the aggregation of climate data from 1981 to 2010. Thus, here, t0 = t1 (by default, initial date of the period). 
#Here, the temporal resolution is not a useful parameter as data are yet aggregated over the 30-years period. 
#Spatial resolution is set to 2000 m, with aggregation done using the function mean.

layers <- c("bio2", "bio6")

cube <- 
 load_cube(stac_path = "http://io.biodiversite-quebec.ca/stac/",
           collections = c("chelsa-clim"), 
           bbox = bbox,
           limit = 5000,
           srs.cube = "EPSG:6623",
           t0 = "1981-01-01",
           t1 = NULL,
           layers = layers,
           variable = NULL,
           spatial.res = 1000, # in meters
           temporal.res = "P1Y",
           aggregation = "mean",
           resampling = "near") 

x11(); plot(cube)
cube %>% plot()

#Warning: The function plot() directly applied to the cube may create awkward visualization with apparently empty data, as for bio2. It is caused because the color palette is the same for all the plotted variables.

#For better visualization (and for export), the cube can be converted to classic raster format in R. The custom-built cube_to_raster function allows converting either to the raster or terra format, using the argument format. 

my_raster <- cube_to_raster(cube, format = "terra")
my_raster

plot(my_raster)

#It's better ! The drawback is that the two rasters are now loaded in your current session and uses memory. If you don't need to load them yet as rasters, we advice to keep them as a cube.

#For instance, we can apply a mask directly on the cube with the function gdalcubes::extract_geom, using a vector as a mask. The mask extent must be included in the cube extent, so we crop it prior to applying the function.

mask <- terra::vect("C:/GitHub/stac-catalogue/data/shape_study_area_nolakes_nad83.shp")

mask <- crop_lon_lat(mask, xmin = bbox$xmin,
                      ymin = bbox$ymin,
                      xmax = bbox$xmax,
                      ymax = bbox$ymax)
plot(mask)

```

#### Loading monthly climate data from chelsa-monthly

We load the cube from the collections ``chelsa-monthly``,  using the observations points as an extent. 
To load monthly climate data, the names of the climatic variable must be used in the argument "variable".         

```{r, warning=FALSE}
cube <- 
 load_cube(stac_path = "http://io.biodiversite-quebec.ca/stac/",
           collections = c("chelsa-monthly"), 
           bbox = bbox,
           limit = 5000,
           srs.cube = "EPSG:6623",
           t0 = "1990-01-01",
          t1 =  "1990-03-01",
         #  layers = "tas_12_2019",
          variable = c("tas"),
           spatial.res = 1000, # in meters
           temporal.res = "P1M",
           aggregation = "mean",
           resampling = "near") 

```




```{r, warning=FALSE}
my.raster.masked <- gdalcubes::filter_geom(cube,  sf::st_geometry(sf::st_as_sf(mask)), srs = proj_to)
plot(my.raster.masked)
```

To extract the values of variables for a set of points, we can use the function ``extract_geom`` from the ``gdalcubes`` package. Points have to be in a sf format.
```{r, warning=FALSE}
value.points <- gdalcubes::extract_geom(cube, sf::st_as_sf(obs_pts))

head(value.points)
```

#### Loading future climate data

To load the climate data projection from the collection "chelsa-clim-proj", we use a function very similar to ``load_cube``, except you have to to select the climatic scenario (``rcp``): "ssp126", "ssp370", or "ssp585", and the time span of the projection (``time.span``): "2011-2040", "2041-2070" or "2071-2100").


```{r, warning=FALSE}
cube.future <- load_cube_projection(stac_path = 
                                    stac_path,
                                  limit = 5000, 
                                  collections = c('chelsa-clim-proj'), 
                                  bbox = bbox,
                                  rcp = 'ssp126', #ssp126, ssp370, ssp585
                                  srs.cube = proj_to, 
                                  variable = "bio1",
                                  time.span = "2041-2070", #2041-2070 or 2071-2100
                                  spatial.res = 1000, 
                                  temporal.res  = "P1Y", 
                                  aggregation = "mean",
                                  resampling = "near") 
```

Variable extraction also uses the extract_geom function but the output is different. The output table contains one column
per variable and per climatic model. 
```{r, warning=FALSE}
value.points.future <- extract_geom(cube.future, sf::st_as_sf(obs_pts))

head(value.points.future)
```

We extract the name of the variable, the year, the model and the scenario from the column names, then we aggregate the value by point and bioclimatic variable using the mean (we could use another function such as median, min, etc.)
```{r, warning=FALSE}
value.points.future <- value.points.future %>% tidyr::pivot_longer(
    cols = dplyr::starts_with("bio"),
    names_to = c("variable", "year", "model", "scenario"), 
    names_pattern = "(.*)_(.*)_(.*)_(.*)",
    values_to = "value"
  )  %>%
  dplyr::group_by(FID, variable) %>% dplyr::summarise(mean = mean(value))
head(value.points.future)

```

We eventually convert the data into a tidy dataframe.

```{r, warning=FALSE}
value.points.future <- value.points.future %>%
     tidyr::pivot_wider(names_from = variable, values_from = mean)
head(value.points.future)

```

#### Loading land cover data

We load ESA time series for the year 2000. We decrease the resolution from 250m to 2000m. As land coer is a categorical variable, we select the aggregation method "mode"

```{r, warning=FALSE}
cube <- 
  load_cube(stac_path = stac_path,
           collections = c("esacci-lc"), 
           bbox = bbox,
           srs.cube = proj_to,
           t0 = "2000-01-01",
           t1 = "2000-01-01",
           spatial.res = 2000, # in meters
           temporal.res =  "P1Y",
           aggregation = "mode",
           resampling = "near") 

```

```{r, warning=FALSE}
plot(cube)
```

#### Loading soil data

Let's have a look at the collection:
```{r, warning=FALSE}
info_soil <- get_info_collection(stac_path =
                                  "http://io.biodiversite-quebec.ca/stac/",
                                collections = c('soilgrids'),
                                bbox = NULL)

print(info_soil$variables)
print(info_soil$layers)
```
In this collection, each variable is declined in several depths. Let's download the proportion of sand, whatever the depth.

```{r, warning=FALSE}
cube <- 
  load_cube(stac_path = stac_path,
           collections = c("soilgrids"), 
           bbox = bbox,
           variable = "sand",
           srs.cube = proj_to,
           limit = 5000,
           spatial.res = 1000, # in meters
           temporal.res =  "P1Y",
           aggregation = "mean",
           resampling = "near") 
cube
```
We can also directly select a specific variable and depth by specifying the layer name:

```{r, warning=FALSE}
cube <- 
  load_cube(stac_path = stac_path,
           collections = c("soilgrids"), 
           bbox = bbox,
           layers = "sand_100-200cm",
           srs.cube = srs_cube,
           limit = 5000,
           spatial.res = 1000, # in meters
           temporal.res =  "P1Y",
           aggregation = "mean",
           resampling = "near") 
cube
```
### To continue: usefull ressources

* gdalcube doc: [https://cran.r-project.org/web/packages/gdalcubes/gdalcubes.pdf](https://cran.r-project.org/web/packages/gdalcubes/gdalcubes.pdf)
* Examples based on gdalcubes library: [https://github.com/appelmar/gdalcubes_R](https://github.com/appelmar/gdalcubes_R)