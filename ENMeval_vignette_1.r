#### Vignette about ENMeval ####
# https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html
installed.packages()

# Data acquisition & pre-processing
library('ENMeval')
#library(terra)
library(raster)
library(dplyr)

set.seed(48)


bv <- spocc::occ('Bradypus variegatus',
                'gbif',
                limit = 300,
                has_coords = TRUE)
occs <- as.data.frame(bv$gbif$data$Bradypus_variegatus[,2:3])
dim(occs)

# Removing occurrences that have the same coordinates is good practice to
# avoid pseudoreplication.
occs <- occs[!duplicated(occs),]
dim(occs)

# Locate the predictor raster files from the dismo folder.
envs.files <- list.files(path = paste(system.file(package = 'dismo'),
                        '/ex',
                        sep = ''), 
                         pattern = 'grd', full.names = TRUE)


# Read the raster files into a RasterStack.
# These variables represent 8 bioclimatic variables and one categorical variable "biome".
# Find the descriptions of the bioclimatic variables here: 
# https://www.worldclim.org/data/bioclim.html
envs <- raster::stack(envs.files)
names(envs)

# The biome raster has some NAs for cells that have values in the other rasters.
# Let's mask all rasters to biome to change the value of these cells to NA for all rasters.
# ENMeval will do this automatically, but let's do it here to avoid the warning message later.
# We change back from a RasterBrick to RasterStack because of issues with assigning 
# factor rasters for RasterBricks.
envs <- raster::mask(envs, envs[[9]]) %>% raster::stack()
# Make sure to declare the categorical variable as a factor
envs$biome <- raster::as.factor(envs$biome)

# Let's now remove occurrences that are cell duplicates -- these are
# occurrences that share a grid cell in the predictor variable rasters.
# Although Maxent does this by default, keep in mind that for other algorithms you may
# or may not want to do this based on the aims of your study.
# Another way to space occurrence records a defined distance from each other to avoid
# spatial autocorrelation is with spatial thinning (Aiello-Lammens et al. 2015).
occs.cells <- raster::extract(envs[[1]],
                            occs,
                            cellnumbers = TRUE)
occs.cellDups <- duplicated(occs.cells[,1])
occs <- occs[!occs.cellDups,]
dim(occs)



# Plot first raster in the stack, the mean annual temperature.
x11();plot(envs[[1]], main="Mean annual temperature")

# Add points for all the occurrence points onto the raster.
points(occs)

# There are some points east of the Amazon River.
# Suppose we know this is a population that we don't want to include in the model.
# We can remove these points from the analysis by subsetting the occurrences by 
# latitude and longitude.
occs <- filter(occs, latitude > -20, longitude < -45)
dim(occs)
# Plot the subsetted occurrences to make sure we filtered correctly.
points(occs, col = 'red')
