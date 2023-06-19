#### Vignette about ENMeval ####
# https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html
#installed.packages()

#### Data acquisition & pre-processing ####
# --------------------------------------- #

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


# First we extract the climatic variable values at the occurrence points -- these values are our "reference". We remove the categorical variable for these operations because the math only makes sense for continuous variables -- the function will not work with categorical variables.
occs.z <- raster::extract(envs[[-9]], occs)
# Now we use the similarity() function (borrowed from the rmaxent package) to calculate environmental similarity metrics of our predictor variable extent compared to the reference points.
occs.sim <- similarity(envs[[-9]], occs.z)
occs.mess <- occs.sim$similarity_min

# This is the MESS plot -- increasingly negative values represent increasingly different climatic conditions from the reference (our occurrences), while increasingly positive values are more similar. First, we'll make a SpatialPoints object for our occurrences for plotting with levelplot() from the rasterVis package (Lamigueiro & Hijmans 2021). This package has great plotting functionality for rasters, and by default bins values for display when data is continuous.
occs.sp <- sp::SpatialPoints(occs)

# Vector data (points, polygons) are added to a levelplot with a "+", like ggplot.
rasterVis::levelplot(occs.mess, main = "Environmental similarity", margin = FALSE) + 
  latticeExtra::layer(sp.points(occs.sp, col="black"))

# Continuous plotting can be done as demonstrated below by specifiying a scale
myScale <- seq(cellStats(occs.mess, min), cellStats(occs.mess, max), length.out = 100)
rasterVis::levelplot(occs.mess, main = "Environmental similarity", at = myScale, margin = FALSE) + 
  latticeExtra::layer(sp.points(occs.sp, col="black"))

# Here we define some good colors for representing categorical variables
cols <- RColorBrewer::brewer.pal(8, "Set1")
# This map shows the variable for each grid cell that is most different from the reference
rasterVis::levelplot(occs.sim$mod, col.regions = cols, main = "Most different variable") + 
  latticeExtra::layer(sp.points(occs.sp, col="black"))

# This map shows the variable for each grid cell that is most similar to the reference
rasterVis::levelplot(occs.sim$mos, col.regions = cols, main = "Most similar variable") + 
  latticeExtra::layer(sp.points(occs.sp, col="black"))

# We'll now experiment with a different spatial R package called sf (simple features). Let's make our occs into a sf object -- as the coordinate reference system (crs) for these points is WGS84, a geographic crs (lat/lon) and the same as our envs rasters, we specify it as the RasterStack's crs.
occs.sf <- sf::st_as_sf(occs, coords = c("longitude","latitude"), crs = raster::crs(envs))

# Now, we project our point data to an equal-area projection, which converts our degrees to meters, which is ideal for buffering (the next step). We use the typical Eckert IV projection.
eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
occs.sf <- sf::st_transform(occs.sf, crs = eckertIV)

# Buffer all occurrences by 500 km, union the polygons together (for visualization), and convert back to a form that the raster package can use. Finally, we reproject the buffers back to WGS84 (lat/lon).
# We choose 500 km here to avoid sampling the Caribbean islands.
occs.buf <- sf::st_buffer(occs.sf, dist = 500000) %>% 
  sf::st_union() %>% 
  sf::st_sf() %>%
  sf::st_transform(crs = raster::crs(envs))
plot(envs[[1]], main = names(envs)[1])
points(occs)
# To add sf objects to a plot, use add = TRUE
plot(occs.buf, border = "blue", lwd = 3, add = TRUE)


# Crop environmental rasters to match the study extent
envs.bg <- raster::crop(envs, occs.buf)

# Next, mask the rasters to the shape of the buffers
envs.bg <- raster::mask(envs.bg, occs.buf)
plot(envs.bg[[1]], main = names(envs)[1])
points(occs)
plot(occs.buf, border = "blue", lwd = 3, add = TRUE)

# Randomly sample 10,000 background points from one background extent raster (only one per cell without replacement). Note: Since the raster has <10,000 pixels, you'll get a warning and all pixels will be used for background. We will be sampling from the biome variable because it is missing some grid cells, and we are trying to avoid getting background points with NA. If one raster in the stack has NAs where the other rasters have data, ENMeval internally converts these cells to NA.
bg <- dismo::randomPoints(envs.bg[[9]], n = 10000) %>% as.data.frame()
colnames(bg) <- colnames(occs)

# Notice how we have pretty good coverage (every cell).
plot(envs.bg[[1]])
points(bg, pch = 20, cex = 0.2)

#### Partitionning occurences for evaluation ####
# --------------------------------------------- #
# 7 methods

#### --> Spatial Block
# subdivision in 4 groups with equal number of occurences but no equal spatial division
block <- get.block(occs, bg, orientation = "lat_lon")
# Let's make sure that we have an even number of occurrences in each partition.
table(block$occs.grp)
# We can plot our partitions on one of our predictor variable rasters to visualize where they fall in space. The ENMeval 2.0 plotting functions use ggplot2 (Wickham 2016), a popular plotting package for R with many online resources. We can add to the ggplots with other ggplot functions in an additive way, making these plots easily customizable.
evalplot.grps(pts = occs, pts.grp = block$occs.grp, envs = envs.bg) + 
  ggplot2::ggtitle("Spatial block partitions: occurrences")

# PLotting the background shows that the background extent is partitioned in a way that maximizes evenness of points across the four bins, not to maximize evenness of area.
evalplot.grps(pts = bg, pts.grp = block$bg.grp, envs = envs.bg) + 
  ggplot2::ggtitle("Spatial block partitions: background")


# If we are curious how different the environment associated with each partition is from that of all the others, we can use this function to plot histograms or rasters of MESS predictions with each partition as the reference. First we need to extract the predictor variable values at our occurrence and background localities.
occs.z <- cbind(occs, raster::extract(envs, occs))
bg.z <- cbind(bg, raster::extract(envs, bg))
evalplot.envSim.hist(sim.type = "mess", ref.data = "occs", occs.z = occs.z, bg.z = bg.z, 
                     occs.grp = block$occs.grp, bg.grp = block$bg.grp, categoricals = "biome")

evalplot.envSim.hist(sim.type = "most_diff", ref.data = "occs", occs.z = occs.z, bg.z = bg.z, 
                     occs.grp = block$occs.grp, bg.grp = block$bg.grp, categoricals = "biome")

evalplot.envSim.hist(sim.type = "most_sim", ref.data = "occs", occs.z = occs.z, bg.z = bg.z, 
                     occs.grp = block$occs.grp, bg.grp = block$bg.grp, categoricals = "biome")


# Here we plot environmental similarity values for the entire extent with respect to each validation group. We use the bb.buf (bounding box buffer) argument to zoom in to our study extent.
evalplot.envSim.map(sim.type = "mess", ref.data = "occs", envs = envs, occs.z = occs.z, 
                    bg.z = bg.z, occs.grp = block$occs.grp, bg.grp = block$bg.grp, 
                    categoricals = "biome", bb.buf = 7)

evalplot.envSim.map(sim.type = "most_diff", ref.data = "occs", envs = envs, occs.z = occs.z, 
                    bg.z = bg.z, occs.grp = block$occs.grp, bg.grp = block$bg.grp, 
                    categoricals = "biome", bb.buf = 7)

evalplot.envSim.map(sim.type = "most_sim", ref.data = "occs", envs = envs, occs.z = occs.z, 
                    bg.z = bg.z, occs.grp = block$occs.grp, bg.grp = block$bg.grp, 
                    categoricals = "biome", bb.buf = 7)
#### --> Spatial Checkerboard
# these two next methods subdivide geographic space equally but do not ensure a balanced number of occurrence localities in each bin
cb1 <- get.checkerboard1(occs, envs.bg, bg, aggregation.factor=5)
evalplot.grps(pts = occs, pts.grp = cb1$occs.grp, envs = envs.bg)

# Plotting the background points shows the checkerboard pattern clearly.
evalplot.grps(pts = bg, pts.grp = cb1$bg.grp, envs = envs.bg)

# We can see from the MESS maps that this method results in similar environmental representation between the partitions.
evalplot.envSim.hist(sim.type = "mess", ref.data = "occs", occs.z = occs.z, bg.z = bg.z, 
                     occs.grp = cb1$occs.grp, bg.grp = cb1$bg.grp, categoricals = "biome")

evalplot.envSim.map(sim.type = "mess", ref.data = "occs", envs = envs, occs.z = occs.z, 
                    bg.z = bg.z, occs.grp = cb1$occs.grp, bg.grp = cb1$bg.grp, 
                    categoricals = "biome", bb.buf = 7)

# We can increase the aggregation factor to give the groups bigger boxes. This can result in groups that are more environmentally different from each other.
cb1.large <- get.checkerboard1(occs, envs.bg, bg, aggregation.factor=30)
evalplot.grps(pts = occs, pts.grp = cb1.large$occs.grp, envs = envs.bg)

evalplot.grps(pts = bg, pts.grp = cb1.large$bg.grp, envs = envs.bg)

evalplot.envSim.hist(sim.type = "mess", ref.data = "occs", occs.z = occs.z, 
                     bg.z = bg.z, occs.grp = cb1.large$occs.grp, bg.grp = cb1$bg.grp, 
                     categoricals = "biome")

evalplot.envSim.map(sim.type = "mess", ref.data = "occs", envs = envs, occs.z = occs.z, 
                    bg.z = bg.z, occs.grp = cb1.large$occs.grp, bg.grp = cb1$bg.grp, 
                    categoricals = "biome", bb.buf = 7)

#### --> Spatial Hierarchical Checkerboard
# k = 4 groups

cb2 <- get.checkerboard2(occs, envs.bg, bg, aggregation.factor=c(5,5))
evalplot.grps(pts = occs, pts.grp = cb2$occs.grp, envs = envs.bg)

# Plotting the background points shows the checkerboard pattern very clearly.
evalplot.grps(pts = bg, pts.grp = cb2$bg.grp, envs = envs.bg)

# Different from checkerboard1, some partitions here do show some difference in environmental representation, but not as consistently different as with block.
evalplot.envSim.hist(sim.type = "mess", ref.data = "occs", occs.z = occs.z, 
                     bg.z = bg.z, occs.grp = cb2$occs.grp, bg.grp = cb2$bg.grp, 
                     categoricals = "biome")

evalplot.envSim.map(sim.type = "mess", ref.data = "occs", envs = envs, occs.z = occs.z, 
                    bg.z = bg.z, occs.grp = cb2$occs.grp, bg.grp = cb2$bg.grp, 
                    categoricals = "biome", bb.buf = 7)
#### --> Jackknife (leave-one-out)
# The next two methods differ from the first three in that (a) they do not partition the background points into different groups (meaning that the full background is used to evaluate each partition), and (b) they do not account for spatial autocorrelation between validation and training localities. 

jack <- get.jackknife(occs, bg)
# If the number of input points is larger than 10, the legend for the groups is suppressed.
evalplot.grps(pts = occs, pts.grp = jack$occs.grp, envs = envs.bg)

#### --> Random k-fold
# The ‘random k-fold’ method partitions occurrence localities randomly into a user-specified number of (k) bins (Hastie et al. 2009). This method is equivalent to the ‘cross-validate’ partitioning scheme available in the current version of the Maxent software GUI.

rand <- get.randomkfold(occs, bg, k = 5)
evalplot.grps(pts = occs, pts.grp = rand$occs.grp, envs = envs.bg)

# As the partitions are random, there is no large environmental difference between them.
evalplot.envSim.hist(sim.type = "mess", ref.data = "occs", occs.z = occs.z, 
                     bg.z = bg.z, occs.grp = rand$occs.grp, bg.grp = rand$bg.grp, 
                     categoricals = "biome")

evalplot.envSim.map(sim.type = "mess", ref.data = "occs", envs = envs, occs.z = occs.z, 
                    bg.z = bg.z, occs.grp = rand$occs.grp, bg.grp = rand$bg.grp, 
                    categoricals = "biome", bb.buf = 7)

#### --> Fully Withheld Testing Data
# The ‘testing’ method evaluates the model on a fully withheld testing dataset that is not used to create the full model (i.e., not included in the training data), meaning that cross validation statistics are not calculated. Evaluations with fully withheld testing data have been shown to result in models with better transferability (Soley-Guardia et al. 2019).

# First, let's specify a fake testing occurrence dataset and plot the testing points with the rest of our data
occs.testing <- data.frame(longitude = -runif(10, 55, 65), latitude = runif(10, -10, 0))
evalplot.grps(pts = rbind(occs, occs.testing), 
              pts.grp = c(rep(1, nrow(occs)), rep(2, nrow(occs.testing))), envs = envs.bg)

# Next, let's extract the predictor variable values for our testing points.
occs.testing.z <- cbind(occs.testing, raster::extract(envs, occs.testing))
# We use the same background groups as random partitions here because the background used for testing data is also from the full study extent. We use here the occs.testing.z parameter to add information for our testing localities, and we set the partitions for occurrences all to zero (as no partitioning is done).
evalplot.envSim.hist(sim.type = "mess", ref.data = "occs", occs.z = occs.z, 
                     bg.z = bg.z, occs.grp = rep(0, nrow(occs)), bg.grp = rand$bg.grp, 
                     categoricals = "biome", occs.testing.z = occs.testing.z)

evalplot.envSim.map(sim.type = "mess", ref.data = "occs", envs = envs, occs.z = occs.z, 
                    bg.z = bg.z, occs.grp = rep(0, nrow(occs)), bg.grp = rand$bg.grp, 
                    categoricals = "biome", bb.buf = 7, occs.testing.z = occs.testing.z)
# We can see what is to be expected -- the testing dataset is much more restricted environmentally than the training data, and thus is much more difference with the study extent.

#### --> User
# maximum flexibility
# Here we run a k-means clustering algorithm to group our occurrences into discrete spatial groups based on their coordinates.
grp.n <- 6
kmeans <- kmeans(occs, grp.n)
occs.grp <- kmeans$cluster
evalplot.grps(pts = occs, pts.grp = occs.grp, envs = envs.bg)
# Assign all background records
bg.grp <- rep(0, nrow(bg))
evalplot.grps(pts = bg, pts.grp = bg.grp, envs = envs.bg)

# Here we find the centers of the occurrence k-means clusters and calculate the spatial distance of each background point to them. We then find which center had the minimum distance for each record and assign that record to this centroid group.
centers <- kmeans$center
d <- raster::pointDistance(bg, centers, lonlat = TRUE)
bg.grp <- apply(d, 1, function(x) which(x == min(x)))
evalplot.grps(pts = bg, pts.grp = bg.grp, envs = envs.bg)

# other method
library(blockCV)
library(sf)

# First, we convert our occurrence and background records to spatial point data with the package sf and assign the correct coordinate reference system.
occsBg.sf <- sf::st_as_sf(rbind(occs, bg), coords = c("longitude","latitude"), 
                          crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
raster::crs(envs.bg) <- raster::crs(occsBg.sf)
# Here, we implement the spatialBlock function from the blockCV package. The required inputs are similar to ENMeval partitioning functions, but here you assign the size of blocks in meters with the argument theRange (here set at 1000 km), and the partition selection method can be assigned as either "checkerboard" or "random". In addition, the spatialBlock function returns a map showing the different spatial partitions.
sb <- blockCV::spatialBlock(speciesData = occsBg.sf,
                            rasterLayer = envs.bg,
                            theRange = 1000000,
                            k = 5,
                            selection = "random") # deprecated function !

# We can pull out the partition information from the SpatialBlock object to assemble occs.grp and bg.grp, which can be used for plotting or as user.grp inputs for ENMevaluate.
occs.grp <- sb$foldID[1:nrow(occs)]
bg.grp <- sb$foldID[(nrow(occs)+1):length(sb$foldID)]
evalplot.grps(pts = bg, pts.grp = bg.grp, envs = envs.bg)


#### Running ENMeval ####
# -------------------- #

e.mx.l <- ENMevaluate(occs = occs, envs = envs, bg = bg, 
                      algorithm = 'maxnet', partitions = 'block', 
                      tune.args = list(fc = "L", rm = 1:2))
