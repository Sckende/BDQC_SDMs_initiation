# obj: dev of pretty simple recommender system
library(terra)
library(sf)
library(dplyr)

#### Creation of data ####
# --------------------- #

# Province polygons
prov <- st_as_sf(terra::readRDS("/home/claire/BDQC-GEOBON/data/gadm/gadm41_CAN_1_pk.rds"))

# Selection of Ontario & convertion in UTM
onUTM <- prov %>% 
            filter(NAME_1 == "Ontario") %>% 
            st_transform(st_crs("+init=epsg:2027"))
x11(); plot(st_geometry(onUTM))

# Grid creation
grid_spacing <- 50000  # size of squares, in units of the CRS 

grid_glob <- st_make_grid(
    onUTM,
    square = T,
    cellsize = c(grid_spacing, grid_spacing)) %>% # the grid, covering bounding box
  st_sf()

plot(grid_glob, col = 'white')
plot(st_geometry(onUTM), add = T)

overlay <- st_intersects(
    onUTM,
    grid_glob)

grid_on <- grid_glob[overlay[[1]], ]

plot(st_geometry(grid_on))
plot(st_geometry(onUTM), add = T)

on_plot <- function(){
  plot(grid_on)
  plot(st_geometry(onUTM), add = T)
}


# random selection of areas - function
# connectUp <- function(polys, n){
#     nb = spdep::poly2nb(polys)
#     g = igraph::graph.adjlist(nb)

#     selected = sample(nrow(polys),1)

#     while(length(selected) < n){
#         nbrs = unlist(igraph::ego(g, 1, selected, mindist=1))
#         newnbrs = setdiff(nbrs, selected)
#         selected = c(selected, sample(newnbrs,1))
#     }
#     return(selected)
# }

plot(grid_on) # 506 polys

# species A - omnipresent
# test <- connectUp(grid_on, 350)
# plot(grid_on[test,], add = T, col = rgb(0, 0, 1, 0.5))

# species B - fractionnee
# test2 <- vector()
# n <- sample(3:10, 5) #8 5 4 6 3
# for (i in seq_along(n)) {
#   pols <- connectUp(grid_on, n[i])

#   test2 <- c(test2, pols)
# }
# test2 #33  45  55  44  69  43  84  53  10  16  17  15   7 453 438 437 436 141 121 100 122 119 101  97 118 116
# plot(grid_on[unique(test2),], add = T, col = rgb(1, 0, 0, 0.5))

# species C - normale
# test3 <- connectUp(grid_on,125)
# plot(grid_on[test3,], add = T, col = rgb(0, 1, 0, 0.5))

# species D - normale
# test4 <- connectUp(grid_on, 90)
# plot(grid_on[test4,], add = T, col = rgb(0.5, 0.5, 0, 0.5))

# # species E - normale
# test5 <- connectUp(grid_on, 45)
# plot(grid_on[test5,], add = T, col = rgb(0, 0.5, 0.5, 0.5))

# assemb <- data.frame(species = c(rep("spe_A", length(test)),
#                                  rep("spe_B", length(test2)),
#                                  rep("spe_C", length(test3)),
#                                  rep("spe_D", length(test4)),
#                                  rep("spe_E", length(test5))),
#                      pol_numb = c(test, test2, test3, test4, test5),
#                      col = c(rep(rgb(0, 0, 1, 0.25), length(test)),
#                                  rep(rgb(0, 1, 0, 0.25), length(test2)),
#                                  rep(rgb(1, 0, 0, 0.25), length(test3)),
#                                  rep(rgb(0.5, 0.5, 0, 0.25), length(test4)),
#                                  rep(rgb(0, 0.5, 0.5, 0.25), length(test5))))
# assemb$species <- as.factor(assemb$species)
# summary(assemb)
# saveRDS(assemb, paste(getwd(), "/ALGO_assemblage.rds", sep = ""))

assemb <- readRDS(paste(getwd(), "/ALGO_assemblage.rds", sep = ""))

x11(); on_plot()
plot(st_geometry(grid_on[assemb[assemb$species %in% c("spe_C", "spe_D"),]$pol_numb, ]),
                 col = assemb[assemb$species %in% c("spe_C", "spe_D"),]$col,
                 add = T)

on_plot()
plot(st_geometry(grid_on[assemb$pol_numb, ]),
      col = assemb$col, add = T)

#### creation de la matrice users/items ####
# la note des items représente le nombre de polygones partagés par espèces,
# ramené à un % pour normaliser la note
unique(assemb$species)
spe_pol_ls <- split(assemb, assemb$species)
# on_pol <- dim(grid_on)[1]
mtr_ls <- lapply(spe_pol_ls, function(x) {
                    id <- unique(x$species)
                    pol_size <- dim(x)[1]
                    spe_A <- sum(x$pol_numb %in% spe_pol_ls[["spe_A"]]$pol_numb) * 100 / pol_size
                    spe_B <- sum(x$pol_numb %in% spe_pol_ls[["spe_B"]]$pol_numb) * 100 / pol_size
                    spe_C <- sum(x$pol_numb %in% spe_pol_ls[["spe_C"]]$pol_numb) * 100 / pol_size
                    spe_D <- sum(x$pol_numb %in% spe_pol_ls[["spe_D"]]$pol_numb) * 100 / pol_size
                    spe_E <- sum(x$pol_numb %in% spe_pol_ls[["spe_E"]]$pol_numb) * 100 / pol_size

                    inf <- data.frame(species = id,
                                      spe_A = spe_A,
                                      spe_B = spe_B,
                                      spe_C = spe_C,
                                      spe_D = spe_D,
                                      spe_E = spe_E)
                    inf
})

names(mtr_ls) <- NULL
mtr_UI <- do.call("rbind", mtr_ls)
mtr_UI

#### see also - Alexandros Karatzoglou & recommender system
#### see also - https://www.youtube.com/watch?v=G4MBc40rQ2k - simpler method with Python
#### see also - https://builtin.com/data-science/collaborative-filtering-recommender-system