#!/bin/bash

spe=("abies_balsamea" "acer_rubrum" "acer_saccharum" "betula_alleghaniensis" "betula_papyrifera" "fagus_grandifolia" "larix_laricina" "picea_glauca" "picea_mariana" "picea_rubens" "pinus_banksiana" "pinus_resinosa" "pinus_strobus" "populus_tremuloides" "quercus_rubra" "tsuga_canadensis" "thuja_occidentalis")
indd=("valid_scientific_name" "geom" "year_obs" "month_obs" "day_obs" "dataset_name")

for species in ${spe[@]};
do
    for indexa in ${indd[@]}
    do
        ogrinfo -sql 'CREATE INDEX '$indexa' ON '$species' ('$indexa')' /home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/Boulanger_tree_species/$species.gpkg
    done
done