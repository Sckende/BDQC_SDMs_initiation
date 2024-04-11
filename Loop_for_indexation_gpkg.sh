#!/bin/bash

spe=("Abies_balsamea" "Acer_rubrum" "Acer_saccharum" "Betula_alleghaniensis" "Betula_papyrifera" "Fagus_grandifolia" "Larix_laricina" "Picea_glauca" "Picea_mariana" "Picea_rubens" "Pinus_banksiana" "Pinus_resinosa" "Pinus_strobus" "Populus_tremuloides" "Quercus_rubra" "Tsuga_canadensis" "Thuja_occidentalis")
indd=("valid_scientific_name" "geom" "year_obs" "month_obs" "day_obs" "dataset_name")

for species in ${spe[@]};
do
    for indexa in ${indd[@]}
    do
        ogrinfo -sql 'CREATE INDEX '$indexa' ON '$species' ('$indexa')' /home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/Boulanger_tree_species/$species.gpkg
    done
done