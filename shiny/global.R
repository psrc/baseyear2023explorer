library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(tidyverse)
library(DT)
library(geoshaper)
library(sp)
library(data.table)

wrkdir <- '/home/shiny/apps/' # shiny path
#wrkdir <- '/Users/hana/psrc/R/shinyserver'
# wrkdir <- 'C:/Users/CLam/Desktop/'

#data <- 'base_year_2018/data'
data <- 'baseyear2018explorer/data'

parcel.main <- 'parcels_geo.rds'
parcel.att <- 'parcels.rds'
blds.file <- 'buildings.rds'
hhs.file <- 'households.rds'
jobs.file <- 'jobs.rds'

parcels <- readRDS(file.path(wrkdir, data, parcel.main))
attr <- readRDS(file.path(wrkdir, data, parcel.att))

parcels.attr <- parcels %>% left_join(attr, by = "parcel_id")

buildings <- readRDS(file.path(wrkdir, data, blds.file))

building_types <- read.csv(file.path(wrkdir, data, "building_types.csv"), stringsAsFactors = FALSE)[,c("building_type_id", "building_type_name", "generic_building_type_id", "generic_building_type_description")]
ordered_building_type_names <- c("single family residential", "multi-family residential", 
                                 "commercial", "office", "industrial"
                                 )
ordered_building_type_names <- c(ordered_building_type_names, 
                                 setdiff(building_types$generic_building_type_description, ordered_building_type_names))
building_types_selection <- data.frame(unique(subset(data.table(building_types), generic_building_type_description %in% ordered_building_type_names)[, .(generic_building_type_id, generic_building_type_description)]))
rownames(building_types_selection) <- building_types_selection$generic_building_type_description
building_types_selection <- building_types_selection[ordered_building_type_names,"generic_building_type_id", drop=FALSE]
building_types <- data.table(building_types)
setkey(building_types, building_type_id)

color.attributes <- c("bt"="building_type_id", 
                      "sizeres"="residential_units", "sizenonres"="non_residential_sqft")

hhs <- readRDS(file.path(wrkdir, data, hhs.file))
buildings[hhs[, .N, by = "building_id"], households := i.N, on = "building_id"][is.na(households), households := 0]

jobs <- readRDS(file.path(wrkdir, data, jobs.file))
buildings[jobs[, .N, by = "building_id"], jobs := i.N, on = "building_id"][is.na(jobs), jobs := 0]

parcels.attr <- data.table(parcels.attr)
parcels.attr[buildings[, .(households = sum(households)), by = "parcel_id"], households := i.households, on = "parcel_id"]
parcels.attr[buildings[, .(jobs = sum(jobs)), by = "parcel_id"], jobs := i.jobs, on = "parcel_id"]
parcels.attr[buildings[, .(DU = sum(residential_units)), by = "parcel_id"], residential_units := i.DU, on = "parcel_id"]
parcels.attr[buildings[, .(nrsqft = sum(non_residential_sqft)), by = "parcel_id"], non_residential_sqft := i.nrsqft, on = "parcel_id"]
parcels.attr$secondLocationID <- paste(as.character(parcels.attr$parcel_id), "_selectedLayer", sep = "")

buildings <- merge(buildings, building_types, by = "building_type_id")
buildings <- merge(buildings, parcels.attr[, c("parcel_id", setdiff(colnames(parcels.attr), colnames(buildings))), with = FALSE], by = "parcel_id")

# add jitter to coordinates where there are multiple buildings per parcel
buildings[, Nbld := .N, by = parcel_id]
set.seed(1234)
buildings[Nbld > 1, lat := jitter(lat)]
buildings[Nbld > 1, lon := jitter(lon)]
buildings[, Nbld := NULL]

coordinates <- SpatialPointsDataFrame(parcels.attr[!is.na(lon),.(lon, lat)], parcels.attr[!is.na(lon)])

rm(attr)
rm(parcels)
rm(jobs)
rm(hhs)
