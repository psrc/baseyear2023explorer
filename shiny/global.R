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
library(sf)
library(rmapshaper)
library(googleVis)

wrkdir <- '/home/shiny/apps/' # shiny path
#wrkdir <- '/Users/hana/psrc/R/shinyserver'

#data <- 'base_year_2018/data'
data <- 'baseyear2018explorer/data'

parcel.main <- 'parcels_geo.rds'
parcel.att <- 'parcels.rds'
blds.file <- 'buildings.rds'
hhs.file <- 'households.rds'
jobs.file <- 'jobs.rds'
persons.file <- 'persons.rds'
hhs.wrace.file <- 'households_with_race.rds'
persons.wrace.file <- 'persons_with_race.rds'

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
buildings[hhs[, .(.N, population=sum(persons)), by = "building_id"], 
          `:=`(households = i.N, population = i.population), on = "building_id"][
    is.na(households), `:=`(households = 0, population = 0)]

jobs <- readRDS(file.path(wrkdir, data, jobs.file))
buildings[jobs[, .N, by = "building_id"], jobs := i.N, on = "building_id"][is.na(jobs), jobs := 0]

parcels.attr <- data.table(parcels.attr)
parcels.attr[buildings[, .(households = sum(households), jobs = sum(jobs), 
                           DU = sum(residential_units), nrsqft = sum(non_residential_sqft),
                           pop = sum(population)
                           ), by = "parcel_id"], 
             `:=`(households = i.households, jobs = i.jobs, residential_units = i.DU, 
                  non_residential_sqft = i.nrsqft, population = i.pop), 
             on = "parcel_id"]
parcels.attr[, region_id := 1]

buildings <- merge(buildings, building_types, by = "building_type_id")
buildings <- merge(buildings, parcels.attr[, c("parcel_id", setdiff(colnames(parcels.attr), colnames(buildings))), with = FALSE], by = "parcel_id")

# add jitter to coordinates where there are multiple buildings per parcel
buildings[, Nbld := .N, by = parcel_id]
set.seed(1234)
buildings[Nbld > 1, lat := jitter(lat)]
buildings[Nbld > 1, lon := jitter(lon)]
buildings[, Nbld := NULL]

coordinates <- SpatialPointsDataFrame(parcels.attr[!is.na(lon),.(lon, lat)], parcels.attr[!is.na(lon)])

# prepare for spatial indicators
shapes <- list(zone_id = rmapshaper::ms_simplify(sf::st_read(file.path(wrkdir, data, "gis", "TAZ_2010_WGS84.shp"),
                            stringsAsFactors = FALSE), keep = 0.005),
               faz_id = rmapshaper::ms_simplify(sf::st_read(file.path(wrkdir, data, "gis", "FAZ_2010_WGS84.shp"),
                                    stringsAsFactors = FALSE))
                )
shapes$zone_id$name_id <- shapes$zone_id$TAZ
shapes$faz_id$name_id <- shapes$faz_id$FAZ10

hhs[buildings, parcel_id := i.parcel_id, on = "building_id"]
hhs[parcels.attr, `:=`(zone_id = i.zone_id, faz_id = i.faz_id), on = "parcel_id"]
hhs[income < 0, income := 0]

jobs[buildings, parcel_id := i.parcel_id, on = "building_id"]
jobs[parcels.attr, `:=`(zone_id = i.zone_id, faz_id = i.faz_id), on = "parcel_id"]

pers <- readRDS(file.path(wrkdir, data, persons.file))
pers[hhs, `:=`(zone_id = i.zone_id, faz_id = i.faz_id), on = "household_id"]

hhs.wrace <- readRDS(file.path(wrkdir, data, hhs.wrace.file))
hhs.wrace[buildings, parcel_id := i.parcel_id, on = "building_id"]
hhs.wrace[parcels.attr, `:=`(zone_id = i.zone_id, faz_id = i.faz_id), on = "parcel_id"]
pers.wrace <- readRDS(file.path(wrkdir, data, persons.wrace.file))
pers.wrace[hhs.wrace, `:=`(zone_id = i.zone_id, faz_id = i.faz_id), on = "household_id"]

hhs.wrace <- merge(hhs.wrace, pers.wrace[, .(rc_alone = sum(race < 9), 
                                                rc_white = sum(race == 1),
                                                rc_black = sum(race == 2),
                                                rc_asian = sum(race == 6)), by = "household_id"],
                   by = "household_id")
buildings[hhs.wrace[, .(rc_alone = sum(rc_alone), rc_white = sum(rc_white), 
                        rc_black = sum(rc_black), rc_asian = sum(rc_asian)), by = "building_id"], 
          `:=`(pop_rc_alone = i.rc_alone, pop_rc_white = i.rc_white, 
               pop_rc_black = i.rc_black, pop_rc_asian = i.rc_asian), on = "building_id"][
                   is.na(pop_rc_alone), `:=`(pop_rc_alone = 0, pop_rc_white = 0, pop_rc_black = 0, pop_rc_asian = 0)]

# pre-compute indicators
indicators.dt <- list()
incquant <- quantile(hhs$income, probs = c(0.25, 0.75), na.rm = TRUE)
for(gid in c("zone_id", "faz_id")){
    indicators.dt[[gid]] <- hhs[, `:=`(is_low_income = income < incquant[[1]],
                                       is_high_income = income > incquant[[2]])][, 
                                     .(median_income = median(income),
                                       average_hh_size = mean(persons),
                                       tot_households = .N,
                                       tot_population = sum(persons),
                                       low_income = sum(is_low_income),
                                       high_income = sum(is_high_income)
                                    ), by = list(name_id = eval(parse(text=gid)))]
    indicators.dt[[gid]] <- merge(indicators.dt[[gid]], 
                                  jobs[, .(tot_jobs = .N, 
                                              nonHB_jobs = sum(home_based_status == 0),
                                              home_based_jobs = sum(home_based_status)), 
                                       by = list(name_id = eval(parse(text=gid)))],
                                  by = "name_id", all = TRUE)
    indicators.dt[[gid]] <- merge(indicators.dt[[gid]], 
                                  pers[, .(average_age = mean(age)), 
                                       by = list(name_id = eval(parse(text=gid)))],
                                  by = "name_id", all = TRUE)
    
    indicators.dt[[gid]] <- merge(indicators.dt[[gid]], 
                                  pers.wrace[, .(pop_rc_alone = sum(race < 9), 
                                                 pop_rc_white = sum(race == 1),
                                                 pop_rc_black = sum(race == 2),
                                                 pop_rc_asian = sum(race == 6),
                                                 pop_rc_total = .N), 
                                       by = list(name_id = eval(parse(text=gid)))],
                                  by = "name_id", all = TRUE)
    
    indicators.dt[[gid]] <- merge(indicators.dt[[gid]], 
                                  parcels.attr[, sqft_for_land_value := parcel_sqft * (land_value > 0)][, 
                                                .(acres = sum(parcel_sqft)/43560,
                                                   land_value = sum(land_value/1000),
                                                   sqft_for_value = sum(sqft_for_land_value/1000)),
                                               by = list(name_id = eval(parse(text=gid)))], 
                                  by = "name_id")
    indicators.dt[[gid]][acres > 0, `:=`(population_per_acre = tot_population/acres,
                                        jobs_per_acre = tot_jobs/acres
                                        )]
    indicators.dt[[gid]][, `:=`(percent_low_income = 100*low_income/tot_households, 
                                percent_high_income = 100*high_income/tot_households,
                                land_value_per_sf = land_value/sqft_for_value,
                                #percent_non_white_mixed = 100*(pop_rc_total - pop_rc_white)/pop_rc_total,
                                #percent_non_white = 100*(pop_rc_alone - pop_rc_white)/pop_rc_alone,
                                percent_black = 100*pop_rc_black/pop_rc_total,
                                percent_asian = 100*pop_rc_asian/pop_rc_total,
                                percent_white = 100*pop_rc_white/pop_rc_total,
                                percent_other = 100*(pop_rc_total - (pop_rc_white + pop_rc_black + pop_rc_asian))/pop_rc_total
                                )]
    indicators.dt[[gid]][tot_population > 0, `:=`(jobs_per_capita = tot_jobs/tot_population)]
}
chart.geo <- "faz_id"
indicators.chart <- copy(indicators.dt[[chart.geo]])
setnames(indicators.chart, "name_id", chart.geo)
indicators.chart[, `:=`(Year = 2018)]
#zones <- unique(parcels.attr[faz_id > 0, .(zone_id, faz_id, county_id)])
zones <- unique(parcels.attr[faz_id > 0, .(faz_id, county_id)])
zones[data.table(county_id = c(33, 35, 53, 60), county = c("King", "Kitsap", "Pierce", "Snohomish")), 
      county := i.county, on = "county_id"][, county_id := NULL]
zones <- zones[!duplicated(zones[[chart.geo]])]
indicators.chart <- merge(indicators.chart[faz_id > 0], zones, by = chart.geo)
fcols <- c("zone_id", "faz_id", "county")
fcols <- c("faz_id", "county")
indicators.chart[, (fcols) := (lapply(.SD, as.factor)), .SDcols = fcols]

polmap.settings <- list(median_income = list(breaks = c(0, 50000, 65000, 80000, 100000, 120000), digits = 0),
                        average_hh_size = list(breaks = c(1, 1.5, 2.5, 3.5), digits = 1),
                        population_per_acre = list(breaks = c(0, 1, 5, 10, 15, 20), digits = 1),
                        jobs_per_acre = list(breaks = c(0, 0.5, 1, 5, 10, 15), digits = 1),
                        jobs_per_capita = list(breaks = c(0, 0.1, 0.5, 0.8, 1, 2), digits = 1),
                        land_value_per_sf = list(digits = 0),
                        average_age = list(breaks = c(0, 25, 35, 45, 55, 65), digits = 1)
                        )
# percent settings
race.indicators <- c("percent_black", "percent_asian", "percent_white", "percent_other")
for(ind in c("percent_low_income", "percent_high_income", race.indicators))
    polmap.settings[[ind]] <- list(breaks = c(0, 10, 25, 50, 75, 80, 90, 100), digits = 1)

rm(attr)
rm(parcels)
rm(jobs)
rm(hhs)
rm(hhs.wrace)
rm(pers.wrace)

