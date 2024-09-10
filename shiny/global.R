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

#wrkdir <- '/home/shiny/apps/' # shiny path
wrkdir <- '/Users/hana/psrc/R/shinyserver'

data <- 'baseyear2023explorer/data'

parcel.main <- 'parcels_geo.rds'
parcel.att <- 'parcels.rds'
parcel.cap <- 'parcels_capacity.rds'
blds.file <- 'buildings.rds'
hhs.file <- 'households.rds'
jobs.file <- 'jobs.rds'
persons.file <- 'persons.rds'
schools.file <- 'schools.rds'

# load parcels files
parcels <- readRDS(file.path(wrkdir, data, parcel.main)) # geo-coordinates
attr <- readRDS(file.path(wrkdir, data, parcel.att)) # various attributes

# jitter parcels that have the same coordinates (stacked parcels)
dupl.pcl <- unique(parcels[duplicated(parcels[, .(lat, lon)]), parcel_id])
parcels[parcel_id %in% dupl.pcl, `:=`(lat = jitter(lat, factor = 0.2), 
                                      lon = jitter(lon, factor = 0.2))]

# join parcel datasets together
parcels.attr <- parcels %>% left_join(attr, by = "parcel_id") 

# capacity
if(file.exists((f <- file.path(wrkdir, data, parcel.cap)))){
    cap <- readRDS(f) 
    ratio <- 50/100
    cap[, DUcapxratio := ifelse(mixed_cap == 1, ratio * DUcap, DUcap)]
    cap[, SQFTcapxratio := ifelse(mixed_cap == 1, ratio * SQFTcap, SQFTcap)]
    # join with parcel dataset
    parcels.attr <- parcels.attr %>% left_join(cap, by = "parcel_id")
} else {
    parcels.attr[, `:=`(DUcap = 0, SQFTcap = 0, DUcapxratio = 0, SQFTcapxratio = 0)]
}


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

tod_data <- data.table(tod_id = 0:6, tod_name = c("No TOD", "BRT", "Commuter Rail", NA, "Light Rail", "Ferry", "RGC"),
                       #tod_color = c("#0092FF", "#FF00DB", "#49FF00", "#00FF92", "#FF0000", "#4900FF", "#FFDB00")) # from re-arranged rainbow() call (doesn't work for awesomeIcons)
                       tod_color = c("lightblue", "pink", "lightgreen", "white", "red", "purple", "orange")) # these colors 

color.attributes <- c("bt"="building_type_id", 
                      "sizeres"="residential_units", "sizenonres"="non_residential_sqft", 
                      "tod" = "tod_id")

if(file.exists((f <- file.path(wrkdir, data, hhs.file)))) {
    hhs <- readRDS(f)
    buildings[hhs[, .(.N, population=sum(persons)), by = "building_id"], 
          `:=`(households = i.N, population = i.population), on = "building_id"][
            is.na(households), `:=`(households = 0, population = 0)]
} else buildings[, `:=`(households = 0, population = 0)]

if(file.exists((f <- file.path(wrkdir, data, jobs.file)))) {
    jobs <- readRDS(f)
    buildings[jobs[, .N, by = "building_id"], jobs := i.N, on = "building_id"][is.na(jobs), jobs := 0]
} else buildings[, `:=`(jobs = 0)]

if(file.exists((f <- file.path(wrkdir, data, schools.file)))) 
    schools <- readRDS(f)

# add attributes to parcels
parcels.attr <- data.table(parcels.attr)
parcels.attr[buildings[, .(households = sum(households), jobs = sum(jobs), 
                           DU = sum(residential_units), nrsqft = sum(non_residential_sqft),
                           pop = sum(population), Nblds = .N
                           ), by = "parcel_id"], 
             `:=`(households = i.households, jobs = i.jobs, residential_units = i.DU, 
                  non_residential_sqft = i.nrsqft, population = i.pop, Nblds = i.Nblds), 
             on = "parcel_id"]
parcels.attr[, region_id := 1]

if("census_2020_block_group_id" %in% colnames(parcels.attr)){
    parcels.attr[, census_2020_block_group_id := substr(census_2020_block_id, 1, 12)]
} else parcels.attr[, census_2020_block_group_id := 0]

if("tod_id" %in% colnames(parcels.attr)){
    parcels.attr <- merge(parcels.attr, tod_data, by = "tod_id")
} else parcels.attr[, `:=`(tod_id = 0, tod_name = "", tod_color = "")]

if(exists("schools")){
    parcels.attr[schools, school_id := i.school_id, on = "parcel_id"]
} else parcels.attr[, school_id := 0]

buildings <- merge(buildings, building_types, by = "building_type_id")
buildings <- merge(buildings, parcels.attr[, c("parcel_id", 
                                               setdiff(colnames(parcels.attr), colnames(buildings))), 
                                           with = FALSE], by = "parcel_id")
                   
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

if(exists("hhs")){
    hhs[buildings, parcel_id := i.parcel_id, on = "building_id"]
    hhs[parcels.attr, `:=`(zone_id = i.zone_id, faz_id = i.faz_id), on = "parcel_id"]
    hhs[income < 0, income := 0]
}

if(exists("jobs")){
    jobs[buildings, parcel_id := i.parcel_id, on = "building_id"]
    jobs[parcels.attr, `:=`(zone_id = i.zone_id, faz_id = i.faz_id), on = "parcel_id"]
}

if(file.exists((f <- file.path(wrkdir, data, persons.file)))){
    pers <- readRDS(f)
    pers[hhs, `:=`(zone_id = i.zone_id, faz_id = i.faz_id), on = "household_id"]
}

if(exists("hhs")&& exists("pers")){
    hhs <- merge(hhs, pers[, .(rc_white = sum(race_id == 1),
                           rc_black = sum(race_id == 2),
                           rc_asian = sum(race_id == 3),
                           rc_other = sum(race_id == 4),
                           rc_more_nhsp = sum(race_id == 5),
                           rc_hsp = sum(race_id %in% c(6,7))
                           ), by = "household_id"],
                   by = "household_id")
    buildings[hhs[, .(rc_white = sum(rc_white), rc_black = sum(rc_black), rc_asian = sum(rc_asian),
                  rc_other = sum(rc_other), rc_more_nhsp = sum(rc_more_nhsp), 
                  rc_hsp = sum(rc_hsp)), by = "building_id"], 
          `:=`(pop_rc_white = i.rc_white, pop_rc_black = i.rc_black, pop_rc_asian = i.rc_asian,
               pop_rc_other = i.rc_other, pop_rc_more_nhsp = i.rc_more_nhsp, pop_rc_hsp = i.rc_hsp
               ), on = "building_id"][
                   is.na(pop_rc_white), `:=`(pop_rc_white = 0, pop_rc_black = 0, pop_rc_asian = 0,
                                             pop_rc_other = 0, pop_rc_more_nhsp = 0, pop_rc_hsp = 0)]
}
# pre-compute indicators
indicators.dt <- list()
for(gid in c("zone_id", "faz_id")){
    if(exists("hhs")){
        incquant <- quantile(hhs$income, probs = c(0.25, 0.75), na.rm = TRUE)
        indicators.dt[[gid]] <- hhs[, `:=`(is_low_income = income < incquant[[1]],
                                       is_high_income = income > incquant[[2]])][, 
                                     .(median_income = median(income),
                                       average_hh_size = mean(persons),
                                       tot_households = .N,
                                       tot_population = sum(persons),
                                       low_income = sum(is_low_income),
                                       high_income = sum(is_high_income)
                                    ), by = list(name_id = eval(parse(text=gid)))]
        if(exists("jobs")){
            indicators.dt[[gid]] <- merge(indicators.dt[[gid]], 
                                  jobs[, .(tot_jobs = .N, 
                                              nonHB_jobs = sum(home_based_status == 0),
                                              home_based_jobs = sum(home_based_status)), 
                                       by = list(name_id = eval(parse(text=gid)))],
                                  by = "name_id", all = TRUE)
        }
        if(exists("pers")){
            indicators.dt[[gid]] <- merge(indicators.dt[[gid]], 
                                  pers[, .(average_age = mean(age)), 
                                       by = list(name_id = eval(parse(text=gid)))],
                                  by = "name_id", all = TRUE)
    
            indicators.dt[[gid]] <- merge(indicators.dt[[gid]], 
                                  pers[, .(pop_rc_white = sum(race_id == 1),
                                           pop_rc_black = sum(race_id == 2),
                                           pop_rc_asian = sum(race_id == 3),
                                           pop_rc_other = sum(race_id == 4),
                                           pop_rc_more_nhsp = sum(race_id == 5),
                                           pop_rc_hsp = sum(race_id %in% c(6,7)),
                                           pop_total = .N
                                           ), 
                                       by = list(name_id = eval(parse(text=gid)))],
                                  by = "name_id", all = TRUE)
        }
    }
    if(gid %in% colnames(parcels.attr)){
        indicators.dt[[gid]] <- merge(indicators.dt[[gid]], 
                                  parcels.attr[, sqft_for_land_value := parcel_sqft * (land_value > 0)][, 
                                                .(acres = sum(parcel_sqft)/43560,
                                                   land_value = sum(land_value/1000),
                                                   sqft_for_value = sum(sqft_for_land_value/1000),
                                                  total_du = sum(residential_units, na.rm = TRUE),
                                                  total_du_capacity = sum(DUcapxratio),
                                                  free_du_capacity = sum(pmax(0, DUcapxratio - residential_units), na.rm = TRUE)),
                                               by = list(name_id = eval(parse(text=gid)))], 
                                  by = "name_id")
        indicators.dt[[gid]][acres > 0, `:=`(population_per_acre = tot_population/acres,
                                        jobs_per_acre = tot_jobs/acres
                                        )]
        indicators.dt[[gid]][, `:=`(percent_low_income = 100*low_income/tot_households, 
                                percent_high_income = 100*high_income/tot_households,
                                land_value_per_sf = land_value/sqft_for_value,
                                percent_free_du_capacity = 100*free_du_capacity/total_du_capacity,
                                #percent_non_white_mixed = 100*(pop_rc_total - pop_rc_white)/pop_rc_total,
                                #percent_non_white = 100*(pop_rc_alone - pop_rc_white)/pop_rc_alone,
                                percent_black = 100*pop_rc_black/pop_total,
                                percent_asian = 100*pop_rc_asian/pop_total,
                                percent_white = 100*pop_rc_white/pop_total,
                                percent_hispanic = 100*pop_rc_hsp/pop_total,
                                percent_other = 100*(pop_total - (pop_rc_white + pop_rc_black + pop_rc_asian + pop_rc_hsp))/pop_total
                                )]
        indicators.dt[[gid]][tot_population > 0, `:=`(jobs_per_capita = tot_jobs/tot_population)]
    }     
}
chart.geo <- "faz_id"
if(chart.geo %in% names(indicators.dt)){
    indicators.chart <- copy(indicators.dt[[chart.geo]])
    setnames(indicators.chart, "name_id", chart.geo)
    indicators.chart[, `:=`(Year = 2023)]
    #zones <- unique(parcels.attr[faz_id > 0, .(zone_id, faz_id, county_id)])
    zones <- unique(parcels.attr[faz_id > 0, .(faz_id, county_id)])
    zones[data.table(county_id = c(33, 35, 53, 61), county = c("King", "Kitsap", "Pierce", "Snohomish")), 
        county := i.county, on = "county_id"][, county_id := NULL]
    zones <- zones[!duplicated(zones[[chart.geo]])]
    indicators.chart <- merge(indicators.chart[faz_id > 0], zones, by = chart.geo)
    fcols <- c("zone_id", "faz_id", "county")
    fcols <- c("faz_id", "county")
    indicators.chart[, (fcols) := (lapply(.SD, as.factor)), .SDcols = fcols]
}
polmap.settings <- list(median_income = list(breaks = c(0, 50000, 65000, 80000, 100000, 120000), digits = 0),
                        average_hh_size = list(breaks = c(1, 1.5, 2.5, 3.5), digits = 1),
                        population_per_acre = list(breaks = c(0, 1, 5, 10, 15, 20), digits = 1),
                        jobs_per_acre = list(breaks = c(0, 0.5, 1, 5, 10, 15), digits = 1),
                        jobs_per_capita = list(breaks = c(0, 0.1, 0.5, 0.8, 1, 2), digits = 1),
                        land_value_per_sf = list(digits = 0),
                        total_du_capacity = list(digits = 0),
                        free_du_capacity = list(digits = 0),
                        average_age = list(breaks = c(0, 25, 35, 45, 55, 65), digits = 1)
                        )
# percent settings
race.indicators <- c("percent_black", "percent_asian", "percent_white", "percent_hispanic", "percent_other")
capacity.indicators <- c("total_du", "total_du_capacity", "free_du_capacity", "percent_free_du_capacity")
for(ind in c("percent_low_income", "percent_high_income", "percent_free_du_capacity", race.indicators))
    polmap.settings[[ind]] <- list(breaks = c(0, 10, 25, 50, 75, 80, 90, 100), digits = 1)

# make sure all columns are present
for(col in c("parcel_id_fips", "county_id", "control_hct_id", "census_tract_id", "census_block_group_id",
             "census_block_id", "census_2020_block_id", "land_use_type_id", "plan_type_id", 
             "city_id", "faz_id", "zone_id")){
    if(!col %in% colnames(parcels.attr)) parcels.attr[[col]] <- 0
    if(!col %in% colnames(buildings)) buildings[[col]] <- 0
}
if(! "parcel_sqft" %in% colnames(parcels.attr)) parcels.attr[["parcel_sqft"]] <- parcels.attr[["gross_sqft"]]

#browser()
rm(attr)
rm(parcels)
if(exists("jobs")) rm(jobs)
if(exists("hhs")) rm(hhs)


