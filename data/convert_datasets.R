## Convert datasets into rds format

library(data.table)

process.parcels <- FALSE
process.buildings <- FALSE
process.households <- FALSE
process.jobs <- FALSE
process.persons <- TRUE

parcels.file.name <- "parcels.csv"
buildings.file.name <- "imputed_buildings_lodes_match_20200707.csv"
households.file.name <- "households.csv"
jobs.file.name <- "jobs.csv"
persons.file.name <- "persons.csv"

if(process.parcels){
    pclattr <- fread(parcels.file.name)
    pclattr[, census_2010_block_id := as.character(census_2010_block_id)]
    saveRDS(pclattr, "parcels.rds")
}

if(process.buildings){
    bld <- fread(buildings.file.name)
    bldc <- bld[, .(parcel_id, building_id, gross_sqft, non_residential_sqft, residential_units, year_built, 
                land_area, improvement_value, stories, building_type_id, use_code, sqft_per_unit)]
    saveRDS(bldc, "buildings.rds")
}

if(process.households){
    hhs <- fread(households.file.name)
    saveRDS(hhs, "households.rds")
}

if(process.persons){
    hhs <- fread(persons.file.name)
    saveRDS(hhs, "persons.rds")
}

if(process.jobs){
    hhs <- fread(jobs.file.name)
    saveRDS(hhs, "jobs.rds")
}

