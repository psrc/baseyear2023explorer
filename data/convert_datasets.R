## Convert datasets into rds format

library(data.table)

process.parcels <- TRUE
process.buildings <- TRUE
process.households <- TRUE
process.jobs <- TRUE

parcels.file.name <- "parcels.csv"
buildings.file.name <- "imputed_buildings_lodes_match_20200616.csv"
households.file.name <- "households.csv"
jobs.file.name <- "jobs.csv"

if(process.parcels){
    pclattr <- fread(parcels.file.name)
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

if(process.jobs){
    hhs <- fread(jobs.file.name)
    saveRDS(hhs, "jobs.rds")
}

