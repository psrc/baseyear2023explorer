## Convert datasets into rds format

library(data.table)

process.parcels <- TRUE
process.buildings <- TRUE
process.households <- TRUE
process.jobs <- TRUE
process.persons <- TRUE
process.agents.with.race <- FALSE

parcels.file.name <- "parcels.csv"
buildings.file.name <- "imputed_buildings_lodes_match_20210302.csv"
households.file.name <- "households.csv"
jobs.file.name <- "jobs.csv"
persons.file.name <- "persons.csv"
#households.with.race.file.name <- "householdsWR.csv"
#persons.with.race.file.name <- "personsWR.csv"

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

if(process.agents.with.race) {
    hhs <- fread(households.with.race.file.name)
    saveRDS(hhs, "households_with_race.rds")
    pers <- fread(persons.with.race.file.name)
    if(length(grep(":i4", colnames(pers))) > 0) { # remove ":*" from colnames
        spl <- strsplit(colnames(pers), ":")
        colnames(pers) <- sapply(spl, function(x) x[1])
    }
    saveRDS(pers, "persons_with_race.rds")
}


if(process.jobs){
    hhs <- fread(jobs.file.name)
    saveRDS(hhs, "jobs.rds")
}

