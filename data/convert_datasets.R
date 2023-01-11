## Convert datasets into rds format
## Note: When data are updated, change the "last data update" in ui.R

library(data.table)

load.from.mysql <- TRUE

process.parcels <- FALSE
process.buildings <- TRUE
process.households <- TRUE
process.jobs <- TRUE
process.persons <- FALSE
process.agents.with.race <- FALSE # not needed anymore
process.capacity <- FALSE
process.schools <- FALSE


# used when loading from files (if load.from.mysql is FALSE)
parcels.file.name <- "parcels.csv"
buildings.file.name <- "imputed_buildings_lodes_match_20210302.csv"
households.file.name <- "households.csv"
jobs.file.name <- "jobs.csv"
persons.file.name <- "persons.csv"
schools.file.name <- "schools.csv"
#households.with.race.file.name <- "householdsWR.csv"
#persons.with.race.file.name <- "personsWR.csv"

# used when loading from MySQL DB (if load.from.mysql is TRUE)
parcels.tbl.name <- "parcels"
buildings.tbl.name <- "buildings"
households.tbl.name <- "households"
jobs.tbl.name <- "jobs"
persons.tbl.name <- "persons"
schools.tbl.name <- "schools"
db.name <- "2018_parcel_baseyear"
#db.name <- "2018_parcel_baseyear_luv3_working"
#db.name <- "2018_parcel_baseyear_luv3_scenarios"

# Connecting to Mysql
mysql.connection <- function(dbname) {
    # credentials can be stored in a file (as one column: username, password, host)
    if(file.exists("creds.txt")) {
        creds <- read.table("creds.txt", stringsAsFactors = FALSE)
        un <- creds[1,1]
        psswd <- creds[2,1]
        if(nrow(creds) > 2) h <- creds[3,1] 
        else h <- .rs.askForPassword("host:")
    } else {
        un <- .rs.askForPassword("username:")
        psswd <- .rs.askForPassword("password:")
        h <- .rs.askForPassword("host:")
    }
    dbConnect(MySQL(), user = un, password = psswd, dbname = dbname, host = h)
}

if(load.from.mysql) {
    library(RMySQL)
    # create DB connection
    mydb <- mysql.connection(db.name)
}


if(process.parcels){
    cat("\nProcessing parcels ...")
    if(load.from.mysql) {
        qr <- dbSendQuery(mydb, paste0("select * from ", parcels.tbl.name))
        pclattr <- data.table(fetch(qr, n = -1))
        dbClearResult(qr)
    } else {
        pclattr <- fread(parcels.file.name)
    }
    pclattr[, census_2010_block_id := as.character(census_2010_block_id)]
    saveRDS(pclattr, "parcels.rds")
}

if(process.buildings){
    cat("\nProcessing buildings ...")
    if(load.from.mysql) {
        qr <- dbSendQuery(mydb, paste0("select parcel_id, building_id, gross_sqft, non_residential_sqft, residential_units, year_built, 
                land_area, improvement_value, stories, building_type_id, sqft_per_unit from ", buildings.tbl.name))
        bldc <- data.table(fetch(qr, n = -1))
        dbClearResult(qr)
    } else {
        bld <- fread(buildings.file.name)
        bldc <- bld[, .(parcel_id, building_id, gross_sqft, non_residential_sqft, residential_units, year_built, 
                    land_area, improvement_value, stories, building_type_id, use_code, sqft_per_unit)]
    }
    saveRDS(bldc, "buildings.rds")
}

if(process.households){
    cat("\nProcessing households ...")
    if(load.from.mysql) {
        qr <- dbSendQuery(mydb, paste0("select * from ", households.tbl.name))
        hhs <- data.table(fetch(qr, n = -1))
        dbClearResult(qr)
    } else {
        hhs <- fread(households.file.name)
    }
    saveRDS(hhs, "households.rds")
}

if(process.persons){
    cat("\nProcessing persons ...")
    if(load.from.mysql) {
        qr <- dbSendQuery(mydb, paste0("select * from ", persons.tbl.name))
        pers <- data.table(fetch(qr, n = -1))
        dbClearResult(qr)
    } else {
        pers <- fread(persons.file.name)
    }
    saveRDS(pers, "persons.rds")
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
    cat("\nProcessing jobs ...")
    if(load.from.mysql) {
        qr <- dbSendQuery(mydb, paste0("select * from ", jobs.tbl.name))
        jobs <- data.table(fetch(qr, n = -1))
        dbClearResult(qr)
    } else {
        jobs <- fread(jobs.file.name)
    }
    saveRDS(jobs, "jobs.rds")
}

if(process.schools){
    cat("\nProcessing schools ...")
    if(load.from.mysql) {
        qr <- dbSendQuery(mydb, paste0("select * from ", schools.tbl.name))
        jobs <- data.table(fetch(qr, n = -1))
        dbClearResult(qr)
    } else {
        schools <- fread(schools.file.name)
    }
    saveRDS(jobs, "schools.rds")
}

if(process.capacity){
    cat("\nComputing capacity ...")
    pcl <- readRDS("parcels.rds")
    if(load.from.mysql) {
        qr <- dbSendQuery(mydb, "select * from development_constraints")
        constraints <- data.table(fetch(qr, n = -1))
        dbClearResult(qr)
    } else {
        constraints <- fread("development_constraints.csv")
    }
    pclw <- merge(pcl[, .(parcel_id, plan_type_id, parcel_sqft)], constraints, by = "plan_type_id", 
                  allow.cartesian=TRUE, all = TRUE)
    
    # compute building sqft & residential units
    if(! "coverage" %in% colnames(pclw)) pclw[, coverage := 0.95]

    pclw[constraint_type == "far", building_sqft := parcel_sqft * maximum * coverage]
    pclw[constraint_type == "units_per_acre", residential_units := parcel_sqft * maximum / 43560 * coverage]
    
    # select one max for residential and one for non-res type, so that each parcel has 2 records at most
    pclwu <- pclw[pclw[, .I[which.max(maximum)], by = .(parcel_id, constraint_type)]$V1][!is.na(parcel_id)]
    pclwu[, mixed := .N > 1, by = parcel_id]
    pclwm <- merge(pclwu[constraint_type == "far", .(parcel_id, SQFTcap = building_sqft)],
                    pclwu[constraint_type == "units_per_acre", .(parcel_id, DUcap = residential_units)], all = TRUE)[, mixed_cap := 0]
    pclwm[!is.na(SQFTcap) & !is.na(DUcap) & SQFTcap > 0 & DUcap > 0, mixed_cap := 1]
    pclwm[is.na(SQFTcap), SQFTcap := 0]
    pclwm[is.na(DUcap), DUcap := 0]
    saveRDS(pclwm, "parcels_capacity.rds")
}

if(load.from.mysql){
    # disconnect DB
    dbDisconnect(mydb)
}

cat("\nConversion done.\n")
