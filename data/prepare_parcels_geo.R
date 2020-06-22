library(data.table)
setwd("~/psrc/R/shinyserver/base_year_2018/data")

pcl <- fread("prcltzbl.csv")
pclc <- pcl[, .(PIN, PINFIPS, INTPTLAT10, INTPTLON10)]
setnames(pclc, "PIN", "parcel_id")
setnames(pclc, "INTPTLAT10", "lat")
setnames(pclc, "INTPTLON10", "lon")
saveRDS(pclc, "parcels_geo.rds")

