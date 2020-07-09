library(data.table)
setwd("~/psrc/R/shinyserver/baseyear2018explorer/data")

pcl <- fread("prclxy.csv")
setnames(pcl, "PIN", "parcel_id")
setnames(pcl, "LATITUDE", "lat")
setnames(pcl, "LONGITUDE", "lon")
saveRDS(pcl[, .(parcel_id, PINFIPS, lat, lon)], "parcels_geo.rds")

