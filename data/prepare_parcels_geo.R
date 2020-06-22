library(data.table)
setwd("~/psrc/R/shinyserver/base_year_2018/data")

pcl <- fread("prclxy1.csv")
setnames(pcl, "PIN", "parcel_id")
setnames(pcl, "LATITUDE", "lat")
setnames(pcl, "LONGITUDE", "lon")
saveRDS(pcl[, .(parcel_id, PINFIPS, lat, lon)], "parcels_geo.rds")

