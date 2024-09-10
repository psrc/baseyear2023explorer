library(data.table)
library(sf)

setwd("~/psrc/R/shinyserver/baseyear2023explorer/data")

#pcl <- fread("prclxy.csv")
#setnames(pcl, "PIN", "parcel_id")
#setnames(pcl, "LATITUDE", "lat")
#setnames(pcl, "LONGITUDE", "lon")

pcl <- readRDS("parcels.rds")

points <- pcl[, .(parcel_id, x_coord_sp, y_coord_sp)]

points.trans <- st_transform(st_as_sf(points, crs = 2285, coords = c("x" = "x_coord_sp", "y" = "y_coord_sp"), 
                         remove = FALSE), crs = 4326)
coord <- st_coordinates(points.trans)

points[, `:=`(lat = coord[,2], lon = coord[,1])]

saveRDS(points, "parcels_geo.rds")

