# Cm_SDB

# contains library and function calls necessary for many scripts in
# this project

ifelse(Sys.info()[1] == 'Linux',
       source('~/Documents/R/tools/TomosFunctions.R'),
       source('~/R/tools/TomosFunctions.R'))

library(ggmap)
library(sp)
library(rgdal)         # bindings for the geospatial data abstraction library
library(broom)        # instead of fortify in ggplot2 (recommended)

internet <- F

if (internet){
  west.coast <- get_map(location = c(lon = -120.0,
                                     lat = 40.0),
                        zoom = 5,
                        maptype = "hybrid",
                        #color = "bw",
                        filename = 'west_coast',
                        force = F)
  saveRDS(west.coast, file = 'RData/west_coast.rds')

  sdbay.all <- get_map(location = c(lon = -117.15,
                                    lat = 32.65),
                       zoom = 12,
                       maptype = "hybrid",
                       #color = "bw",
                       filename = 'sdbay_all',
                       force = F)
  saveRDS(sdbay.all, file = 'RData/sdbay_all.rds')

  sdbay.south <- get_map(location = c(lon = -117.117,
                                      lat = 32.625),
                         zoom = 13,
                         maptype = "hybrid",
                         #color = "bw",
                         filename = 'sdbay_south',
                         force = F)
  saveRDS(sdbay.south, file = 'RData/sdbay_south.rds')

  sdbay.med <- get_map(location = c(lon =  -117.117,
                                    lat = 32.625),
                       zoom = 13,
                       maptype = "satellite",
                       color = "bw",
                       filename = 'sdbay_med',
                       force = F)
  saveRDS(sdbay.med, file = 'RData/sdbay_med.rds')
} else {
  sdbay.all <- readRDS(file = 'RData/sdbay_all.rds')
  sdbay.south <- readRDS(file = 'RData/sdbay_south.rds')
  sdbay.med <- readRDS(file = 'RData/sdbay_med.rds')
  west.coast <- readRDS(file = 'RData/west_coast.rds')
}

#map.west.coast <- ggmap(west.coast)
#map.sdbay <- ggmap(sdbay.all)


