# Crop, calibrate and illumination correct landsat or hyperion data sets.
#
#  Copyright (C) 2014 Thomas Nauss
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#  Please send any comments, suggestions, criticism, or (for our sake) bug
#  reports to admin@environmentalinformatics-marburg.de

#### General setttings #########################################################
sensor <- "landsat"
input.path <- "active/moc/am/data/landsat/l8_2013-07-07_level_1"
output.path <- "active/moc/am/data/landsat/l8_2013-07-07_level_2"

hillshade.filepath <- "active/moc/am/data/dgm10/dgm10_level_4/dgm10_mr_hillshade.tif"
slope.filepath <- "active/moc/am/data/dgm10/dgm10_level_4/dgm10_mr_slope.tif"
aspect.filepath <- "active/moc/am/data/dgm10/dgm10_level_4/dgm10_mr_aspect.tif"
crop.filepath <- "active/moc/am/data/landsat/l8_2013-07-07_misc/ws-03-03_area_template.tif"
src.filepath <- "active/moc/github/scripts/meg-am/satcalib/src"

rad.unit <- "rad"


#### Set working directory and load libraries ##################################
dsn <- switch(Sys.info()[["sysname"]], 
              "Linux" = "/media/permanent/",
              "Windows" = "D:/")
setwd(paste0(dsn, input.path))

library(rgdal)
library(raster)
for (i in c("getSceneCoef.R", "getInfoFromLevel1Name.R", 
            "allignDataGeometry.R", "lmIlluminationCorrection.R",
            "cCorrection.R"))
  source(paste0(dsn, src.filepath, "/", i))


#### Crop auxiliary data #######################################################
crop.template <- raster(paste0(dsn, crop.filepath), native = TRUE)
hillshade.data <- raster(paste0(dsn, hillshade.filepath), native = TRUE)
slope.data <- raster(paste0(dsn, slope.filepath), native = TRUE)
aspect.data <- raster(paste0(dsn, aspect.filepath), native = TRUE)
print("Croping auxiliary data...")
hillshade.data.crop <- allignDataGeometry(hillshade.data, crop.template)
slope.data.crop <- allignDataGeometry(slope.data, crop.template)
aspect.data.crop <- allignDataGeometry(aspect.data, crop.template)


#### Crop, calibrate and illumination correct satellite bands ##################
# Crop, callibrate and illumination correct each satellite band file and write
# results to level 2 folder. Auxiliary data is only croped during the first loop.
datasets <- list.files(path = ".", 
                       pattern =  glob2rx("*.TIF"),
                       full.names = TRUE, recursive = TRUE)

sapply(datasets,function(x){
  act.filepath <- x
  print(paste0("Computing file ", act.filepath))
  act.info <- getInfoFromLevel1Name(act.filepath, sensor)
  coef <- getSceneCoef(act.info[2], act.info[1], rad.unit, sensor)
  act.data <-  raster(act.filepath, native = TRUE)
  act.data.crop <- allignDataGeometry(act.data, crop.template)
  act.data.calib <- coef[1] * act.data.crop + coef[2]
  act.data.calib.ic <- cCorrection(act.data.calib, coef[4], coef[5],
                                   slope.data.crop, aspect.data.crop)
  writeRaster(act.data.calib.ic, filename = paste0(dsn, output.path, "/",
                                                   basename(act.filepath)),
              format = "GTiff", overwrite = TRUE)
})
