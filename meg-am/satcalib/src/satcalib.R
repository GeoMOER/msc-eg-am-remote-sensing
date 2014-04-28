# General setttings
input.path <- "active/moc/am/data/landsat/l8_2013-07-07_level_1"
output.path <- "active/moc/am/data/landsat/l8_2013-07-07_level_2"
# input.path <- "active/moc/am/data/hyperion/eoh1_2013-03-24_level_1"
# output.path <- "active/moc/am/data/hyperion/eoh1_2013-03-24_level_2"
hillshade.filepath <- "active/moc/am/data/dgm10/dgm10_level_4/dgm10_mr_hillshade.tif"
slope.filepath <- "active/moc/am/data/dgm10/dgm10_level_4/dgm10_mr_slope.tif"
aspect.filepath <- "active/moc/am/data/dgm10/dgm10_level_4/dgm10_mr_aspect.tif"
crop.filepath <- "active/moc/am/data/vectors/mr.shp"
src.filepath <- "active/moc/github/scripts/meg-am/satcalib/src"

rad.unit <- "rad"
sensor <- "landsat"
# sensor <- "hyperion"

# Set working directory and load libraries
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

# Get satellite data files
datasets <- list.files(path = ".", 
                       pattern = "TIF",
                       full.names = TRUE, recursive = TRUE)

# Crop and calibrate satellite bands
crop.template <- readOGR(paste0(dsn, crop.filepath), layer = "mr")

x <- datasets[5]
sapply(datasets, function(x){
  act.filepath <- x
  act.info <- getInfoFromLevel1Name(act.filepath, sensor)
  coef <- getSceneCoef(act.info[2], act.info[1], rad.unit, sensor)
  act.data <-  raster(act.filepath, native = TRUE)
  act.data.crop <- allignDataGeometry(act.data, crop.template)
  act.data.calib <- coef[1] * act.data.crop + coef[2]
  writeRaster(act.data.calib, filename = paste0(dsn, output.path, "/",
                                                basename(act.filepath)),
              format = "GTiff", overwrite = FALSE)
})

# Correct satellite bands for terrain induced illumination patterns
hillshade.data <- raster(paste0(dsn, hillshade.filepath), native = TRUE)
hillshade.data.crop <- allignDataGeometry(hillshade.data, act.data.calib)
slope.data <- raster(paste0(dsn, slope.filepath), native = TRUE)
slope.data.crop <- allignDataGeometry(slope.data, act.data.calib)
aspect.data <- raster(paste0(dsn, aspect.filepath), native = TRUE)
aspect.data.crop <- allignDataGeometry(aspect.data, act.data.calib)

act.data.calib.ic <- lmIlluminationCorrection(act.data.calib, 
                                              hillshade.data.crop)
act.data.calib.ic <- cCorrection(act.data.calib, coef[4], coef[5],
                                 slope.data.crop, aspect.data.crop)
writeRaster(act.data.calib.ic, filename = paste0(dsn, output.path, "/",
                                              basename(act.filepath)),
            format = "GTiff", overwrite = FALSE)
