# General setttings
input.path <- "active/moc/am/data/landsat/l8_2013-07-07_level_1"
output.path <- "active/moc/am/data/landsat/l8_2013-07-07_level_2"
input.path <- "active/moc/am/data/hyperion/eoh1_2013-03-24_level_1"
output.path <- "active/moc/am/data/hyperion/eoh1_2013-03-24_level_2"
dem.filepath <- "active/moc/am/data/dgm10/dgm10_level_4/dgm10_mr_hillshade.tif"
crop.filepath <- "active/moc/am/data/vectors/mr.shp"
src.filepath <- "active/moc/github/scripts/meg-am/satcalib/src"

rad.unit <- "rad"
sensor <- "landsat"
sensor <- "hyperion"


# Working directory and data set
dsn <- switch(Sys.info()[["sysname"]], 
              "Linux" = "/media/permanent/",
              "Windows" = "D:/")
setwd(paste0(dsn, input.path))

library(rgdal)
library(raster)
for (i in c("getCalibCoef.R", "getInfoFromLevel1Name.R", "allignDataGeometry.R"))
  source(paste0(dsn, src.filepath, "/", i))

crop.template <- readOGR(paste0(dsn, crop.filepath), layer = "mr")

# Get satellite data files
datasets <- list.files(path = ".", 
                       pattern = "TIF",
                       full.names = TRUE, recursive = TRUE)

# Calibrate satellite data
x <- datasets[5]
sapply(datasets, function(x){
  act.filepath <- x
  act.info <- getInfoFromLevel1Name(act.filepath, sensor)
  coef <- getCalibCoef(act.info[2], act.info[1], rad.unit, sensor)
  act.data <-  raster(act.filepath, native = TRUE)
  act.data.crop <- allignDataGeometry(act.data, crop.template)
  act.data.calib <- coef[1] * act.data.crop + coef[2]
  writeRaster(act.data.calib, filename = paste0(dsn, output.path, "/",
                                                basename(act.filepath)),
              format = "GTiff", overwrite = FALSE)
})

# Terrain correction
hillshade.data <- raster(paste0(dsn, dem.filepath), native = TRUE)
hillshade.data.crop <- allignDataGeometry(hillshade.data, act.data.calib)
hillshade.lm <- lm(getValues(act.data.calib) ~ getValues(hillshade.data.crop))
hillshade.pred <- hillshade.lm$coefficients[1] + 
  hillshade.data.crop * hillshade.lm$coefficients[2]
hillshade.pred <- hillshade.pred - cellStats(act.data.calib, 'mean')
act.data.illum <- act.data.calib + hillshade.pred
writeRaster(act.data.illum, filename = paste0(dsn, output.path, "/",
                                              basename(act.filepath)),
            format = "GTiff", overwrite = FALSE)
plot(hillshade.lm)
