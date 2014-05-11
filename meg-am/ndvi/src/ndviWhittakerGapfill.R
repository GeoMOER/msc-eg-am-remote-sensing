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
rm(list = ls(all = T))

#### General setttings #########################################################
input.path <- "active/moc/am/data/modis/"
src.filepath <- "active/moc/github/scripts/meg-am/ndvi/src"

#### Set working directory, load libraries and set parallelization #############
dsn <- switch(Sys.info()[["sysname"]], 
              "Linux" = "/media/permanent/",
              "Windows" = "D:/")
setwd(paste0(dsn, input.path))

lib <- c("raster", "rgdal", "MODIS", "doParallel", "Kendall", "RColorBrewer")
sapply(lib, function(...) require(..., character.only = TRUE))

source(paste0(dsn, src.filepath, "/tsOutliers.R"))

registerDoParallel(cl <- makeCluster(2))


#### Download MODIS NDVI data sets #############################################
dir.create(paste0(dsn, output.path), showWarnings = FALSE)
MODISoptions(localArcPath = paste0(dsn, input.path, "/level_3"), 
             outDirPath = paste0(dsn, input.path, "/level_3_processed"),
             gdalPath = "C:/OSGeo4W/bin")

## orgStruc(from = paste0(dsn, input.path, "/level_3"), move = TRUE)

## for (i in c("MOD13Q1", "MYD13Q1"))
for (i in c("MYD13Q1"))
  runGdal(i, begin = "2001-01-01",
          tileH = 18, tileV = 3, SDSstring = "100000000001", 
          outProj = "EPSG:32737", job = "outProj")


## Geographic extent

kili <- data.frame(x = c(37, 37.72), y = c(-3.4, -2.84), id = c("ll", "ur"))
coordinates(kili) <- c("x", "y")
projection(kili) <- CRS("+init=epsg:4326")
kili <- spTransform(kili, CRS("+init=epsg:32737"))


## DEM

dem <- raster("data/kili_dem_utm.tif")


## NDVI data

stats <- foreach(h = c("MOD13Q1", "MYD13Q1"), .packages = lib) %dopar% {
  
  pttrn <- paste(h, c("NDVI.tif$", "pixel_reliability.tif$"), sep = ".*")
  
  # ndvi.rst <- lapply(pttrn, function(i) {
  #   # List available files
  #   fls <- list.files("data/MODIS_ARC/PROCESSED/outProj", 
  #                          pattern = i, full.names = TRUE)  
  #   # Stack and crop files
  #   rst <- stack(fls)
  #   rst.crp <- crop(rst, extent(kili))
  #   writeRaster(rst.crp, filename = "data/processed/CRP", format = "GTiff", 
  #               bylayer = TRUE, suffix = names(rst), overwrite = TRUE)
  #   return(rst.crp)
  # })
  
  fls.ndvi <- list.files("data/MODIS_ARC/PROCESSED/outProj/", 
                         pattern = pttrn[1], full.names = TRUE)
  fls.pr <- list.files("data/MODIS_ARC/PROCESSED/outProj/", 
                       pattern = pttrn[2], full.names = TRUE)
  
  ndvi.rst <- lapply(pttrn, function(i) {
    fls <- list.files("data/processed/", pattern = i, full.names = TRUE)
    stack(fls)
  })
  
  # Rejection of low quality cells
  # ndvi.rst.qa <- overlay(ndvi.rst[[1]], ndvi.rst[[2]], fun = function(x, y) {
  #   x[!y[] %in% c(0:2)] <- NA
  #   return(x)
  # })
  # writeRaster(ndvi.rst.qa, filename = "data/processed/QA", format = "GTiff", 
  #             bylayer = TRUE, suffix = names(ndvi.rst[[1]]), overwrite = TRUE)
  
  ndvi.fls.qa <- list.files("data/processed/", full.names = TRUE, 
                            pattern = paste("QA", pttrn[1], sep = ".*"))
  ndvi.rst.qa <- stack(ndvi.fls.qa)
  
  # Application of outlier check
  #   ndvi.rst.qa.sd <- calc(ndvi.rst.qa, fun = function(x) {
  #     id <- tsOutliers(x, lower.limit = .4, upper.limit = .9, index = TRUE)
  #     x[id] <- NA
  #     return(x)
  #   })
  #   writeRaster(ndvi.rst.qa.sd, filename = "data/processed/SD", format = "GTiff", 
  #               bylayer = TRUE, suffix = names(ndvi.rst.qa), overwrite = TRUE)
  
  ndvi.fls.qa.sd <- list.files("data/processed/", full.names = TRUE, 
                               pattern = paste("^SD", pttrn[1], sep = ".*"))
  ndvi.rst.qa.sd <- stack(ndvi.fls.qa.sd)
  
  # Rejection of pixels surrounding cloudy cells
  #     ndvi.rst.qa.sd.fc <- foreach(i = unstack(ndvi.rst.qa.sd), .combine = "stack", 
  #                                  .packages = lib) %dopar% {
  #       cells <- which(is.na(i[]))
  #       id <- adjacent(i, cells = cells, directions = 8, pairs = FALSE)
  #       i[id] <- NA
  #       return(i)
  #     }
  #     writeRaster(ndvi.rst.qa.sd.fc, filename = "data/processed/BF", format = "GTiff", 
  #                 bylayer = TRUE, suffix = names(ndvi.rst.qa.sd), overwrite = TRUE)
  
  ndvi.fls.qa.sd.fc <- list.files("data/processed/", full.names = TRUE, 
                                  pattern = paste("BF", pttrn[1], sep = ".*"))
  ndvi.rst.qa.sd.fc <- stack(ndvi.fls.qa.sd.fc)
  
  
  
  ### Gap filling
  
#   whittaker.raster(ndvi.rst.qa.sd.fc, timeInfo = orgTime(fls.ndvi), 
#                    lambda = 6000, nIter = 3, groupYears = TRUE, 
#                    outDirPath = paste0("data/processed/whittaker_", tolower(h)), 
#                    overwrite = TRUE)
  
  fls.wht <- list.files(paste0("data/processed/whittaker_", tolower(h)), 
                        pattern = "NDVI_Year.*_year.*.tif$", full.names = TRUE)
  fls.wht <- fls.wht[grep("2003", fls.wht):grep("2013", fls.wht)]
  rst.wht <- stack(fls.wht)
  
#   rst.mk <- overlay(rst.wht, fun = function(x) MannKendall(as.numeric(x))$tau, 
#                     filename = paste("out/MK", toupper(h), unique(substr(names(rst.wht), 1, 21)), sep = "_"), 
#                     format = "GTiff", overwrite = TRUE)
  
  fls.mk <- list.files("out", pattern = paste0("MK_", toupper(h), ".*.tif$"), 
                       full.names = TRUE)
  rst.mk <- raster(fls.mk)
  
  png(paste0(substr(fls.mk, 1, nchar(fls.mk)-4), ".png"), units = "mm", 
      width = 300, res = 300, pointsize = 20)
  print(spplot(rst.mk, scales = list(draw = TRUE), xlab = "x", ylab = "y", 
         col.regions = colorRampPalette(brewer.pal(11, "BrBG")), 
         sp.layout = list("sp.lines", rasterToContour(dem)), 
         par.settings = list(fontsize = list(text = 15)), at = seq(-.9, .9, .1)))
  dev.off()
  
  stats <- lapply(c(.01, .001), function(i) {
#     rst.mk <- overlay(rst.wht, fun = function(x) {
#       mk <- MannKendall(as.numeric(x))
#       if (mk$sl >= i) return(NA) else return(mk$tau)
#     }, filename = paste("out/MK", i, toupper(h), 
#                         unique(substr(names(rst.wht), 1, 21)), sep = "_"), 
#     format = "GTiff", overwrite = TRUE)
    
    fls.mk <- list.files("out", pattern = paste(i, toupper(h), ".tif$", sep = ".*"), 
                         full.names = TRUE)
    rst.mk <- raster(fls.mk)
    
    png(paste0(substr(fls.mk, 1, nchar(fls.mk)-4), ".png"), units = "mm", 
        width = 300, res = 300, pointsize = 20)
    print(spplot(rst.mk, scales = list(draw = TRUE), xlab = "x", ylab = "y", 
                 col.regions = colorRampPalette(brewer.pal(11, "BrBG")), 
                 sp.layout = list("sp.lines", rasterToContour(dem)), 
                 par.settings = list(fontsize = list(text = 15)), 
                 at = seq(-.9, .9, .1)))
    dev.off()
    
    val <- round(sum(!is.na(rst.mk[]))/ncell(rst.mk), digits = 3)
    
    val.pos <- round(sum(rst.mk[] > 0, na.rm = TRUE) / sum(!is.na(rst.mk[])), 3)
    val.neg <- round(sum(rst.mk[] < 0, na.rm = TRUE) / sum(!is.na(rst.mk[])), 3)
    
    return(data.frame(sensor = h, p = as.character(i), nona = val, 
                      nona_pos = val.pos, nona_neg = val.neg))
  })
  
  return(do.call("rbind", stats))
}

# Store percentage information about significant NDVI pixels
write.csv(do.call("rbind", stats), "out/mk_na_stats.csv", row.names = FALSE)

# Remove white margins from output images
system("cd out/; for file in *.png; do convert -trim $file $file; done")

# Deregister parallel backend
stopCluster(cl)
