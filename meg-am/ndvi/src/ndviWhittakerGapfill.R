# Compute trend from croped, quality corrected and gap-filled NDVI series
#
#  Copyright (C) 2014 Florian Detsch, Thomas Nauss
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
dem.filepath <- "active/moc/am/data/dgm10/dgm10_level_4/dgm10_mr.tif"

#### Set working directory, load libraries and set parallelization #############
dsn <- switch(Sys.info()[["sysname"]], 
              "Linux" = "/media/permanent/",
              "Windows" = "D:/")
setwd(paste0(dsn, input.path))
level3.path <- paste0(dsn, input.path, "/level_3")
level3.processed.path <- paste0(dsn, input.path, "/level_3_processed")
level3.crop.path <- paste0(dsn, input.path, "level_3_processed/crop/")
level3.qa.path <- paste0(dsn, input.path, "level_3_processed/qa/")
level3.olc.path <- paste0(dsn, input.path, "level_3_processed/olc/")
level3.cldv.path <- paste0(dsn, input.path, "level_3_processed/cldv/")
level3.wts.path <- paste0(dsn, input.path, "level_3_processed/wts/")
level3.mk.path <- paste0(dsn, input.path, "level_3_processed/mk/")
dem.filepath <- paste0(dsn, dem.filepath)

lib <- c("raster", "rgdal", "MODIS", "Kendall", "RColorBrewer")
sapply(lib, function(...) require(..., character.only = TRUE))

source(paste0(dsn, src.filepath, "/tsOutliers.R"))


#### Download MODIS NDVI data sets #############################################
dir.create(paste0(dsn, output.path), showWarnings = FALSE)
MODISoptions(localArcPath = level3.path, 
             outDirPath = level3.processed.path,
             gdalPath = "C:/OSGeo4W/bin")

## orgStruc(from = paste0(dsn, input.path, "/level_3"), move = TRUE)

## for (i in c("MOD13Q1", "MYD13Q1"))
for (i in c("MYD13Q1"))
  runGdal(i, begin = "2001-01-01",
          tileH = 18, tileV = 3, SDSstring = "100000000001", 
          outProj = "EPSG:32632", job = "outProj")


#### Crop and calibrate MODIS NDVI data sets ###################################
marburg <- data.frame(x = c(474285.0, 496485.0), y = c(5616015.0, 5638215.0), 
                      id = c("ll", "ur"))
coordinates(marburg) <- c("x", "y")
projection(marburg) <- CRS("+init=epsg:32632")

pttrn <- c("*NDVI.tif", "*pixel_reliability.tif")
ndvi.rst <- lapply(pttrn, function(i) {
  fls <- list.files(paste0(level3.processed.path, "/outProj"), 
                    pattern = i, full.names = TRUE)
  rst <- stack(fls)
  rst.crp <- crop(rst, extent(marburg))
  rst.crp <- rst.crp / 10000.0
  dir.create(level3.crop.path, showWarnings = FALSE)
  writeRaster(rst.crp, filename = paste0(level3.crop.path, "crop"), 
              format = "GTiff", bylayer = TRUE, suffix = names(rst),
              overwrite = TRUE)
  return(rst.crp)
})
# ndvi.rst <- lapply(pttrn, function(i) {
#   fls <- list.files(level3.crop.path, pattern = i, full.names = TRUE)
#   stack(fls)
# })


#### Reject low quality pixels #################################################
ndvi.rst.qa <- overlay(ndvi.rst[[1]], ndvi.rst[[2]], fun = function(x, y) {
  x[!y[] %in% c(0:2)] <- NA
  return(x)
})
dir.create(level3.qa.path, showWarnings = FALSE)
writeRaster(ndvi.rst.qa, filename = paste0(level3.qa.path, "qa"), 
            format = "GTiff", bylayer = TRUE, suffix = names(ndvi.rst[[1]]), 
            overwrite = TRUE)
# ndvi.fls.qa <- list.files(level3.qa.path, full.names = TRUE, 
#                           pattern = "*.tif")
# ndvi.rst.qa <- stack(ndvi.fls.qa)


#### Outlier check #############################################################
ndvi.rst.qa.sd <- calc(ndvi.rst.qa, fun = function(x) {
  id <- tsOutliers(x, lower.limit = 0.01, upper.limit = 0.99, index = TRUE)
  x[id] <- NA
  return(x)
})
dir.create(level3.olc.path, showWarnings = FALSE)
writeRaster(ndvi.rst.qa.sd, filename = paste(level3.olc.path, "olc"), 
            format = "GTiff", bylayer = TRUE, suffix = names(ndvi.rst[[1]]), 
            overwrite = TRUE)
# ndvi.fls.qa.sd <- list.files(level3.olc.path, full.names = TRUE, 
#                              pattern = "*.tif")
# ndvi.rst.qa.sd <- stack(ndvi.fls.qa.sd)


#### Cloud vicinity check ######################################################
ndvi.rst.qa.sd.fc <- stack(lapply(unstack(ndvi.rst.qa.sd), function(x){
  cells <- which(is.na(x[]))
  id <- adjacent(x, cells = cells, 
                 directions = 8, pairs = FALSE)
  x[id] <- NA
  return(x)
}))
dir.create(level3.cldv.path, showWarnings = FALSE)
writeRaster(ndvi.rst.qa.sd.fc, filename = paste0(level3.cldv.path, "cldv"), 
            format = "GTiff", bylayer = TRUE, suffix = names(ndvi.rst[[1]]), 
            overwrite = TRUE)
# ndvi.fls.qa.sd.fc <- list.files(level3.cldv.path, full.names = TRUE, 
#                                 pattern = "*.tif")
# ndvi.rst.qa.sd.fc <- stack(ndvi.fls.qa.sd.fc)


#### Gap filling ###############################################################
fls.ndvi <- list.files(paste0(level3.processed.path, "/outProj"), 
                              pattern = pttrn[1], full.names = TRUE)
dir.create(level3.wts.path, showWarnings = FALSE)
whittaker.raster(ndvi.rst.qa.sd.fc, timeInfo = orgTime(fls.ndvi), 
                 lambda = 6000, nIter = 3, groupYears = TRUE, 
                 outDirPath = level3.wts.path, overwrite = TRUE)
fls.wht <- list.files(level3.wts.path, pattern = "NDVI_Year.*_year.*.tif$", 
                      full.names = TRUE)
fls.wht <- fls.wht[grep("2003", fls.wht):grep("2013", fls.wht)]
rst.wht <- stack(fls.wht)


#### Compute trend #############################################################
dir.create(level3.mk.path, showWarnings = FALSE)
sig <- c("100", "010", "005", "001")
rst.mk <- sapply(sig, function(x){
  act.sig.char <- x
  act.mk <- overlay(rst.wht, fun = function(y){
    mk <- MannKendall(as.numeric(y))
    act.sig <- as.numeric(act.sig.char) / 100.0
    if (mk$sl >= act.sig) return(NA) else return(mk$tau)
  }, filename = paste0(level3.mk.path, unique(substr(names(rst.wht), 1, 21)), 
                       "_", act.sig.char), format = "GTiff", overwrite = TRUE)
  return(act.mk)
})
# fls.mk <- list.files(level3.mk.path, pattern = "*.tif", full.names = TRUE)
# rst.mk <- raster(fls.mk)


#### Visualization #############################################################
dem <- raster(dem.filepath)
  
png(paste0(substr(fls.mk, 1, nchar(fls.mk)-4), ".png"), units = "mm", 
    width = 300, res = 300, pointsize = 20)
print(spplot(rst.mk, scales = list(draw = TRUE), xlab = "x", ylab = "y", 
             col.regions = colorRampPalette(brewer.pal(11, "BrBG")), 
             sp.layout = list("sp.lines", rasterToContour(dem)), 
             par.settings = list(fontsize = list(text = 15)), at = seq(-.9, .9, .1)))
dev.off()

stats <- lapply(c(.01, .001), function(i) {
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
stats.df <- do.call("rbind", stats)

# Store percentage information about significant NDVI pixels
write.csv(stats.df, "mk_na_stats.csv", row.names = FALSE)

# Remove white margins from output images
system("for file in *.png; do convert -trim $file $file; done")
