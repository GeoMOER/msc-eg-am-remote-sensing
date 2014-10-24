# Extract user defined pixels from NDVI time series and plot it.
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
input.path <- "active/moc/am/data/modis/level_3_processed/"

#### Set working directory, load libraries and set parallelization #############
dsn <- switch(Sys.info()[["sysname"]], 
              "Linux" = "/media/permanent/",
              "Windows" = "D:/")
setwd(paste0(dsn, input.path))
level3.wts.path <- paste0(dsn, input.path, "wts/")

lib <- c("raster", "latticeExtra", "ts")
sapply(lib, function(...) require(..., character.only = TRUE))


#### Load NDVI time series and extract values for user defined coordinates #####
fls.wht <- list.files(level3.wts.path, pattern = "NDVI_Year.*_year.*.tif$", 
                      full.names = TRUE)
fls.wht <- fls.wht[grep("2004", fls.wht):grep("2013", fls.wht)]
rst.wht <- stack(fls.wht)

urban.xy <- cbind(483555, 5628006)
urban <- SpatialPoints(urban.xy)
projection(urban) <- projection(rst.wht)

forest.xy <- cbind(486221, 5626004)
forest <- SpatialPoints(forest.xy)
projection(forest) <- projection(rst.wht)

grass.xy <- cbind(480716, 5625568)
grass <- SpatialPoints(grass.xy)
projection(grass) <- projection(rst.wht)

urban.wht <- c(extract(rst.wht, urban))
forest.wht <- c(extract(rst.wht, forest))
grass.wht <- c(extract(rst.wht, grass))

ndvi <- data.frame(urban = urban.wht,
                   forest = forest.wht,
                   grass = grass.wht)

#### Create time series object and visualize data ##############################

ndvi.ts <- ts(ndvi, start=2004, end=2013, frequency = 23)
xyplot(ndvi.ts)