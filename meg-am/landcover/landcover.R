# Compare a cassified raster data set with validation polygons.
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
input.path <- "active/moc/am/data/landcover"
rasterfile <- "maxlike.tif"
polygonfile <- "training_areas.shp"
sources <- "active/moc/github/scripts/meg-am/landcover"

#### Set working directory, load libraries and set parallelization #############
library(rgdal)
library(raster)
library(irr)
source(paste0(dsn, sources, "/calculateKappa.R"))

dsn <- switch(Sys.info()[["sysname"]], 
              "Linux" = "/media/permanent/",
              "Windows" = "D:/")
setwd(paste0(dsn, input.path))


#### Prepare data ##############################################################

prediction <- raster(rasterfile, native = TRUE)
reference <- readOGR(polygonfile, layer = "training_areas")

overlay <- extract(prediction, reference)
# save(overlay, file = "overlay.RData")
load("overlay.RData")

test <- do.call("rbind", lapply(1:length(reference@data$id), function(x) {
  dat <- data.frame(reference@data$id[x], overlay[x])
  names(dat) <- c("reference", "prediction")
  return(dat)
}))

contingencytable <- ftable(test)
print(contingencytable)

#### Compute statistics ########################################################

kappa2(contingencytable)

# Some more...
calculateKappa(contingencytable)

