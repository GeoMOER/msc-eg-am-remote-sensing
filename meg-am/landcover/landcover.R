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

dsn <- switch(Sys.info()[["sysname"]], 
              "Linux" = "/media/permanent/",
              "Windows" = "D:/")
source(paste0(dsn, sources, "/calculateKappa.R"))
setwd(paste0(dsn, input.path))


#### Prepare data ##############################################################

prediction <- raster(rasterfile, native = TRUE)
reference <- readOGR(polygonfile, layer = "training_areas")

# overlay <- extract(prediction, reference)
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

kappa2(test)

# Some more...
calculateKappa(contingencytable)

ct <- contingencytable
g <- nrow(ct)
s <- sum(ct)
ct <- ct/sum(ct)
##################
P0 <- 0
for (i in 1:g){
  P0 <- P0 + ct[i,i]
}
P0 <- P0 / s
##################

PE <- 0
for (i in 1:g){
  colsum <- 0
  rowsum <- 0
  for (j in 1:g) {
    colsum <- colsum + ct[j,i]
    rowsum <- rowsum + ct[i,j]
  }
  PE <- PE + colsum * rowsum
}
PE <- PE / s**2

(P0 - PE)/(1-PE)

PE <- 0
for (i in 1:g){
  PE <- PE + sum(ct[,i]) * sum(ct[i,])
  
  colsum * rowsum
}
PE <- PE / s**2




nrow(contingencytable)
PA = 0
for (i in 1:nrow(contingencytable)){
  PA = PA + contingencytable[i,i]
}
PA = PA / sum(contingencytable)

PE = 0
for (i in 1:nrow(contingencytable)){
  PE = PE + sum(contingencytable[i,]) * sum(contingencytable[,i])
}
PE = PE / sum(contingencytable)**2

(PA - PE)/(1-PE)

Q = 0
for (i in 1:nrow(contingencytable)){
  Q = Q + abs(sum(contingencytable[i,]) - sum(contingencytable[,i]))
}
Q = Q / sum(contingencytable) / 2
1-Q

PMax=0
for (i in 1:cmax) {
  PMax=PMax+min(sum(ct[i,]),sum(ct[,i]))
}  
