# Convert LAS data from HVBG, ETRS89/UTM 32 north, to a txt file and create
# ESRI shape file subsets containing only ground and surface points. 
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
input.path <- "active/moc/am/data/lidar/level_1"
output.path <- "active/moc/am/data/lidar/level_2"
src.filepath <- "active/moc/github/scripts/meg-am/lidar/src"
liblas.path <- "C:/OSGeo4w/bin"

compute.grid <- FALSE


#### Set working directory and load libraries ##################################
dsn <- switch(Sys.info()[["sysname"]], 
              "Linux" = "/media/permanent/",
              "Windows" = "D:/")
input.path <- paste0(dsn, input.path)
output.path <- paste0(dsn, output.path)
setwd(input.path)

library(rgdal)
library(raster)

for (i in c("las2txt.R", "subsetLidar.R"))
  source(paste0(dsn, src.filepath, "/", i))


#### Convert and subset las data ###############################################
# Convert las to txt files containing pre-defined subsets and also create
# gridded data sets, if requested.
datasets <- list.files(path = input.path, 
                       pattern =  glob2rx("*.las"),
                       full.names = TRUE, recursive = TRUE)

sapply(datasets,function(x){
  act.filepath <- x
  print(paste0("Converting ", act.filepath))
  las.data <- las2txt(act.filepath, output.path, liblas.path, 
                      create.shape = TRUE, return.data = TRUE)  
  subsetLidar(las.data, output.path)
})
