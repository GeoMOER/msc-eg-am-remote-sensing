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
input.path <- "active/moc/am/data/speclib/ASCII"
output.path <- "active/moc/am/data/speclib"

#### Set working directory and load libraries ##################################
dsn <- switch(Sys.info()[["sysname"]], 
              "Linux" = "/media/permanent/",
              "Windows" = "D:/")
setwd(paste0(dsn, input.path))

library(latticeExtra)

#### Read spectral data and plot results #######################################
datasets <- list.files(path = ".", 
                       pattern =  glob2rx("*.asc"),
                       full.names = TRUE, recursive = TRUE)
datasets.sub <- subset(datasets, grepl("grass", datasets) | 
                         grepl("oak", datasets) | grepl("cedar", datasets) | 
                         grepl("spruce", datasets)| grepl("rangeland", datasets))
datasets.sub <- subset(datasets, grepl("grass", datasets))
datasets.sub <- subset(datasets, 
                       grepl("water_mont_mix_a.27324.asc", datasets) |
                         grepl("lawn_grass_gds91b.31090.asc", datasets) |
                         grepl("asphalt_gds366.27407.asc", datasets))

x <- datasets.sub[11]
layers <- lapply(datasets.sub,function(x){
  act.filepath <- x
  print(act.filepath)
  act.data <- read.table(act.filepath, sep = " ", skip = 16, fill = TRUE)
  act.data <- act.data[,colSums(is.na(act.data))<nrow(act.data)]
  colnames(act.data) <- c("wavelength", "reflectance", "standard deviation")
  act.layer <- xyplot(act.data$reflectance ~ act.data$wavelength)
  return(act.layer)
})
layers[1]
layers[2]


str(layers)
layers[1][[1]] + as.layer(layers[2][[1]])

