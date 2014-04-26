getCalibCoef <- function(filepath, band, coef = "rad", sensor){
  # Get calibration coefficients from Landsat 8 standard metadata file.
  #
  # Args:
  #   filepath: path and filename to the landsat metadata file
  #   band: band number for which the coefficients should be retrieved
  #   coef: either "rad" or "ref" for radiance/reflectance coefficients
  #
  # Returns:
  #   Vector containing 
  #   - multiplication coefficient [1]
  #   - addition coefficient [2]
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
  cal.data <- read.table(filepath, header = FALSE, sep = "=", fill = TRUE)
  
  if(sensor == "landsat"){
    if(coef == "ref"){
      search.term.mult <- paste0("REFLECTANCE_MULT_BAND_", band)
      search.term.add <- paste0("REFLECTANCE_ADD_BAND_", band)
    } else {
      search.term.mult <- paste0("RADIANCE_MULT_BAND_", band)
      search.term.add <- paste0("RADIANCE_ADD_BAND_", band)
    }
    cal.mult <- as.numeric(as.character(
      (subset(cal.data$V2, gsub("\\s","", cal.data$V1) == search.term.mult))))
    cal.add <- as.numeric(as.character(
      (subset(cal.data$V2, gsub("\\s","", cal.data$V1) == search.term.add))))
  } else if(sensor == "hyperion"){
    band <- as.numeric(band)
    if(band >= 8 & band <= 57){
      cal.mult <- 1.0/40.0
    } else if(band >= 77 & band <= 224){
      cal.mult <- 1.0/80.0  
    } else {
      cal.mult <- 0.0
    }
    cal.add <- 0.0
  } 
  result <- c(cal.mult, cal.add)
  attr(result, "Info") <- c("CalMult", "CalAdd")
  return(result)
}