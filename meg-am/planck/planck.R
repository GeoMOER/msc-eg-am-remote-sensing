library(latticeExtra)

planck <- function(l, t){
  h <- 6.62606957E???34 # J/s
  kb <- 1.3806488E???23 # J/K
  c <- 299792458 # m/s
  l = l * 1E-9 # nm to m
  B <- (2 * h * c^2) / l^5 / (exp((h * c)/(l * kb * t)) - 1)
}

data <- do.call("rbind", lapply(c(288, 3000, 4000, 5000), function(x){
  do.call("rbind",lapply(seq(3000), function(y){
    data.frame(Lambda = y,
               Temp = x,
               SR = planck(y, x))
  }))
}))

xyplot(SR ~ Lambda, data = data, groups = Temp, 
       main = "Planck function", xlab = "Wavelength (nm)", ylab = "Spectral Radiance (W/m2/nm/sr)",
       auto.key = TRUE)



