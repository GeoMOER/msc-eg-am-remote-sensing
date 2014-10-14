# Analyse vegetation data
#
#  Copyright (C) 2014 Alice Ziegler, Jürgen Kluge, Thomas Nauss
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
dat1file <- "Arten_Plots_R_kompatibel_Plots_senkr.txt"
dat2file <- "Arten_Plots_R_kompatibel_infos.txt"
outputcsvfile <- "Arten_Plots_Cluster.txt"
templateshpfile <- "alt/Plot_Points_all_Cluster.shp"
outputshpfile <- "Plot_Points_BD"
input.path <- ("active/moc/am/data/vegetation")

#### Set working directory, load libraries and set parallelization #############
dsn <- switch(Sys.info()[["sysname"]], 
              "Linux" = "/media/permanent/",
              "Windows" = "D:/")
setwd(paste0(dsn, input.path))

library(vegan)
library(rgdal)
library(sp)
library(raster)

#### Read, combine and analyse the data sets by vegetation type ################
dat1 <- read.table(dat1file, header=T)
dat2 <- read.table(dat2file,header=T)
dat12 <- cbind(dat1, dat2)

# Cluster all major land covers types individually
analysis <- lapply(seq(1:3), function(x){
  print(x)
  datact <- dat12[dat12$Nutzung_num==x,]
  datact <- datact[,which(colSums(datact[,1:61])>0)]
  datact.dca <- decorana(datact)
  # plot(datact.dca, display="sites")
  datact.vegdist <- vegdist(datact, method="bray", binary = FALSE)
  datact.hclust <- hclust(datact.vegdist, method = "ward")
})

# Combine grass and mixed orchards in an additional analysis
datwiese <- dat12[dat12$Nutzung_num==2 | dat12$Nutzung_num==3,]
datwiese <- datwiese[,which(colSums(datwiese[,1:61])>0)]
datwiese.cca <-cca(datwiese)
# summary(datwiese.cca)
# plot(datwiese.cca, display="sites")
# datwiese.cca.eig<-datwiese.cca$CA$eig
# sum(datwiese.cca.eig)
# datwiese.cca.expl <-datwiese.cca.eig*100/sum(datwiese.cca.eig)
# barplot(datwiese.cca.expl)
datwiese.dca <- decorana(datwiese)
# plot(datwiese.dca, display="sites")
# points(datwiese.dca,display="sites",pch=21,bg=grey(0.3),cex=2)
# ef <- envfit(datwiese.dca, datwiese)
# plot(ef)
datwiese.vegdist <- vegdist(datwiese, method="bray", binary = FALSE)
datwiese.hclust <- hclust(datwiese.vegdist, method = "ward")

# Combine individual land cover analysis with combined grass/mixed orchard data
analysis[[length(analysis)+1]] <- datwiese.hclust
names(analysis) <- list("Wald", "Wiese", "Streuobstwiese", "KombiWiese")

# Group forest clustering results
plot(analysis[[1]], main = paste0("Cluster Dendrogram ", names(analysis)[1]))
rect.hclust(analysis[[1]], 2)
wald.clust <- cutree(analysis[[1]], k = 2)
wald.clust <- data.frame(PlotID = names(wald.clust), CID = wald.clust)

# Group grass clustering results
plot(analysis[[2]], main = paste0("Cluster Dendrogram ", names(analysis)[2]))
rect.hclust(analysis[[2]], 2)
wiese.clust <- cutree(analysis[[2]], k = 2)
wiese.clust <- data.frame(PlotID = names(wiese.clust), CID = wiese.clust)

# # Group mixed orchard clustering results
plot(analysis[[3]], main = paste0("Cluster Dendrogram ", names(analysis)[3]))
rect.hclust(analysis[[3]], 2)
streuobstwiese.clust <- cutree(analysis[[3]], k = 2)
streuobstwiese.clust <- data.frame(PlotID = names(streuobstwiese.clust), CID = streuobstwiese.clust)

# Group grass/mixed orchard clustering results
plot(analysis[[4]], main = paste0("Cluster Dendrogram ", names(analysis)[4]))
rect.hclust(analysis[[4]], 4)
kombiwiese.clust <- cutree(analysis[[4]], k = 4)
kombiwiese.clust <- data.frame(PlotID = names(kombiwiese.clust), KomCID = kombiwiese.clust)
              
# Combine groups into one data set (use dat 12 for that)
dat12$PlotID <- row.names(dat12)
merge.clust <- merge(rbind(wald.clust, wiese.clust, streuobstwiese.clust), kombiwiese.clust, by = "PlotID", all = TRUE)
merge.all <- merge(dat12, merge.clust, by = "PlotID", all = TRUE)
str(merge.all)
merge.all$CID <- paste0(substr(merge.all$Nutzung_einheitlich, 1, 2), as.character(merge.all$CID))
merge.all$KomCID[!is.na(merge.all$KomCID)] <- paste0("WS", merge.all$KomCID[!is.na(merge.all$KomCID)])
merge.all$KomCID[is.na(merge.all$KomCID)] <- merge.all$CID[is.na(merge.all$KomCID)]
# merge.all <- within(merge.all, KomCID[is.na(merge.all$KomCID)] <- CID[is.na(merge.all$KomCID)])

# Write data set to csv file
write.table(merge.all, file = outputcsvfile, quote = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)

# Write data set as ESRI shape data file
sdf <- readOGR(templateshpfile, layer = "Plot_Points_all_Cluster")
sdf$PlotID <- as.character(sdf$NAME)
sdf.merge <-merge(merge.all, sdf, by = "PlotID")
coordinates(sdf.merge) <- ~coords.x1 + coords.x2
projection(sdf.merge) <- projection(sdf)
writeOGR(sdf.merge, outputshpfile, outputshpfile, driver="ESRI Shapefile")

