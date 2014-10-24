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
input.path <- "active/moc/am/data/vegetation"
vegdatalayer <- "Plot_Points_BD_RS"
vegdatafile <- paste0(vegdatalayer, ".shp")


#### Set working directory, load libraries and set parallelization #############
dsn <- switch(Sys.info()[["sysname"]], 
              "Linux" = "/media/permanent/",
              "Windows" = "D:/")
setwd(paste0(dsn, input.path))

library(rgdal)
library(sp)
library(raster)
library(party)
library(rpart)
library(randomForest)
library(caret)
library(corrplot)

#### Read vegetation and remote sensing data file ##############################
vegdata <- readOGR(vegdatafile, layer = vegdatalayer)

# Subset data according to desired data sets
vegdata <- data.frame(vegdata[,c(63, 65:66, 157:178)])
vegdata <- vegdata[,-c((ncol(vegdata)-1):ncol(vegdata))]

#### Q&D analysis using classification trees ###################################
vd <- vegdata[,c(3,4:ncol(vegdata))]
vd$KmCID_x <- factor(vd$KmCID_x)
# vd$Ntzng_nht <- factor(vd$Ntzng_nht)
# vd <- vd[vd$Ntzng_nht != "Sonstiges",]
# vd$Ntzng_nht <- factor(vd$Ntzng_nht)


# party library
vd.ctree <- ctree(Ntzng_nht ~ ., data = vd,
                  controls = ctree_control(minsplit = 2, minbucket = 1, 
                                           mincriterion = 0.5))
vd.ctree <- ctree(KmCID_x ~ ., data = vd,
                  controls = ctree_control(minsplit = 2, minbucket = 1, 
                                           mincriterion = 0.5))
plot(vd.ctree)  

# rpart library
vd.rpart <- rpart(KmCID_x ~ ., data = vd, method="class",
                 control = rpart.control(minsplit = 5))
plot(vd.rpart, uniform=TRUE, main="Classification tree for botany data set")
text(vd.rpart, use.n=TRUE, all=TRUE, cex=0.75)
printcp(vd.rpart)
summary(vd.rpart)

vd.prune <- prune(vd.rpart, cp = vd.rpart$cptable[which.min(vd.rpart$cptable[,"xerror"]),"CP"])
plot(vd.prune, uniform=TRUE, main="Classification tree for botany data set")
text(vd.prune, use.n=TRUE, all=TRUE, cex=0.5)






#### Analysis using random forest ##############################################


# Analysis using functionallity of caret package
df <- vdVEG
df <- vdCID
df <- vd
df <- df[!(df$KmCID_x=="SoNA"), ]
df$KmCID_x <- factor(df$KmCID_x)

featurePlot(x = df[,-1], y = df[,1], plot = "pairs")
featurePlot(x = df[,-1], y = df[,1], plot = "density")

df.cor <- as.matrix(cor(df[,-1]))
corrplot(df.cor)
cor.rm <- findCorrelation(df.cor, verbose = TRUE)
df.clean <- df[,-(cor.rm+1)]

test <- lapply(seq(1:5), function(x){
  trainIndex <- createDataPartition(df.clean[,1], p = 0.7, list = FALSE)
  df.train <- df.clean[trainIndex,]
  df.test <- df.clean[-trainIndex,]
  
  rf.train <- train(df.train[,-1], df.train[,1], method = "rf", tuneLength = 10)
  plot(rf.train)
  varImpPlot(rf.train$finalModel)
  rf.test <- predict(rf.train, df.test[,-1])
  rf.prob <- predict(rf.train, df.test[,-1], type="prob")
  result <- data.frame(PRED = rf.prob,
                       VALD = df.test[,1])  
})
test <- do.call("rbind", test)

# confusionMatrix(table(rf.test, df.test[,1]))
confusionMatrix(table(test))




# randomForest library
rf <- tuneRF(vdCID[,-1], vdCID[,1], ntreeTry=1000, 
             stepFactor=1, improve=0.05, doBest=TRUE)
rfCID <- randomForest(CID ~ ., data=vdCID, importance = TRUE, mtry=4)
print(rfCID) # view results
rfCID$importance
importance(rfCID)
varImpPlot(rfCID, type = 1, n.var = 4)

rfKomCID <- randomForest(KomCID ~ ., data=vdKomCID, importance = TRUE)
print(rfKomCID) # view results
rfKomCID$importance
importance(rfKomCID)
varImpPlot(rfKomCID, type = 1, n.var = 4)
