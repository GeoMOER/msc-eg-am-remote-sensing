Classification of datasets using decission trees
========================================================
The following example illustrates the classification of the iris data set which
is part of the R base installation. It gives measurements of sepal length, sepal
width, petal length and petal width for 50 flowers of the species Iris setosa, 
Iris versicolor, and Iris virginica. 

In order to make visual things easier, Iris setosa is abbrevated as "S", 
Iris versicolor as "C", and Iris virginica as "G". This is what is done in the
following lines.


```r
data(iris)
iris$Label <- substr(iris$Species, 1, 3)
iris$Label <- replace(iris$Label, which(iris$Label == "set"), "S")
iris$Label <- replace(iris$Label, which(iris$Label == "ver"), "C")
iris$Label <- replace(iris$Label, which(iris$Label == "vir"), "G")
```


Since we want to predict the species based on the width/length variables, let's
first have a look if we can identify seperated clusters of classes. For 
simplicity, only the case of sepal length vs. width and petal legnth vs. width
is visualized.

```r
library(latticeExtra)
```

```
## Loading required package: RColorBrewer
## Loading required package: lattice
```

```r

xyplot(Sepal.Length ~ Sepal.Width, data = iris, groups = iris$Label, pch = 15, 
    panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        ltext(x = x, y = y, labels = iris$Label, pos = 1, offset = 1, cex = 1)
    })
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-21.png) 

```r

xyplot(Petal.Length ~ Petal.Width, data = iris, groups = iris$Label, pch = 15, 
    main = "Iris species as function of petal width and length", panel = function(x, 
        y, ...) {
        panel.xyplot(x, y, ...)
        ltext(x = x, y = y, labels = iris$Label, pos = 1, offset = 1, cex = 1)
    })
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-22.png) 


While the sepal parameters do not clearly separate the species, the pental 
measurements show a very good seperation between S and C and G while the latter
two species are overlaping to some extend.

Let's see how a decission tree performs on that data set.

```r
library(rpart)
pred <- rpart(Label ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
    data = iris, method = "class")
printcp(pred)
```

```
## 
## Classification tree:
## rpart(formula = Label ~ Sepal.Length + Sepal.Width + Petal.Length + 
##     Petal.Width, data = iris, method = "class")
## 
## Variables actually used in tree construction:
## [1] Petal.Length Petal.Width 
## 
## Root node error: 100/150 = 0.67
## 
## n= 150 
## 
##     CP nsplit rel error xerror  xstd
## 1 0.50      0      1.00   1.17 0.051
## 2 0.44      1      0.50   0.77 0.061
## 3 0.01      2      0.06   0.11 0.032
```

As evident from complexity parameter table function, only the variables 
Petal.Length and Petal.Width are used by the final tree. The cross validation 
error (column xerror) is continously decreasing over the depth of the tree which
starts at level 1 (first row) and ends at level 3 (third row). This can also
be seen in the respective plot.

```r
plotcp(pred)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

A cross validation error of 0.09 is something we can live with for the moment.
So let's have a look at the tree.

```r
plot(pred, uniform = TRUE, main = "Classification Tree for IRIS data set")
text(pred, use.n = TRUE, all = TRUE, cex = 0.75)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

As evident from the petal plot above, species S can be clearly distinguished
by a petal length threshold of 2.45. For species C and G the seperation is not
optimal and C will also be predicted for 5 of the G species (node 4) while
G will be falsly predicted for 1 C species (node 5).

In terms of the petal width vs. length plot, this decission tree can be 
visualized using one pair of horizontal and vertical line which represents
the threshold values of the nodes.

```r
xyplot(Petal.Length ~ Petal.Width, data = iris, groups = iris$Label, pch = 15, 
    main = "Iris species as function of petal width and length", panel = function(x, 
        y, ...) {
        panel.xyplot(x, y, ...)
        panel.abline(h = 2.45)
        panel.abline(v = 1.75)
        ltext(x = x, y = y, labels = iris$Label, pos = 1, offset = 1, cex = 1)
    })
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


To get a very detailed view of the decision structure one can also have a look
at the summary of the decission tree object. It shows the decision at each node
but of course, the graphical representation above is much nicer vor human
beings.

```r
summary(pred)
```

```
## Call:
## rpart(formula = Label ~ Sepal.Length + Sepal.Width + Petal.Length + 
##     Petal.Width, data = iris, method = "class")
##   n= 150 
## 
##     CP nsplit rel error xerror    xstd
## 1 0.50      0      1.00   1.17 0.05073
## 2 0.44      1      0.50   0.77 0.06122
## 3 0.01      2      0.06   0.11 0.03193
## 
## Variable importance
##  Petal.Width Petal.Length Sepal.Length  Sepal.Width 
##           34           31           21           14 
## 
## Node number 1: 150 observations,    complexity param=0.5
##   predicted class=C  expected loss=0.6667  P(node) =1
##     class counts:    50    50    50
##    probabilities: 0.333 0.333 0.333 
##   left son=2 (100 obs) right son=3 (50 obs)
##   Primary splits:
##       Petal.Length < 2.45 to the right, improve=50.00, (0 missing)
##       Petal.Width  < 0.8  to the right, improve=50.00, (0 missing)
##       Sepal.Length < 5.45 to the right, improve=34.16, (0 missing)
##       Sepal.Width  < 3.35 to the left,  improve=19.04, (0 missing)
##   Surrogate splits:
##       Petal.Width  < 0.8  to the right, agree=1.000, adj=1.00, (0 split)
##       Sepal.Length < 5.45 to the right, agree=0.920, adj=0.76, (0 split)
##       Sepal.Width  < 3.35 to the left,  agree=0.833, adj=0.50, (0 split)
## 
## Node number 2: 100 observations,    complexity param=0.44
##   predicted class=C  expected loss=0.5  P(node) =0.6667
##     class counts:    50    50     0
##    probabilities: 0.500 0.500 0.000 
##   left son=4 (54 obs) right son=5 (46 obs)
##   Primary splits:
##       Petal.Width  < 1.75 to the left,  improve=38.970, (0 missing)
##       Petal.Length < 4.75 to the left,  improve=37.350, (0 missing)
##       Sepal.Length < 6.15 to the left,  improve=10.690, (0 missing)
##       Sepal.Width  < 2.45 to the left,  improve= 3.556, (0 missing)
##   Surrogate splits:
##       Petal.Length < 4.75 to the left,  agree=0.91, adj=0.804, (0 split)
##       Sepal.Length < 6.15 to the left,  agree=0.73, adj=0.413, (0 split)
##       Sepal.Width  < 2.95 to the left,  agree=0.67, adj=0.283, (0 split)
## 
## Node number 3: 50 observations
##   predicted class=S  expected loss=0  P(node) =0.3333
##     class counts:     0     0    50
##    probabilities: 0.000 0.000 1.000 
## 
## Node number 4: 54 observations
##   predicted class=C  expected loss=0.09259  P(node) =0.36
##     class counts:    49     5     0
##    probabilities: 0.907 0.093 0.000 
## 
## Node number 5: 46 observations
##   predicted class=G  expected loss=0.02174  P(node) =0.3067
##     class counts:     1    45     0
##    probabilities: 0.022 0.978 0.000
```

Here one can see that also the sepal parameters have been tested but since they
never add an improve over the petal parameters, they are not used in the final
tree.

