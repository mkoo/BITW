#############################################
###VertNet Biodiversity Informatics Training Workshop
###June 2013
# Using {dismo} for Species Distribution Modeling
# Batrachoseps example
#
###updated MSK, 20130615
###for R.3.0.1
#
##^ Package		Versions
##^ dismo 	 	0.8-11
##^ maptools	0.8-23
##^ rgdal 	 	0.8-8
##^ sp 			1.0-9
#################################################
#set up your working directory

install.packages(c('dismo','maptools', 'rgdal', 'sp')); #if you have not already installed these packages

library(dismo)
library(rgdal)
library(maptools)
gpclibPermit()
library(sp)


robustus = gbif('Batrachoseps' ,'robustus')
luciae = gbif('Batrachoseps' ,'luciae');
minor = gbif('Batrachoseps' ,'minor');
gregarius = gbif('Batrachoseps' ,'gregarius')

#take a peek at what you just downloaded (try these different ways)
robustus[1,]
robustus[1:5, 6:9]
colnames(minor);
head(luciae);
tail(gregarius)  #Why would you want to see the last 6 rows of data?

#only the essentials
rob = robustus[, c('species', 'lon', 'lat')];
luc = luciae[, c('species', 'lon', 'lat')];
greg = gregarius[, c('species', 'lon', 'lat')];
mino = minor[, c('species', 'lon', 'lat')];
#more info on these fascinating slender salamanders and niche evolution, see: http://ib.berkeley.edu/labs/wake/2002_BiolJLinSoc_batrachoseps.pdf

d = data.frame(rbind(rob, luc, greg, mino))
d$col = 'blue'   # Adding a new column for color assignments
d$col[d$species=='Batrachoseps luciae'] <- 'grey'
d$col[d$species=='Batrachoseps gregarius'] <- 'yellow'
d$col[d$species=='Batrachoseps minor'] <- 'red'

#Hijmans as kindly posted relevant data online that can be accessed with a single function
usa1 <- getData('GADM', country='USA', level=1)
ca <- usa1[usa1$NAME_1=='California', ]
plot(ca)

points(d$lon, d$lat, col=d$col);


# Get environmental data for both species.
# Do they occur in different environments?
# There are numerous ways to look at that
# The below is rough first exploration of environmental space

# compare two species:
dd = d[d$species=='Batrachoseps luciae' | d$species=='Batrachoseps robustus', ];

#climate data--
wc <- getData('worldclim', var='bio', res=10);
#extract values--
xy <- cbind(dd$lon,dd$lat)
v <- extract(wc, xy); 

#principal components analysis
pca = princomp(v);
loadings(pca)
plot(pca$scores[,1], pca$scores[,2], col=dd$col, xlab='Principal component 1', ylab='Principal component 2', main='PCA')
scinames = c(expression(italic('B. luciae')), expression(italic('B. robustus')))
legend('topleft', scinames, pch=1 , col=c('blue', 'grey'))

i <- dd$species=='Batrachoseps luciae'
hluc = convHull(pca$scores[i,1:2] )
hrob = convHull(pca$scores[!i,1:2] )

plot(hluc@polygons, add=T, border='dark grey')
plot(hrob@polygons, add=T, border='light blue')

# Now can you also compare the other species?

##############
#make a fast Bioclim model:
#prep steps:
#RE-use the Worldclim data from above or get another
# Download worldclim raster stack (at a 5 minute resolution)
wc5 <- getData('worldclim', var='bio', res=5)
wc5
# plot the first layer
plot(wc5, 1)
# use zoom (click on the map twice, once to start box, and again to end zoom:
zoom ( raster(wc5, 1) )
# and again:
zoom ( raster(wc5, 1) )

# Let's re-use B. luciae data
luc

  # get the point coordinates
pts <- cbind(luc$lon, luc$lat)

# fit a bioclim model
bc <- bioclim(wc5, pts)
# set the extent for which you want a prediction (not the whole world)
e <- extent(-123, -118, 34, 38)
# predict
pred <- predict(bc, wc5, ext=e)

#graph it
plot(pred)
plot(ca, add=T)
points(pts, col='red')

#########################
## Prepare R for using Maxent.jar
###
#Before we can use Maxent for modeling, there are some setup steps to do
#You should already have downloaded maxent files either from the workshop tutorial or from the Princeton website
#Copy maxent.jar from http://www.cs.princeton.edu/~schapire/maxent/
install.packages("rJava");
require(rJava)
#find out your default directory for java in R
system.file("java", package="dismo")
#will return a path like this:"~/Documents/R/win-library/3.0/dismo/java"
#or for Mac users: "/Library/Frameworks/R.framework/Versions/3.0/Resources/library/dismo/java"
#this is where you will add maxent.jar file

file.copy("Maxent3.3.3k/maxent.jar", "/Library/Frameworks/R.framework/Versions/3.0/Resources/library/dismo/java/maxent.jar")

#refer to ?maxent or ?predict for more information including Mac troubleshooting 

jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

#just in case this crashes, save.image() early and often!
#########################
??maxent

# Here is the general structure of the function: maxent(predictor variable stack, occurence points, args (see Maxent docs), factors = if present, path = to create files, removeDuplicates= TRUE)

#See also Hijmans and Elith's http://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf, p 55

#occurrence data must be 2 columns with longitude (X), then latitude(Y)
p <- cbind(luc$lon,luc$lat)

mx <- maxent(wc5, p, removeDuplicates=TRUE, path="/mxout")

plot(mx)

e <- extent(p)
model <-predict(mx, wc5, ext=e, filename = "/mxout/model.asc", progress='text')

plot(model)
points(p)

response(mx)

#Now we will do this properly with a training and testing set of presence data:
group <- kfold(p, 5)
pres_train <- p[group !=1,]
pres_test <- p[group ==1,]

#Background samples for training and testing; these will be useful in other model types, too.
bg <- randomPoints(wc5, n=1000, ext=e)
colnames(bg) <- c('lon', 'lat')
group <- kfold(bg,5)
bg_train <-bg[group !=1,]
bg_test <- bg[group ==1,]


xm <- maxent(wc5, pres_train, a=bg, removeDuplicates=TRUE, args=c("-J") , path="~/Desktop/BITW-Workshop/mxout")

# You now have the relevant objects to try out other parameters of either Maxent or another model like a GLM. 
#Refer to SDm vignette pdf, ch. 10 and 11 for steps in implementing more models.
#How would you visualize these in QGIS?

