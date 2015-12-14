#################################################
###  Introduction to Geospatial Analysis in R (GIF)
###  November 20, 2015
#
#
### Michelle S. Koo, mkoo@berkeley.edu
### Nancy Thomas, nethomas@berkeley.edu
##
##Prepared in R.3.2.0 using
##^ Package		Version
##^ dismo 	 	 1.0-12
##^ ecoengine  1.9.1
##^ maptools	0.8-37
##^ plyr      1.8.3
##^ rgdal 	 	1.1-1
##^ sp 		  	1.2-1
##^ raster    2.4-20
#################################################
#   Table of Contents:
#  1. Intro- setting up your working directory and environment
#  2. Getting data into your R session: importing XLS
#  3. Exploring your data - basics
#  4. Importing shapefiles, polygons
#  5. Acquiring raster data: downloading and preparing climate data {dismo}
#  6. Simple modeling with {dismo}
#  7. Mapping models
#  8. Evaluate and display results!
#   OPTIONAL Features to try
#
#####################
### Goals for this tutorial:
## A. Import species occurrence data as matrices, use as spatial data
## B. Acquire raster data, and clip as needed for modeling
## C. Mapping and visualizing data in R
## D. Create and evaluate models using DISMO package
## 
#################################################
### Part 1. Introduction to R, set up your working directory and environment
##
getwd()
#setwd("\\Workshops\\Workspace") #PC use double backslashes to escape characters
#setwd("/Workshops/Workspace")  # MAC

#clear out globals
rm(list = ls())

install.packages(c('rgdal', 'ecoengine', 'spatial', 'plyr', 'dismo', 'xlsx', 'maptools', 'XML')); #if you have not already installed these packages

#  RStudio TIP: you can see list of installed packages under the Packages tab

library("rgdal")
library("ecoengine")
library("spatial")
library("plyr")
library("dismo")
library("xlsx")
library("maptools")
gpclibPermit()
library("XML")

# What's in my working directory?
dir()

?help

#################################################
### Part 2. Getting data into your R session: importing files
## Downloading data directly from data portals is also possible with packages like
## {ecoengine}, {rgbif}, see the Optional section at end.
#
# Import a excel file-- two general ways, either convert to CSV and import CSV using read.csv()
# OR
# use a package to import XLS directly. We will use XLSX package
# What would be the advantages of importing a single sheet of an XLS file over CSV?
# This package also allows writing to new sheets or appending rows to existing ones in an Excel file so you can keep your data in one place, or as an output product
#Read more at: https://cran.r-project.org/web/packages/xlsx/xlsx.pdf

species1 = read.xlsx("data/Tamias_alpinus-MVZ.xls", 1, sheetName="csv") #first row has header info

species2 = read.xlsx("data/Tamias_speciosus-MVZ.xls", 1, sheetName="csv")

## We just loaded in data for two species of chipmunks: Tamias alpinus, Alpine Chipmunk and Tamias speciosus, Lodgepole Chipmunk. 
## These two species have been subject to many studies of habitat use and range shifts over time throughout the Sierra Nevada, especially in Yosemite National Park, where they occur sympatrically.
## Alpine chipmunks is a high elevation species, which has shown upward range contraction in its lower limits while Lodgepole chipmunks have expanded both its upper and lower elevational ranges, making the two species ideal for asking questions of the impact of climate change.

# See Objects stored in the workspace.

ls()         #see them all

#rm(x)      # Uncomment this line to remove 'x'



#################################################
### Part 3. Exploring your data - basics and graphing
#

head(species1)  #check out the first few records in a dataset

# specific parts of a dataframe can also be called
species1 [1,]  #first row, all cols
species1 [1,1:5]

#What's the difference in these commands?
col(species1)
row(species1)
dim(species1)
names(species1)

summary(species1) #summary stats
str(species1)     #structure of object

# Rstudio TIP: in the Environment tab, you can see the data directly in a table by clicking on the table icon.

# you can see the specific elements of matrices or dataframes (2D)

species1$COLLECTOR

# that was confusing, so maybe you just want to see the unique values
unique(species1$COLLECTORS)

count(species1$COLLECTORS)


# Even better
table(species1$group_era);
table(species1$group_area);  # now compare these same values with Tamias speciosus


# Graphing is fun
# make some XY variables

pts_1 <-cbind(species1$DEC_LONG, species1$DEC_LAT)
pts_2 <-cbind(species2$DEC_LONG, species2$DEC_LAT)

# What is cbind? Use 'help' to find out
?cbind

#You just created spatial arrays

# simple plotting
plot (pts_1, type="p", col="red")
points(pts_2, col="blue")

# How is it different if you reverse order of plotting?

#because we have MAPTOOLS installed... Map out to check for spatial integrity,
#or at least make sure records are on the right continent!

data(wrld_simpl);
plot(wrld_simpl, axes=TRUE, col='light green')
points(pts_1, col='orange', pch=20, cex=0.75);

#Using zoom() to get a closer look-- click once on map to start bounding box, and second click to close
zoom(wrld_simpl)
points(pts_1, col='orange', pch=20, cex=0.75)

# or set an extent
zoom(wrld_simpl, pts_1)

######## Warning
# Zoom() may not work with your version; it doesnt seem to work with the latest version of R (3.2 +)
# So we will use another call to only draw a region of the wrld_simpl

USA <- wrld_simpl[wrld_simpl$NAME %in% c('United States'),]
plot(USA)


##in conjunction with DISMO (warning: last I checked, PC graphics issue with drawing rasters)

g <- gmap (pts_2, exp=1, type='terrain', lonlat=TRUE)
plot(g, interpolate=TRUE)
points(pts_2, col="red", add=TRUE)


#DISMO has posted relevant data online that can be accessed with a single function
usa1 <- getData('GADM', country='USA', level=1) #fetches from server, may take awhile
#or if you have already downloaded the gadm dataset, then go ahead and load the dataset:
load("USA_adm1.RData")
ca <- usa1[usa1$NAME_1 =='California', ]
plot(ca)
points(pts_1);

#################################################
#  4. Importing shapefiles, polygons

##read in shape poly-- this will be your clipping mask
CA_poly<-readOGR(dsn="data", layer="California_poly")  #Note coding in paths relative to your working directory; also no need to put in .shp extension

# How would you look at the structure of this object? What is it?
...

# Note that this is a new type of dataframe, a spatial one, with 'slots', a new level of the dataframe 
slotNames(CA_poly)

proj4string(CA_poly); #What is the current projection?

# How would you map it out?

# what does the proj4string tell us? The projection of the shapefile.
# Proj4 library of projections and crosswalks to the EPSG geodetic parameter datasets
# Both SP and RGDAL access proj4
#https://github.com/OSGeo/proj.4/wiki
# For a searchable database of every projection imaginable see:
# http://spatialreference.org/


#Let's save mapping for later, after we acquire more data and modeling, see next section
#
# Below is OPTIONAL
# To reproject:
# Look up 'California Teale Albers'
ALB <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

CA_polya <- spTransform(CA_poly, CRS(ALB))

# Draw shapefile
plot(CA_polya)

#now add some of our species data
pts2 <- SpatialPoints(pts_2, proj4string=CRS(as.character(ALB)))
pts1 <- SpatialPoints(pts_1, proj4string=CRS(as.character(ALB)))

plot(CA_polya)
points(pts2, col="blue")
points (pts1, type="p", col="red", ADD="TRUE")



#################################################
### Part 5.  Acquiring and Preparing Climate data in raster format
##
#Simple, batch raster exporting approach

#Download Worldclim raster data using {dismo}
wc5 <- getData('worldclim', var = 'bio', res = 5)
#Create a list of .bil files that exist in the working directory
files = list.files(path="wc5", pattern='\\.bil', full.names=TRUE)

# combine all list elements into a stack; this way we can treat all the rasters as a unit in modeling
s <- stack(files)
# check one of them out
plot(s, 1) #this could take awhile to draw all 19 rasters in your stack, so pick one

# crop to your study extent
s.crop <- crop(s, CA_poly)
plot(s.crop[[1]])  # check one
points(pts_2, col="blue")
points(pts_1, add=TRUE);
plot(CA_poly, add=TRUE, border="dark gray");

#For Reference--> NOT NEEDED for this WORKSHOP
#Create exported rasters-->Uncomment the next two lines to create a directory of the cropped rasters

#bioNum<-c(1:19)
#for (i in 1:length(files)) {
  writeRaster(s.crop, filename="wc5\\bio.asc", bylayer=TRUE, overwrite=TRUE)
}

#################################################
### Part 6. Simple modeling with {dismo}
#
# By now, you should have two sets of data for your model:
# 1) species occurrence data
# 2) set of ASCII rasters of worldclim variables
#
# We are using DISMO package to do some simple modeling but there are others....
#
# For several detailed vignettes and examples:  http://cran.r-project.org/web/packages/dismo/


#Subsetting
# Because these are datasets from the Grinnell Resurvey Project we have group_era assignments.
# We may want to create subsets of these data by region or eras
# You have a choice of subsetting by region or era or species
species1_hist<-subset(species1, species1$group_era == "pre-1945"); # any logical statement can be made here.
species2_hist<-subset(species2, species2$group_era == "pre1945");

#Set coordinates to a Spatial data frame
coordinates(species1_hist) = c("DEC_LONG", "DEC_LAT")
coordinates(species2_hist) = c("DEC_LONG", "DEC_LAT")
sp1_hist = species1_hist[11:12]

#extract raster stack values to unique sample sites
sp1_swd <- extract(s, species2_hist)
sp2_swd <- extract(wc5, species2_hist);

sp1_data<-data.frame(sp1_swd)
sp2_data<-data.frame(sp2_swd)

# How would you examine values?

#graph values

pairs(sp2_data[,1:11], cex=0.1, fig=TRUE) #how are these bioclim variables related?
pairs(sp2_data[,12:19], cex=0.1, fig=TRUE)

#fast plot in environmental space
plot(sp2_data$bio2, sp2_data$bio12, axes=TRUE)

#write other ways to view the values
plot(...)

#Now do the same for the other taxa

######################
#{Dismo} is about conducting species distribution modeling
#There are several {dismo} tools for prepping data, data cleaning, etc
#See Hijmans and Elith, "Species Distribution Modeling"
#http://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf
#We have been doing a simplified version

# We are going to skip georeferencing in this tutorial, but here's an example to filter on coordinate uncertainties:
# Make use of properly georeferenced localities by filtering on "coordinateuncertaintyinmeters".
#Uncomment to run:
#sp2clean=subset(sp2uniques, !is.na(coordUncertaintyM) & coordUncertaintyM<=5000, select=c(species,lon,lat))
#
#For setting up your training and testing samples, consider:
#Brenning, A. 2012. sperrorest: Spatial Error Estimation and Variable Importance. R package, available online from CRAN.
#http://cran.r-project.org/web/packages/sperrorest/index.html
#http://cran.r-project.org/web/packages/sperrorest/sperrorest.pdf
#
#Other ways to contrast/compare niches-- looking at niche similarity with phyloclim package
#http://cran.r-project.org/web/packages/phyloclim/phyloclim.pdf

###############
# We will use a simple climate envelope method for now-- Bioclim algorithm

# fit a bioclim model
bc <- bioclim(wc5, pts_2);

# set the extent for which you want a prediction (not the whole world)
e <- extent(CA_poly)

# predict
pred1 <- predict(bc, wc5, ext=e)

#graph it
plot(pred1)
plot(CA_poly, add=T)
points(pts_2, col='red')

plot(bc, a=1, b=12, p=0.90)  # graphed in environmental space

# Bioclim does not require background or absence data but other methods do
# DISMO has some functions to create background datasets

bg <- randomPoints(s, n=500, ext=e) # background data generation
eval <- evaluate(pts_2, bg, bc, wc5)


plot(eval, 'TPR') #True positive rate
boxplot(eval)
density(eval)

#Examine the structure
str(eval)   # Read ModelEvaluation {dismo} for all the classes and abbreviatons

# Now we are going to set up a different model, one that subsets the training and testing data.

sample <- sample(nrow(pts_2), round(0.75 * nrow(pts_1))) #sample 75% of pts_1
train_2 <- pts_2[sample,]
test_2 <- pts_2[-sample,]

bc <- bioclim(wc5, train_2)
eval <- evaluate(train_2, test_2, bc, wc5)

eval

plot(eval, 'ROC')
plot(eval, 'TPR')
boxplot(eval, col=c("blue", "red"))

# A random sampling is fine for a demo, but you will probably want to employ a k-fold partitioning strategy in replicate.

k <- 5
group <- kfold (pts_1, k);
group [1:10]
unique(group)

evals <- list()
for (i in 1:k) {
  train <-pts_1[group != i,]
  test <- pts_1[group == i,]
  bc <- bioclim (wc5, train)
  evals[[i]] <- evaluate(p=test, a=bg, bc, wc5)  # where p=presence, a=absence 
}

# You can take a look at these individually but more common to use in AUC
auc <- sapply (evals, function (x){slot(x,'auc')})
auc
mean(auc)

####################
# Thresholding the model output
#

t <- threshold (eval, 'spec_sens')
# Calls the highest value of the sum of the sensitivity (true positive rate) and specificity (true negative rate)
t

bc_predict <- predict(bc, wc5, ext=e);
bc_t <- bc_predict > t;

# Now to see the models, without and with threshold

plot(bc_predict, main="Bioclim Model")
plot(CA_poly, add=TRUE, border='gray')

plot(bc_t, main="Bioclim Presence/ Absence")
plot(CA_poly, add=TRUE, border='gray')
points(train_2, pch="+")


#########################
# The next sections are for trying out these modeling approaches with the second species (it you have not already run species 2 in parallel)
# With two models, either two taxa or the same taxa but different time periods, you may want
#compare the results. See Compare section below or continue with another model
#
#########################
# Compare a different modeling method: Random Forest,
# an extension of the Classification and Regression Trees or CART
# In R, the package {randomForest} is used in conjunction with {dismo}
#See also Hijmans and Elith's http://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf
#p.59

install.packages('randomForest')
library('randomForest')

# Since this is a regression model, we need to extract environmental values from  presence/absence sites. Data frame manipulation follows.


sp2_swd <- extract(wc5, pts_2, df=TRUE); #last parameter returns a data frame; otherwise convert the result into a data frame with data.frame()
sp2_swd$pa <- 1
head(sp2_swd)

bg_swd <- extract(wc5, bg, na.rm=TRUE, df=TRUE);
bg_swd$pa <- 0
head(bg_swd)

train2 <- rbind(sp2_swd, bg_swd)   #What does rbind do?
train2 <- na.omit(train2)

model <- pa ~ bio1 +bio2 +bio3 +bio4 +bio5 +bio11 +bio12 +bio13 +bio17 +bio18 +bio19

RF <- randomForest(model, data=train2)
RF

# see the tree in the forest!
getTree(RF, 1, labelVar=TRUE)

importance(RF)  # in decreasing mean accuracy, the variables of importance

plot(RF, log="y")

# To plot out a tree- like structure there has to be some functions written first as it is not in the 
# randomForest package SEe:https://cran.r-project.org/web/packages/randomForest/randomForest.pdf

cat(colnames(train2[,2:20]), sep="+") #cheating shortcut...

model <- pa ~ bio1 +bio2 +bio3+bio4+bio5+bio6+bio7+bio8+bio9+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19

RF2 <- randomForest(model, data=train2)
RF2

eRF2 <- evaluate(sp2_swd, bg_swd, RF2)
eRF2

pRF2 <- predict(wc5, RF2, ext=e)
plot(pRF2, main="Random Forest (regression)")
plot(CA_poly, add=TRUE, border="dark gray")

t <- threshold(eRF2, 'spec_sens')
RF2_t = pRF2 > t
plot(RF2_t, main=" Random Forest, presence/absence")


#########################
# Compare a different modeling method: Maxent
##
## Prepare R for using Maxent.jar
###
#Before we can use Maxent for modeling, there are some setup steps to set up R to call the Maxent java executable. 
# SKIP THIS SECTION IF YOU"VE ALREADY INSTALLED maxent.jar
#You should already have downloaded maxent files either from the workshop tutorial or from the Princeton website
#Copy maxent.jar from http://www.cs.princeton.edu/~schapire/maxent/
install.packages("rJava");
require(rJava)
#find out your default directory for java in R
system.file("java", package="dismo")
#will return a path like this:"~/Documents/R/win-library/3.0/dismo/java"
#or for Mac users: "/Library/Frameworks/R.framework/Versions/3.0/Resources/library/dismo/java"
#this is where you will add maxent.jar file

#EG. for Mac users:
file.copy("maxent-software/maxent.jar", "~/Library/R/3.2/library/dismo/java/maxent.jar")

jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

#refer to ?maxent or ?predict for more information including Mac troubleshooting

#just in case this crashes, save.image() early and often!
#########################
??maxent

# Here is the general structure of the function: maxent(predictor variable stack, occurence points, args (see Maxent docs), factors = if present, path = to create files, removeDuplicates= TRUE)

#See also Hijmans and Elith's http://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf
# p 56

#Maxent has different but specific file format requirements
#occurrence data must be at least 2 columns with longitude (X), then latitude(Y)
#luckily we already gave datasets in memory in that format

#set the extent of pseudo-absence and model training
e2 <- extent(pts_2)

mx <- maxent(wc5, pts_2, removeDuplicates=TRUE, args=c("outputdirectory=mxout", "plots=TRUE"))

# Call to args can be found in the help files of Maxent (when you download from http://www.cs.princeton.edu/~schapire/maxent/

model <-predict(mx, wc5, ext=e2, filename = "/mxout/model.asc", progress='text')

plot(model)
points(pts_2)

# For more indepth R scripts and tutorial on Maxent, refer to the tutorial and other papers at
# http://www.cs.princeton.edu/~schapire/maxent/

###############################
#
# Comparing two models can be done simply as raster calculations
# For example:

dbcRF = bc_t - RF2_t

#add some vectors for breakpoints and color

breakpoints <- c(-1, -0.5, 0, 0.5, 1)
breakcolor <- c("blue", "white","white", "red", "red")

plot(dbcRF, breaks=breakpoints, col=breakcolor)
plot(CA_poly, add=TRUE)

# For more plotting fun, check out {ggplot} and {rColorBrewer}



###########     OPTIONAL    #################
#You could also visit the websites for
#the Berkeley Ecoinformatics Engine at http://holos.berkeley.edu and
#Ecoengine (http://ecoengine.berkeley.edu/) to search and visualize data from there...
#But instead, we will do so all from the R console.

#Search parameters for all adhere to DarwinCore metadata standards. A full list is here: http://rs.tdwg.org/dwc/
#
# First, let's check out the Berkeley Ecoengine:
ee_about() #Available endpoints as API under constant development

sources <- ee_sources()
sources # full list of data sources currently supported in Ecoengine


species1 <-ee_observations(scientific_name="Tamias alpinus", georeferenced="TRUE", page_size=200) #pages default to the first 25 but can specify numbers of page or page ="all"

ee_map(species1); #interactive map using leaflet; will launch your default browser

#Export occurrences to CSV:
data <- as.data.frame(species1$data)
data
colnames(data)

data4mx <- data[,c('scientific_name', 'longitude', 'latitude')]
write.csv (data4mx, "data4mx.csv", quote=FALSE, row.names=FALSE)

# Explore other distribution portals on ROPenSci
# https://ropensci.org/packages/


