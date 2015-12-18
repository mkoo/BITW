#############################################
###VertNet Biodiversity Informatics Training Workshop
###June 2013
#Acquiring and Visualizing occurrence records using 3 different packages
#in geographic and environmental space
#using dismo, rgdal, maptools packages
#
###updated MSK, MH 20130524
###for R.3.0.1
#
##^ Package		Versions
##^ dismo 	 	0.8-11
##^ maptools	0.8-23
##^ rgdal 	 	0.8-8
##^ sp 			1.0-9
##^ scales 		0.2.3
##^ rvertnet	0.0-5
##^ rgbif	 	0.2.0
##^ ggplot2		0.9.3.1
#################################################

getwd()
setwd("~/Desktop/BITW-Workshop")

install.packages(c('dismo','maptools', 'rgdal', 'sp')); #if you have not already installed these packages

library(dismo)
library(rgdal)
library(maptools)
gpclibPermit()
library(sp)

#Acquire species occurrence records (here we're using GBIF with {dismo}, yesterday you used the website):
species1<-gbif("Chiromantis", "xerampelina", geo=T);
species2<-gbif("Agama", "atra", geo=T);

#check out values
colnames(species1);
head(species1);
head(species2);

#Make some Tallies
table(species1$country);


#Subsetting on coordinates
#with geo=T, only saved georeferenced records, but just in case...
species1_xy<-subset(species1,lat !=0 & lon !=0);
dim(species1_xy)
species2_xy<-subset(species2, lat !=0 & lon !=0);
dim(species2_xy)

#Set coordinates to a Spatial data frame
coordinates(species1_xy) = c("lon", "lat")
coordinates(species2_xy) = c("lon", "lat")

#Map out to check for spatial integrity, or at least make sure records are on the right continent!
#for quick viewing we'll use the wrld_simpl dataset (from maptools package).
data(wrld_simpl);
plot(wrld_simpl, axes=TRUE, col='light green')
points(species1_xy, col='orange', pch=20, cex=0.75); #outliers in other continents? #why are they occurring there?

#Using zoom() to get a closer look-- click once on map to start bounding box, and second click to close
zoom(wrld_simpl)
points(species1_xy, col='orange', pch=20, cex=0.75)

#######
#Subsetting by country
#First tally records by country
table(species1_xy@data$country)  #Note that for spatial data frames we have to specify the 'slot' of the object
#let's pick those in South Africa for now
sp1_SA = subset(species1_xy, country=="South Africa")
sp2_SA = subset(species2_xy, country=="South Africa")


# {dismo} has a few functions to display Google Maps:
points.SA <- as.data.frame(sp1_SA)
points.SA$x <- sp1_SA@coords[,"lon"]
points.SA$y <- sp1_SA@coords[,"lat"]
sp.map <- gmap(points.SA, type="terrain") #
# Google Maps are in Web Mercator projection, so
# project the points to that using Mercator()
# this will allow proper mapping of points on basemaps
sp1_SA.merc <- Mercator(points.SA[,c("x","y")])
plot(sp.map)
points(sp1_SA.merc, pch=20, col="red")

###
#Export to CSV for snapshot of data (which one to export? why?):
species1_gbifresults = write.csv(species1_xy, "gbifresults_xy.csv", sep=",")


#####################
# Using {rVertnet} and {rgbif}
#Check out DarwinCore field and datatypes before exploring data on VertNet portal
#first of all, what formats are the data in?

install.packages(c('rvertnet','rgbif')); #if you have not already installed these packages
library("rvertnet")
library("rgbif")

#{rvertnet}
data(dcnames)
head(dcnames) #sneak peek
dcnames #see them all

data(dc) #shows data type
head(dc)
#
#run taxon queries one by one
vertoccurrencecount(t="Megaceryle maxima", grp="bird") #how many?
vertproviders(t="Megaceryle maxima", grp="bird") # which collections have the most?
vertoccurrencecount(t="Alcedo semitorquata", grp="bird")
vertproviders(t="Alcedo semitorquata", grp="bird")
vertoccurrencecount(t="Halcyon albiventris", grp="bird")
vertproviders(t="Halcyon albiventris", grp="bird")

vertlocations(l="Africa", t="Alcedo semitorquata", grp="bird", d="1900-1950") # locations and number of historic records (d=YYYY or range)
vertlocations(l="Africa", t="Alcedo semitorquata", grp="bird", d="1950-2012") # locations and number of more recent records

#map of all three bird specimens:
splist <- splist <- c("Alcedo semitorquata", "Halcyon albiventris", "Megaceryle maxima")
out <- lapply(splist, function(x) vertoccurrence(t=x, grp="bird", num=500))
out <- vertoccurrence(t="Alcedo semitorquata", grp="bird")
vertmap(out) #this rVertnet command takes care of a lot of things behind the curtain, including basemaps, NA values, data.frame

#now using {rgbif}
library(rgbif)
#under the hood taxon concepts being used in GBIF
taxoncount(scientificname="Halcyon albiventris")
hkey=taxonsearch(scientificname="Halcyon albiventris")
taxonget(hkey)

#density check: this is a powerful way to see the spatial make-up of records without mapping them. This retrieves a data.frame of total occurrence counts by one-degree cell for a taxon, country, dataset, provider, etc
head(densitylist(taxonconceptKey = 5228322)) #for Halcyon albiventris rank of species
#and here are the top counts by species list
density_spplist(taxonconceptKey = 5228322, spplist = "great", listcount = "counts")[1:10,   ]
d_out=densitylist(taxonconceptKey = 5228322)
gbifmap(d_out)

#grab and view data
Hal_alb=occurrencelist(scientificname='Halcyon albiventris', coordinatestatus=TRUE, latlongdf=TRUE, removeZeros=TRUE)
dim(Hal_alb)
Hal_alb
gbifmap(Hal_alb)

#We may want to check all records by country
density_spplist(originisocountrycode = "ZA", spplist = "great")[1:10] # Top ten list of species data for South Africa
d_ZA <- densitylist(originisocountrycode = "ZA")
gbifmap(d_ZA)  # on world map
gbifmap(d_ZA, region="South Africa") # country only-- review the top ten list or the entire list of species for South Africa and see if this density map makes sense? What other issues are there?

#How does this compare with the point data?





#####################
#If you are interested in other ways to create maps in R, there are lots of great tools out there for the job,
#some a bit involved, but worth the effort.
#Check out ggplot, or use CartoDB's R package (https://github.com/Vizzuality/cartodb-r) with your own data uploaded on
#Cartodb, cloud-based postgres/postgis
#
#More on mapping species richness later...


#############################
#Mapping in environmental space using Worldclim.org climate models
#We will use worldclim bioclimatic variables to explore the species' environmental niche
bc5 <- getData('worldclim', var='bio', res=5)  # If slow or server not responding we have a local version for copying
plot(bc5,1)
#interactive zooming... double click
zoom(raster(bc5,1))
#do this again if you like to see climate model-- at coarse resolution

#or use your own datasets:
#...
#create a raster stack ()
#to make a stack of the current climate data-- it's the same as the above fetch routine
files = list.files(path="wc5", pattern='*.bil', full.names=TRUE)
library(raster)
s = stack(files)
plot(s)
plot(s, 12)


#extract raster stack values to sample sites
sp1_swd<-extract(bc5, sp1uniques[, c('lon','lat')])
sp2_swd<-extract(bc5, sp2uniques[, c('lon','lat')])

#examine values

#graph values
sp1_data<-data.frame(sp1_swd)
sp2_data<-data.frame(sp2_swd)
pairs(sp1_data[,1:11], cex=0.1, fig=TRUE) #how are these bioclim variables related?
pairs(sp1_data[,12:19], cex=0.1, fig=TRUE)

#fast plot in environmental space
plot(sp1_data$bio1, sp1_data$bio12, axes=TRUE)

#write other ways to view the values
plot(...)

# kmeans clustering-to partition n observations into k clusters in which each observation belongs to the cluster with the nearest mean.
sp2_dataclean <- na.omit(sp2_data) # First remove the NAs from the data frame
k <- kmeans(sp2_dataclean, 2)
plot(africa_shp, axes=TRUE)
points(species2$lon, species2$lat, col=k$cluster)


#reduce variability via PCA
sp1_pca<-prcomp(na.omit(sp1_swd))
plot(sp1_pca)
sp1_pca
sp1_pca$rotation   # loadings
sp1_pca$x          # PCs (aka scores)
screeplot(sp1_pca, npcs = 5, type = "lines");

#play with the plotting...
plot(sp1_pca$x[,1], sp1_pca$x[,2], xlab='Principal component 1', ylab='Principal component 2', main='PCA of Species 1')
plot(sp2_pca$x[,1], sp2_pca$x[,2], xlab='Principal component 1', ylab='Principal component 2', main='PCA of Species 2')

#But prcomp() is pretty barebones so try some better packages.
#to inspect the variables a little more closely we can delve a little into some other packages that specializes in multivariate stats (e.g.factomineR):
#FactoMineR (http://factominer.free.fr/)
install.packages("FactoMineR")
library(FactoMineR)

sp1_pca<-PCA(na.omit(sp1_swd))
sp1_pca #Results of PCA and more-- explore them.
#A few I use often
sp1_pca$ind$contrib
sp1_pca$var$contrib
plot.PCA(sp1_pca)
barplot(sp1_pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(sp1_pca$eig))
dimdesc(sp1_pca)  #describe the dimensions

#Online Tutorial (http://factominer.free.fr/docs/code_tutorial.r)
dev.off()
sp1.pca =PCA(sp1_swd[,1:19], scale.unit=TRUE, ncp=2, quanti.sup=c(11: 12), graph=T, axes = c(1,2))
sp2.pca =PCA(sp2_swd[,1:19], scale.unit=TRUE, ncp=5, quanti.sup=c(11: 12), graph=T)

#Correlation of variables is important consideration in modeling
#How would you use this for variable selection?
cor(sp1_data, use = "na.or.complete", method = "pearson")

######################
#{Dismo} is ultimately about conducting species distribution modeling
#There are several {dismo} tools for prepping data, data cleaning, etc
#See Hijmans and Elith, "Species Distribution Modeling"
#http://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf or available from us
#We have been doing a simplified version
#Start with 2.2 Data Cleaning Chapter, p 8
#here we will do a simple find and remove duplicate records

dups1<-duplicated(species1[, c('species', 'lon','lat')])
sum(dups1)
sp1uniques<-species1[!dups1,];

dups2<-duplicated(species2[, c('species', 'lon','lat')])
sum(dups2)
sp2uniques<-species2[!dups2,];

#Make use of properly georeferenced localities by filtering on "coordinateuncertaintyinmeters" ; we are going to skip georeferencing in this tutorial as it the pdf is out of data, but here's one suggestion to filter on coordinate uncertainties:
sp2clean=subset(sp2uniques, !is.na(coordUncertaintyM) & coordUncertaintyM<=5000, select=c(species,lon,lat))



#
#For setting up your training and testing samples, consider:
#Brenning, A. 2012. sperrorest: Spatial Error Estimation and Variable Importance. R package, available online from CRAN.
#http://cran.r-project.org/web/packages/sperrorest/index.html
#http://cran.r-project.org/web/packages/sperrorest/sperrorest.pdf
#
#Other ways to contrast/compare niches-- looking at niche similarity with phyloclim package
#http://cran.r-project.org/web/packages/phyloclim/phyloclim.pdf

