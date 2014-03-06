#############################################
### VertNet Biodiversity Informatics Training Workshop
### June 2013
#   Introduction to R-- data frames, vectors, and basic functions
#
### MSK, 20130615
### for R.3.0.1
#
# Packages:
# {base}, {gdata}
#
#################################################

##R is a calculator (try your own):
555/66
2+3
#Respects laws of precedence
2+3*4
(2+3)*4
###
#Assigning variables: operator is "<-" OR "="
#R is case sensitive so q and  Q are different variables.
x <- 2

y = 3

z <- 5

x+y*z

result <- x+y*z

result

#Objects are stored in the workspace.
ls()         #see them all
rm(x)      #remove 'x'
ls()
#also this
objects()
#Add back 'x', RStudio makes that easy to do without typing the whole line again!

#############
###       Vectors: creating vectors,  the functions c(), seq(), subsetting
xvec <- 1:5
xvec

yvec <-10:15
#see yvec

zvec <- c(10,17,12,8,5,1)
zvec

#    [] square brackets allows returning values by indexing
zvec[3]
yvec[2:4]
#     What happens when the index is out of range?
xvec[8]

#Subsetting vectors
zvec[c(5,1,2)]

zvec[(z==TRUE)]    #Use logical

# seq() function--generate sequential values
help(seq)
a <- seq(10,20)
a

a <- seq(20,10)
a

b <- seq(from=17,to=30,by=2)
b

b <- seq(17,30,2)
b

# Functions that work on vectors
sum(xvec)

mean(zvec)

yvec^2

yvec*10

length(xvec)

sum(xvec)/length(xvec)

#see how vectors can interact
table(xvec)
vec2 <- c(xvec,10,10,4)
vec2

table(vec2)

# Finding things in vectors: use comparison operators (==, <, >, <=, >=, !=), boolean operators ( |, & )and sum() function
vec2 == 1

sum(vec2 == 1)

vec2 == 10

sum(vec2 == 10)

vec2 > 1 & vec2 < 12 #Use & Boolean operator

length(vec2) #Very useful function for writing loops

#Check to see whether an object is a vector
is.vector(vec2)
#Convert an object to a vector
vec3 <- as.vector(vec2)

#Vectors can also be characters
n <- c("Turdus", "migratorius")

names(n) = c("Genus", "species")
n
#or reverse order
n[c("species", "Genus")]


##########################
###     Data Frames
#Data frames are for storing data tables, think of it as a list of vectors of equal length.

###     Matrices
#Matrices are arrays of 2 dimension arranged in columns and rows
#Creating with matrix() function
?matrix #see help for set of arguments
x <- matrix(1:12, nrow=3)
x
#how many rows and columns
dim(x)

#Converting Vector to Matrix
temp1 <- c(5,10,12,24,42,60,63,72)
temp2 <- c(8,1, 3, 88, 33, 0, 77, 42)

#cbind() -- Column Bind. Good for matrices, works with data frames but can cause problems
#each vector must be of same length.
mat <- cbind(temp1, temp2)
mat

is.matrix(mat)

#rbind() -- Row Bind, same logic as cbind but with rows. Each vector must be of same length.

mat2 <- rbind(temp1,temp2)
dim(mat2)

#Using indexes how would you get a specific value from a matrix?
mat2[2,4]  #2nd row, 4th element

#How would you find the first three values of the first row?
#your answer here:


mat2[,1:4] #All rows, but just columns 1 through 4 --very handy

#How would you add a new row of data?

#######
###    Lists   - useful data type that can be anything like numbers, character strings, matrices, other lists
#Lists may be a mix of data types
list = list(species="amphibians", country="US", count="297")
list[1:3]

#How would you call up individual elements? Say, only 'count'?

#Create another list
list2 = list(species="amphibians", country="MX", count="368")

#How would you append these lists together?
# Careful about trying to use as a vector as R will try to coerce to the same data type
#(either all numbers or all characters)

#Note in the Workspace what the data types are for the objects you created

###
#  cat() is really useful as it will write just about anything anywhere.
cat("There are", list2[[3]], list2[[1]], "in", list2[["country"]], ".")

#add an output file argument
cat("There are", list2[[3]], list2[[1]], "in",list2[["country"]],".", file = "reply.txt")


#Depending on what you called your appended lists, you should've gotten a 2x3 list matrix
#I called mine ct and created it with ct = rbind(list, list2)
#It has column headers
colnames(ct)

#And now I want to sum up the tallies of species in both countries
sum(ct$count)

# Did that work? Why or why not?
#OK, hint: it didn't return meaningful results, because our list matrix is data type of character strings; no need to panic!
#First we need to convert to a data frame
#We will use do.call(), which is handy to put together a function and list of arguments
#Our objects are already in a list so we need to only call that set of lists (ct) and we set it up
#with as.data.frame. Map() is a wrapper for a suite of 'apply' functions that we will look at next.
ct.df = do.call(rbind, Map(as.data.frame,list(ct)))

sum(ct.df$count)  #  What happened?

#To convert one atomic value to another these are handy:
#as.character(x)
#as.integer(x)
#as.logical(x)
#as.numeric(x)
#
#So we can do this anytime we want!
sum(as.numeric(ct.df$count))


#########################
###      Working directories and Datasets - Import/Export
###      Installing packages, getting down to business!
#
#setwd(), dir(), getwd(), list.files(), install.packages()
#Majority of the time you will likely be importing data already in a table
# In Windows, directory paths / need to be escaped with double//. Can copy/paste from
# Explorer address bar, then switch the slashes

getwd() # Where is the working directory?
setwd("~/Desktop/BITW-Workshop")  #set for a Mao OS

dir() #See what is in directory
list.files() #does same as above but may take longer

#Write out the matrix data
write.csv(mat2, "temp_mat.csv", row.names=FALSE)
dir()

#Import a CSV file
data = read.csv(file.choose())
#See a sample of the data you just read into the working space:
head(data)
colnames(data)

#Import a Excel File with {gdata}
#R's power comes from packages that extend R's functionality with libraries of functions submitted by users around the world.
#So far, we've been using the base functions that come pre-installed with R.
#Now we will install a package to read XLS files.
install.packages("gdata")
library(gdata)
??gdata
help(read.xls)
#try reading an xls file of specimen records
#For the tutorial, let's use the data acquired yesterday from online portals, named "SearchResults--HN2_Agama_atra.csv" or the xls if you have cleaned up data in this file etc

data = read.csv("SearchResults--HN2_Agama_atra.csv", header=T)
colnames(data)  #see column names in the header row
unique(data$ScientificName)   #what is says: unique values in the ScientificName column of 'data'

# Other handy ways to quickly peek at your data:
head(data)   # First 6 rows of data frame
tail(data)      # Last 6 rows-- I like to check that no errant comma(s) or tab(s) has messed up the fields
# To check the structure of your data, use str()
str(data)

#############################
###    Loops
#Another critical skill to have in R, the ability to iterate through a set of groups, lists, objects
#One way to do this is with For Loops, which has it's pros and cons.
#Please see ForLoops_Rabosky.pdf for examples and exercises.
#
#Another way is with the suite of apply functions
??apply # To see list of functions

#Simply example with an Apply Function

mat # remember our matrix?
d <- data.frame(mat) 
d

sapply(d, mean)  #also can use lapply(); the tutorials will use Apply in various places



#########################
#R is a great graphing environment!
#Let's play with graphs

pie(rep(1,16),col=rainbow(16)) #rep() is a generic function that replicates value x, n times: rep(x,n)
#try changing numbers above to see what I mean
###
s <- seq(-3,3,.1) #creating sequential values between -3 and 3 at 0.1 intervals
 d <- dnorm(s) #calculate standard normal density
plot(s,d,type="l")
 title("Standard Normal Density",col.main="darkblue")

 #rnorm() calculates random deviations; try plotting the deviates!
###

#Lots of great examples out there and type: help(par) for further arguments to refine your graph
# We spend more time with mapping than graphing data however...

Add more examples....


