
# this is the location number as delivered to me.  This is a single location for the data that needs to be joined to the data with all the lab values

#read in the "Lodi_locations_dirty.csv" data
library(dplyr)

#C:\Users\swilso49\Dropbox\Cal Poly classes\SS 413\data\Procsseed

###My local drive
setwd("C:/Users/swilso49/Dropbox/Cal Poly classes/SS 413/data/Procsseed")


# cleaning the location data ----------------------------------------------


locations<-read.csv("Lodi_locations_dirty_rightone.csv")
list.files()
# inspect the data 
names(locations)
locations<-locations[,c("Profile.location.code", "x", "y" )]
locations<-select(locations,Profile.location.code, x, y)# works

# this code snippet appears to be depreciated...
####as.numeric(levels(locations$x))[locations$x]###

# overwrite the x and y location data with the new numeric data you just learned how to create

locations$x<-as.numeric(locations$x)
locations$y<-as.numeric(locations$y)

#now you may notice, that whoever entered in the data into the spread sheet entered the coordiantes as positive for the longitude.    
#This, of course, puts the points in the yellow sea between Korea and China.  please convert the x values to negatives.  Overwrite the initial data

# positive longitude
locations$x<-(-(locations$x))

# much of the data are missing location codes.  Unfortunately, we can't do anything with this data...

#please remove the rows containing NA data 

locations<-na.omit(locations)

sum(is.na(locations))

write.csv(locations, "cleaned Lodi Location data NA removed.csv")




# Cleaning and aggreagting the Lodi 30-100 data ---------------------------


# Read in the attribute data ----------------------------------------------
# Now that we have added the location data and cleaned it up a bit, lets add the soil analysis data

# The file is called "Lodi_new_raw_data_new_names.csv"

lodi_data<-read.csv("Lodi_new_raw_data_new_names.csv")
# take a look at the data
dim(lodi_data)
names(lodi_data)
View(lodi_data)

# How many rows?

# it looks like the pH was not measured.  
#remove the pH_1.1 column (keep the same name, so overwrite lodi_data)
lodi_data<-lodi_data[,!colnames(lodi_data)=="pH_1.1"]

#check that you got rid of the pH_1.1 column
names(lodi_data)


# exploratory data analysis -----------------------------------------------


# Some initial visualizations
# refer back to the previous lab. 
# using ggplot, make a plot of the density of the "k_mg_kg" colum
#
library(ggplot2)

#K

ggplot(lodi_data, aes(x = K_mg_kg)) + geom_density(cex = 2)+xlim(0,700)


# K fixation
ggplot(lodi_data, aes(x = K_fix)) + geom_density(cex = 2)+xlim(0,700)

# Whoa!  Lots of values were removed!

# This is because many negative values were removed.  These were area's that had NO K fixation.    

# notice that the K fixation data looks like there may be two populations of K fixation, a low K fixaion, aroung 100 mg K per kg soil, and a large rK fixation of around 300 mg K per kg soil


# you can visualize the data using a histogram too
hist(lodi_data$K_mg_kg)
hist(lodi_data$K_fix)

# now, with the full profile data, is there any realtionship between K fixation and clay?

# lets create an x~ y plot
names(lodi_data)

plot(lodi_data$clay_pct, data$K_fix)
ggplot(data=lodi_data, aes(x = clay_pct, y = K_fix))+geom_point()
# there doesn't appear to be a very strong correlation between these data points

# So, lets view the realtionship between all of our data points

# we can view the relationship between all the data at once using the pacakage GGally()
# learn more here https://www.r-graph-gallery.com/correlogram.html

install.packages("GGally")

library(GGally)

#now we call ggpairs, but only on a subset of the continous data (numierc data)

names(lodi_data)
# lets look at the correlation with 
paired_correlation<-lodi_data[, c(5:7, 11:14)]# the Potassium columns versus the CEC and texture columns

ggpairs(paired_correlation)

# what has the best correlation with K fixation?
# what has the strongest correlations with K mg/kg 


# now, refer to the last of the R review section of the rspatial.org
#https://www.rspatial.org/intr/13-statmodels.html

# make a lienar model (regression) between K_mg_kg and the best predictor from the correlation matrix

m <- lm(K_fix ~ silt_pct, data=lodi_data)

summary(m)



# Depth weighted averages -------------------------------------------------

#make integers (note hzdepb_r is the bottom depth and hzdept_r is the top depth)

names(lodi_data)[names(lodi_data) == "Profile.location.code"] <- "Soil.ID"

lodi_data$hzdepb_r<-as.integer(lodi_data$hzdepb_r)
lodi_data$hzdept_r<-as.integer(lodi_data$hzdept_r)

class(lodi_data$Lower.Boundary)
a<-lodi_data$hzdept_r<30 & lodi_data$hzdepb_r<100

lodi_data$hzdepb_r-30

# the second condition

#If the top is less than 30, but the bottom is greater than 100, do 30-100, this is for 0-109

b<-lodi_data$hzdept_r<30 & lodi_data$hzdepb_r>100
#if b, than 100-30

# the third condition
#Finally, if top is greater than 30, abd bottom is greater than 100, do 100-top

c<-lodi_data$hzdept_r>30 & lodi_data$hzdepb_r>100

100-horizon_all$hzdepb_r

#nested if else for logical vectors
#Sweet!  This nested if else statement worked awesomely!
Thickness_30_100<-ifelse(a, lodi_data$hzdepb_r-30, 
                         ifelse(b, 100-30, 
                                ifelse(c, 100-lodi_data$hzdept_r, 
                                       (lodi_data$hzdepb_r-(lodi_data$hzdept_r))
                                )
                         ))

#comibne to original data 
horizon_all_30_100<-cbind(lodi_data, Thickness_30_100)

#now we subset so that we get ones where the bottom is greater than 30 (so no 0-25) and the top is less than 100 (that is no 105-150)

HZ_30_100<-horizon_all_30_100[horizon_all_30_100$hzdepb_r>30 & horizon_all_30_100$hzdept_r<100,]


# Making depth weighted averages from the thickness vector ----------------



names(horizon_all_0_30)
library(dplyr)

Lodi_points_30_100<-HZ_30_100 %>%# sends the ssurgo clean data set to group by
  group_by(Soil.ID) %>%# groups surgo_clean by mukey
  dplyr::summarize(#summarizes each input by a weighted average by component percent)
    K_fix = weighted.mean(K_fix, Thickness_30_100),
    K_mg_kg = weighted.mean(K_mg_kg, Thickness_30_100,na.rm=TRUE),
    K_cmol = weighted.mean(K_cmol, Thickness_30_100,na.rm=TRUE),
    Ca_cmol = weighted.mean(Ca_cmol, Thickness_30_100,na.rm=TRUE),
    Mg_cmol = weighted.mean(Mg_cmol, Thickness_30_100,na.rm=TRUE),
    Na_cmol = weighted.mean(Na_cmol, Thickness_30_100,na.rm=TRUE),
    CEC_cmol = weighted.mean(CEC_cmol, Thickness_30_100,na.rm=TRUE),
    clay_pct = weighted.mean(clay_pct, Thickness_30_100,na.rm=TRUE),
    silt_pct = weighted.mean(silt_pct, Thickness_30_100,na.rm=TRUE),
    sand_pct = weighted.mean(sand_pct, Thickness_30_100,na.rm=TRUE))

View(HZ_30_100)

sum(is.na(Lodi_points_30_100))

sum(is.na(locations))

# ok, so now we are going to merge the location data with the soil chemical data
# the merge call is realted to a SQL querey.  We are combining data frames with a one to many realtionship. In this case the soil analysis data has many horizons.  However, each pedon only has one location. So, our goal is to get the location data to fill in for every horizon.  So that the data, basically, has the same corrdiantes replicated for wach horizons in a profile. 

testy<-na.omit(Lodi_points_30_100)
dim(testy)


locations<-read.csv("cleaned Lodi Location data NA removed.csv")
dim(Lodi_points_0_30)
names(locations)[names(locations) == "Profile.location.code"] <- "Soil.ID"

data <- sp::merge(x=Lodi_points_30_100, y=locations, by = "Soil.ID", all.x = TRUE)
data <- merge(x=Lodi_points_30_100, y=locations, by = "Soil.ID", all.x = TRUE)

nrow(data)
# What the "all.x=T" does is make sure that each instance of the analysis data is kept, we are combining the data by the "profile.location.code"  So, for each profile location code, the soil analysis data are kept, and the data from locaiton dat awill be filled in for the data in the soil analysis data


# Ok, so now we have a pretty nice data frame with all the point data, and all the soils analysis done. 

View(data)

head(data)

tail(data)


# more cleaning of data
clean_data<-filter(data, !is.na(x))
dim(clean_data)

nrow(clean_data)

sum(is.na(clean_data$x))

# save clean_data as a .csv and turn in

# to turn in "clean_data.csv"  and the code

write.csv(clean_data, "Soils Analysis Data 30_100 cm (point data).csv")

# Going spatial,  creating a spatial points data frame --------------------

#creating the points, making a spatial points object with the coordiante data
xy <-SpatialPoints(cbind(clean_data$x, clean_data$y))

# assign the CRS
proj4string (xy) <-(CRS("+proj=longlat +datum=WGS84"))

# merge existing data and the lcoation data, make
points_30_100<-SpatialPointsDataFrame(xy, clean_data)

# now write the depth weighted points to a shapefile, and store
shapefile(points_30_100, "lodi_points_30_100_1.shp", overwrite=TRUE)

# reading in the 0-30 data to make a clean spread sheet for making figures

#read in from shapefile
test<-shapefile("lodi_points_0_30.shp")

#make a DF from .shp
testy<-data.frame(test)
# write to file
write.csv(testy, "Soils Analysis Data 0_30 cm (point data).csv")


