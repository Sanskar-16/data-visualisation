# 1. Loading data sets and libraries ----

# original data - df

# loading the libraries
library(funModeling)
library(dplyr)
library(ggplot2)
library(plotrix)
library(chron)
library(sf)
library(mapview)

# loading the file
df <- read.csv("37-00049_UOF-P_2016_prepped.csv")

# creating a copy of the data to experiment on so that it does not contaminate
datacopy <- data.frame(df)
attach(datacopy)

# preliminary analysis, checking the dimension and the type of the variables
str(datacopy)
# tells us all the variables are of the datatype character, so we assign proper
# data types to the variables.


# 2. Cleaning and re factoring ----

datacopy <- datacopy[-c(1),] # removing the extra definition row at the first index
# assigning all the "" and "NULL" to NA for easier manipulation of the data down the road.
datacopy[datacopy=="NULL"] = NA
datacopy[datacopy==""] = NA

# checking for missing values in the data.
colSums(sapply(datacopy, is.na)) 

# changing the dates to date format
x_dates <- as.Date(INCIDENT_DATE, "%m/%d/%Y")
x_dates <- x_dates[-1]
datacopy$INCIDENT_DATE <- x_dates
rm(x_dates)

x_dates <- as.Date(OFFICER_HIRE_DATE, "%m/%d/%Y")
x_dates <- x_dates[-1]
datacopy$OFFICER_HIRE_DATE <- x_dates
rm(x_dates)

# converting the format from character to POSIXlt
datacopy$INCIDENT_TIME <- strptime(datacopy$INCIDENT_TIME, format = "%H:%M:%S")

# converting desired variables into numeric
# datacopy$UOF_NUMBER <- gsub(",", "", datacopy$UOF_NUMBER) 
# datacopy$UOF_NUMBER <- as.numeric(datacopy$UOF_NUMBER) converting into numeric gives NULL
datacopy$OFFICER_ID <- as.numeric(datacopy$OFFICER_ID)

datacopy$SUBJECT_ID <- as.numeric(datacopy$SUBJECT_ID)

datacopy$OFFICER_YEARS_ON_FORCE <- as.numeric(datacopy$OFFICER_YEARS_ON_FORCE)

datacopy$BEAT <- as.numeric(datacopy$BEAT)

datacopy$SECTOR <- as.numeric(datacopy$SECTOR)

datacopy$REPORTING_AREA <- as.numeric(datacopy$REPORTING_AREA)

datacopy$STREET_NUMBER  <- as.numeric(datacopy$STREET_NUMBER)

datacopy$LOCATION_LATITUDE <- as.numeric(datacopy$LOCATION_LATITUDE)

datacopy$LOCATION_LONGITUDE <- as.numeric(datacopy$LOCATION_LONGITUDE)

# converting desired variables into factors
datacopy$OFFICER_GENDER <- as.factor(datacopy$OFFICER_GENDER)
summary(datacopy$OFFICER_GENDER)

datacopy$OFFICER_RACE <- as.factor(datacopy$OFFICER_RACE)
summary(datacopy$OFFICER_RACE)

datacopy$SUBJECT_RACE <- as.factor(datacopy$SUBJECT_RACE)
summary(datacopy$SUBJECT_RACE)

datacopy$SUBJECT_GENDER <- as.factor(datacopy$SUBJECT_GENDER)
summary(datacopy$SUBJECT_GENDER)

datacopy$OFFICER_INJURY <- as.factor(datacopy$OFFICER_INJURY)
summary(datacopy$OFFICER_INJURY)

datacopy$SUBJECT_INJURY <- as.factor(datacopy$SUBJECT_INJURY)
summary(datacopy$SUBJECT_INJURY)

datacopy$OFFICER_HOSPITALIZATION <- as.factor(datacopy$OFFICER_HOSPITALIZATION)
summary(datacopy$OFFICER_HOSPITALIZATION)

datacopy$SUBJECT_WAS_ARRESTED <- as.factor(datacopy$SUBJECT_WAS_ARRESTED)
summary(datacopy$SUBJECT_WAS_ARRESTED)

datacopy$SUBJECT_DESCRIPTION <- as.factor(datacopy$SUBJECT_DESCRIPTION)
summary(datacopy$SUBJECT_DESCRIPTION)

datacopy$DIVISION <- as.factor(datacopy$DIVISION)
summary(datacopy$DIVISION)

datacopy$LOCATION_DISTRICT <- as.factor(datacopy$LOCATION_DISTRICT)
summary(datacopy$LOCATION_DISTRICT)

datacopy$STREET_DIRECTION <- as.factor(datacopy$STREET_DIRECTION)
summary(datacopy$STREET_DIRECTION)

datacopy$STREET_TYPE <- as.factor(datacopy$STREET_TYPE)
summary(datacopy$STREET_TYPE)

datacopy$INCIDENT_REASON <- as.factor(datacopy$INCIDENT_REASON)
summary(datacopy$INCIDENT_REASON)

datacopy$REASON_FOR_FORCE <- as.factor(datacopy$REASON_FOR_FORCE)
summary(datacopy$REASON_FOR_FORCE)

# removing useless columns 
datacopy <- select(datacopy, -c(LOCATION_STATE, LOCATION_CITY, STREET_DIRECTION))

datacopy <- rename(datacopy, LON = LOCATION_LONGITUDE,
                   LAT = LOCATION_LATITUDE)

# handling missing values
datacopy <- datacopy[-c(4:5, 10:18, 20:35, 37:64),]

# viewing the difference betweeen the old data set and the new data set 
str(datacopy)
str(data)

# finding what index holds value for this variable 'Type_of_force_used10'
# which(datacopy$TYPE_OF_FORCE_USED10 == unique(datacopy$TYPE_OF_FORCE_USED10)[2])

# 3. exploring the data set after cleaning ----

# setting numerical variables and categorical variables 
numericVars <- which(sapply(datacopy, is.numeric))
numericVarNames <- names(numericVars) 
cat('There are', length(numericVars), 'numeric variables')

catVars <- which(sapply(datacopy, is.factor))
catVarNames <- names(catVars)
cat('The are', length(catVars), 'categorical variables')

catVars
numericVars

cat_df <- subset(datacopy, select=c(5,6,9,11,13,14,15,17,18,23,24,27,32,33))
num_df <- subset(datacopy, select=c(4,8,12,20,21,22,25,30,31))

plot_num(num_df)
freq(cat_df)

# 4. EDA ----

# plotting the variables as pie charts to visualize the assorted variables
ggplot(datacopy, aes(x = factor(1), fill = OFFICER_RACE)) +
  geom_bar(width = 1) +
  coord_polar("y")

ggplot(datacopy, aes(x = factor(1), fill = OFFICER_GENDER)) +
  geom_bar(width = 1) +
  coord_polar("y")

ggplot(datacopy, aes(x = factor(1), fill = SUBJECT_RACE)) +
  geom_bar(width = 1) +
  coord_polar("y")

ggplot(datacopy, aes(x = factor(1), fill = SUBJECT_GENDER)) +
  geom_bar(width = 1) +
  coord_polar("y")

ggplot(datacopy) +
  geom_bar(aes(x = OFFICER_YEARS_ON_FORCE)) +
  scale_fill_viridis_d()

ggplot(datacopy) +
  geom_bar(aes(x = SUBJECT_WAS_ARRESTED, fill = SUBJECT_RACE)) +
  scale_fill_viridis_d()

ggplot(datacopy) +
  geom_bar(aes(x = SUBJECT_WAS_ARRESTED, fill = OFFICER_RACE)) +
  scale_fill_viridis_d()

ggplot(datacopy) +
  geom_bar(aes(x = SUBJECT_RACE, fill = SUBJECT_WAS_ARRESTED)) +
  scale_fill_viridis_d()

ggplot(datacopy) +
  geom_bar(aes(x = SUBJECT_WAS_ARRESTED, fill = OFFICER_INJURY)) +
  scale_fill_viridis_d()

# 5. Geo spatial mapping ----

# plotting the data using map view library
mapview(datacopy, 
        xcol = "LON", 
        ycol = "LAT",
        zcol = "DIVISION", 
        crs = 4326, 
        grid = FALSE, 
        map.types = "Stamen.Toner")

mapview(datacopy, 
        xcol = "LON", 
        ycol = "LAT", 
        zcol = "LOCATION_DISTRICT", 
        crs = 4326, 
        grid = FALSE, 
        map.types = "Stamen.Toner")

mapview(datacopy, 
        xcol = "LON", 
        ycol = "LAT", 
        zcol = "OFFICER_RACE", 
        crs = 4326, 
        grid = FALSE, 
        map.types = "Stamen.Toner")

mapview(datacopy,
        xcol = "LON",
        ycol = "LAT",
        zcol = "STREET_TYPE",
        crs = 4326,
        grid = FALSE,
        map.types = "Stamen.Toner")
