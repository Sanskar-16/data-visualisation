# Section 1: Early stages ----

# clean data
# deal with the missing values

# original data - data
# copy of the data = datacopy
# cleaned data = df

# loading the libraries
library(funModeling)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(ggmosaic)
library(plotrix)
library(chron)

# loading the file
data <- read.csv('37-00049_UOF-P_2016_prepped.csv')

# creating a copy of the data to experiment on so that it does not contaminate
datacopy = data.frame(data)
attach(datacopy)

# preliminary analysis, checking the dimension and the type of the variables
str(datacopy)
names(datacopy)[which(sapply(datacopy, is.character))]
# tells us all the variables are categorical.

# Cleaning ----
datacopy <- datacopy[-c(1),] # removing the extra definition row at the first index
# assigning all the "" and "NULL" to NA for easier manipulation of the data down the road.
datacopy[datacopy=="NULL"] = NA
datacopy[datacopy==""] = NA

# checking for missing values in the data.
colSums(sapply(datacopy, is.na)) 

# # converting desired variables into date
# library(lubridate)
# data$INCIDENT_DATE <- mdy(data$INCIDENT_DATE)
# str(data)

# alternate way of doing the above
x.dates <- as.Date(INCIDENT_DATE, "%m/%d/%Y")
x.dates <- x.dates[-1]
datacopy$INCIDENT_DATE <- x.dates
rm(x.dates)

x.dates <- as.Date(OFFICER_HIRE_DATE, "%m/%d/%Y")
x.dates <- x.dates[-1]
datacopy$OFFICER_HIRE_DATE <- x.dates
rm(x.dates)

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

# converting desired variables into factors
datacopy$OFFICER_GENDER <- as.factor(datacopy$OFFICER_GENDER)
freq(datacopy$OFFICER_GENDER)
datacopy$OFFICER_RACE <- as.factor(datacopy$OFFICER_RACE)
freq(datacopy$OFFICER_RACE)
datacopy$SUBJECT_RACE <- as.factor(datacopy$SUBJECT_RACE)
freq(datacopy$SUBJECT_RACE)
datacopy$SUBJECT_GENDER <- as.factor(datacopy$SUBJECT_GENDER)
freq(datacopy$SUBJECT_GENDER)
datacopy$OFFICER_INJURY <- as.factor(datacopy$OFFICER_INJURY)
freq(datacopy$OFFICER_INJURY)
datacopy$OFFICER_INJURY_TYPE <- as.factor(datacopy$OFFICER_INJURY_TYPE)
freq(datacopy$OFFICER_INJURY_TYPE)
datacopy$SUBJECT_INJURY_TYPE <- as.factor(datacopy$SUBJECT_INJURY_TYPE)
freq(datacopy$SUBJECT_INJURY_TYPE)
datacopy$OFFICER_HOSPITALIZATION <- as.factor(datacopy$OFFICER_HOSPITALIZATION)
freq(datacopy$OFFICER_HOSPITALIZATION)
datacopy$SUBJECT_WAS_ARRESTED <- as.factor(datacopy$SUBJECT_WAS_ARRESTED)
freq(datacopy$SUBJECT_WAS_ARRESTED)
datacopy$FORCE_EFFECTIVE <- as.factor(datacopy$FORCE_EFFECTIVE)
freq(datacopy$FORCE_EFFECTIVE)
datacopy$DIVISION <- as.factor(datacopy$DIVISION)
freq(datacopy$DIVISION)

# combining columns 
data$alltypesofforce <- paste(data$TYPE_OF_FORCE_USED1, data$TYPE_OF_FORCE_USED2, data$TYPE_OF_FORCE_USED3, data$TYPE_OF_FORCE_USED4, data$TYPE_OF_FORCE_USED5, data$TYPE_OF_FORCE_USED6, data$TYPE_OF_FORCE_USED7, data$TYPE_OF_FORCE_USED8, data$TYPE_OF_FORCE_USED9, data$TYPE_OF_FORCE_USED10)

# changing lat and long from character to numeric
datacopy$LOCATION_LATITUDE <- as.numeric(datacopy$LOCATION_LATITUDE)
datacopy$LOCATION_LONGITUDE <- as.numeric(datacopy$LOCATION_LONGITUDE)

# removing useless columns 
datacopy <- select(datacopy, -c(LOCATION_STATE, LOCATION_CITY))

# converting the cleaned data frame into a csv file for further analysis 
write.csv(datacopy, "../data-visualisation/cleaned_data.csv", row.names = FALSE)
# as we notice the data is filled with categorical varibles, we pass the varibale
# 
df <- read.csv("cleaned_data.csv", stringsAsFactors = T)
str(df)

# finding what index holds value for this variable 'Type_of_force_used10'
which(df$TYPE_OF_FORCE_USED10 == unique(df$TYPE_OF_FORCE_USED10)[2])

# Section 2: EDA
# maybe correlatino checks
# library(ggcorrplot)
# summary(datacopy)
# corr = cor(datacopy, use = 'complete.obs')
# corr[upper.tri(corr)] = 0
# diag(corr) = 0

# subplots
par(mfrow=c(2,2))
hist(table(df$OFFICER_RACE))
pie(table(df$OFFICER_RACE), radius = 1)
hist(table(df$OFFICER_GENDER))
pie(table(df$OFFICER_GENDER))

barplot(table(df$OFFICER_GENDER))
barplot(table(df$OFFICER_RACE))

ggplot(df) + geom_bar(aes(x = OFFICER_YEARS_ON_FORCE))
ggplot(df) + geom_bar(aes(x = OFFICER_RACE))

# plotting the variable officer_race as pie char to visualize the different races
ggplot(df, aes(x=factor(1), fill=OFFICER_RACE))+
  geom_bar(width = 1)+
  coord_polar("y")

i <- 0
for(i in ncol(df)) {
  ggplot(df) + geom_bar(aes(x = i))
  print('yes')
}

# Section 3: Geospatial mapping ----
# plotting lat vs long
library(sf)
library(ggplot2)

my_df <- datacopy[, c(32:33)]

# copying columns into new data frame

str(my_df)

# changing variable names
names(my_df)[1] <- "LAT"
names(my_df)[2] <- "LON"

# handling missing values
my_df_comp <- na.omit(my_df)

my_sf <- st_as_sf(my_df_comp, coords = c('LAT', 'LON'))

my_sf <- st_set_crs(my_sf, 4326)
                  
ggplot(my_sf) + geom_sf(aes())
                  