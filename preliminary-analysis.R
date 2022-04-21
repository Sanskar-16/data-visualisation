# Section 1: Early stages ----
# 
# original data - data
# copy of the data = datacopy
# cleaned data = df

# loading the file
data <- read.csv('37-00049_UOF-P_2016_prepped.csv')
# attach(data)

# loading the libraries
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(ggmosaic)
library(plotrix)
library(chron)

# creating a copy of the data to experiment on so that it does not contaminate
datacopy = data.frame(data)

# preliminary analysis, checking the dimension and the type of the variables
str(datacopy)
cat_var <- names(datacopy)[which(sapply(datacopy, is.character))]
# print(cat_var)
# tells us all the variables are categorical.

# checking for missing values in the data.
colSums(sapply(datacopy, is.na)) # tells us that none of the variables have null values, but we do notice that
datacopy <- datacopy[-c(1),] # removing the extra definition row at the first index
str(datacopy)

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

# converting desired variables into numerics
datacopy$UOF_NUMBER <- as.numeric(datacopy$UOF_NUMBER)
datacopy$OFFICER_ID <- as.numeric(datacopy$OFFICER_ID)
datacopy$SUBJECT_ID <- as.numeric(datacopy$SUBJECT_ID)
datacopy$OFFICER_YEARS_ON_FORCE <- as.numeric(datacopy$OFFICER_YEARS_ON_FORCE)

# converting desired variables into factors
datacopy$OFFICER_GENDER <- as.factor(datacopy$OFFICER_GENDER)
datacopy$OFFICER_RACE <- as.factor(datacopy$OFFICER_RACE)
datacopy$SUBJECT_RACE <- as.factor(datacopy$SUBJECT_RACE)
datacopy$SUBJECT_GENDER <- as.factor(datacopy$SUBJECT_GENDER)
datacopy$OFFICER_INJURY <- as.factor(datacopy$OFFICER_INJURY)
datacopy$OFFICER_INJURY_TYPE <- as.factor(datacopy$OFFICER_INJURY_TYPE)
datacopy$SUBJECT_INJURY_TYPE <- as.factor(datacopy$SUBJECT_INJURY_TYPE)
datacopy$OFFICER_HOSPITALIZATION <- as.factor(datacopy$OFFICER_HOSPITALIZATION)

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

# Section 3: ----
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
                  