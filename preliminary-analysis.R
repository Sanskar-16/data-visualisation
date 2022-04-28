# 1. Loading data sets and libraries ----

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
library(corrplot)

# loading the file
data <- read.csv('37-00049_UOF-P_2016_prepped.csv')

# creating a copy of the data to experiment on so that it does not contaminate
datacopy = data.frame(data)
attach(datacopy)

# preliminary analysis, checking the dimension and the type of the variables
str(datacopy)
names(datacopy)[which(sapply(datacopy, is.character))]
# tells us all the variables are of the datatype character, so we assign proper
# data types to the variables.


# 2. Cleaning ----
datacopy <- datacopy[-c(1),] # removing the extra definition row at the first index
# assigning all the "" and "NULL" to NA for easier manipulation of the data down the road.
datacopy[datacopy=="NULL"] = NA
datacopy[datacopy==""] = NA

# checking for missing values in the data.
colSums(sapply(datacopy, is.na)) 

# changing the dates to date format
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
datacopy$LOCATION_LATITUDE <- as.numeric(datacopy$LOCATION_LATITUDE)
datacopy$LOCATION_LONGITUDE <- as.numeric(datacopy$LOCATION_LONGITUDE)

# converting desired variables into factors
datacopy$OFFICER_GENDER <- as.factor(datacopy$OFFICER_GENDER)
freq(datacopy$OFFICER_GENDER)
datacopy$OFFICER_RACE <- as.factor(datacopy$OFFICER_RACE)
freq(datacopy$OFFICER_RACE)
datacopy$SUBJECT_RACE <- as.factor(datacopy$SUBJECT_RACE)
freq(datacopy$SUBJECT_RACE, na.rm = TRUE) # tells us the highest percentage of the subject race is black which shows a race bias towards the subjects.
datacopy$SUBJECT_GENDER <- as.factor(datacopy$SUBJECT_GENDER)
freq(datacopy$SUBJECT_GENDER, na.rm = TRUE)
datacopy$OFFICER_INJURY <- as.factor(datacopy$OFFICER_INJURY)
freq(datacopy$OFFICER_INJURY)
datacopy$SUBJECT_INJURY <- as.factor(datacopy$SUBJECT_INJURY)
freq(datacopy$SUBJECT_INJURY)
datacopy$OFFICER_INJURY_TYPE <- as.factor(datacopy$OFFICER_INJURY_TYPE)
freq(datacopy$OFFICER_INJURY_TYPE)
datacopy$SUBJECT_INJURY_TYPE <- as.factor(datacopy$SUBJECT_INJURY_TYPE)
freq(datacopy$SUBJECT_INJURY_TYPE)
datacopy$OFFICER_HOSPITALIZATION <- as.factor(datacopy$OFFICER_HOSPITALIZATION)
freq(datacopy$OFFICER_HOSPITALIZATION)
datacopy$SUBJECT_WAS_ARRESTED <- as.factor(datacopy$SUBJECT_WAS_ARRESTED)
freq(datacopy$SUBJECT_WAS_ARRESTED)
datacopy$SUBJECT_DESCRIPTION <- as.factor(datacopy$SUBJECT_DESCRIPTION)
freq(datacopy$SUBJECT_DESCRIPTION)
datacopy$DIVISION <- as.factor(datacopy$DIVISION)
freq(datacopy$DIVISION)
datacopy$LOCATION_DISTRICT <- as.factor(datacopy$LOCATION_DISTRICT)
freq(datacopy$LOCATION_DISTRICT)
datacopy$STREET_DIRECTION <- as.factor(datacopy$STREET_DIRECTION)
freq(datacopy$STREET_DIRECTION)
datacopy$STREET_TYPE <- as.factor(datacopy$STREET_TYPE)
freq(datacopy$STREET_TYPE)
datacopy$INCIDENT_REASON <- as.factor(datacopy$INCIDENT_REASON)
freq(datacopy$INCIDENT_REASON)
datacopy$REASON_FOR_FORCE <- as.factor(datacopy$REASON_FOR_FORCE)
freq(datacopy$REASON_FOR_FORCE)

# unchanged col 

# "NUMBER_EC_CYCLES"    
# [3] "UOF_NUMBER                    
# [19] "SUBJECT_OFFENSE"STREET_NAME                    
# [29] "LOCATION_FULL_STREET_ADDRESS_OR_INTERSECTION"                          

# "TYPE_OF_FORCE_USED1"                         
# [37] "TYPE_OF_FORCE_USED2"                          "TYPE_OF_FORCE_USED3"                         
# [39] "TYPE_OF_FORCE_USED4"                          "TYPE_OF_FORCE_USED5"                         
# [41] "TYPE_OF_FORCE_USED6"                          "TYPE_OF_FORCE_USED7"                         
# [43] "TYPE_OF_FORCE_USED8"                          "TYPE_OF_FORCE_USED9"                         
# [45] "TYPE_OF_FORCE_USED10"                                                 

# # combining columns 
# data$alltypesofforce <- paste(data$TYPE_OF_FORCE_USED1, data$TYPE_OF_FORCE_USED2, data$TYPE_OF_FORCE_USED3, data$TYPE_OF_FORCE_USED4, data$TYPE_OF_FORCE_USED5, data$TYPE_OF_FORCE_USED6, data$TYPE_OF_FORCE_USED7, data$TYPE_OF_FORCE_USED8, data$TYPE_OF_FORCE_USED9, data$TYPE_OF_FORCE_USED10)

# removing useless columns 
datacopy <- select(datacopy, -c(LOCATION_STATE, LOCATION_CITY))

# viewing the changed data
str(datacopy)

# converting the cleaned data frame into a csv file for further analysis 
write.csv(datacopy, "../data-visualisation/cleaned_data.csv", row.names = FALSE)
# as we notice the data is filled with categorical varibles, we pass the varibale

rm(datacopy)
df <- read.csv("cleaned_data.csv")
str(df)

# finding what index holds value for this variable 'Type_of_force_used10'
# which(df$TYPE_OF_FORCE_USED10 == unique(df$TYPE_OF_FORCE_USED10)[2])




# 3. exploring the data set after cleaning ----

# setting numerical variables and categorical variables 
numericVars <- which(sapply(df, is.numeric))
numericVarNames <- names(numericVars) 
cat('There are', length(numericVars), 'numeric variables')

carVars <- which(sapply(df, is.factor))
catVarNames <- names(catVars)
cat('The are', length(catVars), 'categorical variables')

# correlation checks for numerical variables
all_numVar <- datacopy[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs")
cor_sorted <- as.matrix(sort(cor_numVar[,'SUBJECT_ID'], decreasing = TRUE))
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")




# 4. EDA ----

# plotting the variable officer_race as pie char to visualize the different races
ggplot(df, aes(x=factor(1), fill=OFFICER_RACE))+
  geom_bar(width = 1)+
  coord_polar("y")

# subplots
par(mfrow=c(2,2))
hist(table(df$OFFICER_RACE))
pie(table(df$OFFICER_RACE), radius = 1)
hist(table(df$OFFICER_GENDER))
pie(table(df$OFFICER_GENDER))

barplot(table(df$OFFICER_GENDER))
barplot(table(df$OFFICER_RACE))

ggplot(df) + geom_bar(aes(x = OFFICER_YEARS_ON_FORCE))
ggplot(df) + geom_bar(aes(x = SUBJECT_WAS_ARRESTED, fill = OFFICER_RACE)) + scale_fill_viridis_d()
ggplot(df) + geom_bar(aes(x = OFFICER_RACE))

i <- 0
for(i in ncol(df)) {
  ggplot(df) + geom_bar(aes(x = i))
  print('yes')
}




# 5. Geo spatial mapping ----
# plotting lat vs long
library(sf)
library(ggplot2)
library(mapview)

# copying columns into new data frame
attributes(df)
str(df)
my_df <- df[, c(23, 24, 30,31)]

# changing variable names
names(my_df)[3] <- "LAT"
names(my_df)[4] <- "LON"

# handling missing values
my_df <- na.omit(my_df)

# viewing the new data frame for plotting the map
str(my_df)

# setting the plot size

# my_sf <- st_as_sf(my_df_comp, coords = c('LAT', 'LON'))
# 
# my_sf <- st_set_crs(my_sf, 4326)
# 
# sf::sf_use_s2(FALSE)
# 
# ggplot(my_sf) + geom_sf(aes(colour = DIVISION))

# plotting the data using map view library
mapview(my_df, 
        xcol = "LAT", 
        ycol = "LON",
        zcol = 'DIVISION', 
        crs = 4326, 
        grid = FALSE, 
        map.types = "Stamen.Toner") # simple graph

mapview(my_df, 
        xcol = "LON", 
        ycol = "LAT", 
        zcol = 'LOCATION_DISTRICT', 
        crs = 4326, 
        grid = FALSE, 
        map.types = "Stamen.Toner")

mapview(my_df, zcol='DIVISION')

