# loading the file

data <- read.csv('37-00049_UOF-P_2016_prepped.csv')

# preliminary analysis

# having a brief look at the data set's dimensions and columns
str(data)

# checking for null values
summary(data)

# viewing the initial entries
head(data)

library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(ggmosaic)

# removing useless columns 
data1 <- select(data, -c(LOCATION_STATE, LOCATION_CITY))
data1 <- data1[-c(1),] # removing the extra definition row at the first index

# finding what index holds value for this variable 'Type_of_force_used10'
which(data1$TYPE_OF_FORCE_USED10 == unique(data1$TYPE_OF_FORCE_USED10)[2])

barplot(table(data1$OFFICER_GENDER))
barplot(table(data1$OFFICER_RACE))

# creating a filter to view 
head(data %>% filter(OFFICER_RACE == "Black"))
