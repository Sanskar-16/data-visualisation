# loading the file

data <- read.csv('37-00049_UOF-P_2016_prepped.csv')

# preliminary analysis

# gaving a bried look at the dataset's dimensions and columns
str(data)

# checking for null values
summary(data)

# viewing the initial entries
head(data)

library(dplyr, warn.conflicts = FALSE)
head(data %>% filter(OFFICER_RACE == "Black"))
     
     