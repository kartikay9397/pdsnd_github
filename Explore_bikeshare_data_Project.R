
install.packages('ggplot2')
install.packages('plyr')
install.packages('dplyr')

# Running all the needed libraries

library(ggplot2)
library(plyr)
library(dplyr)

# Importing all the needed data 

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')


# Here i am removing those columns from 'ny' and 'chi' which are not present in 'wash' so that i can
# easily use the rbind(), since all the three table will have equal number of columns after the '
# removal. 

ny_1 <- ny[,-9]  # Here i have removed the ninth column from the table 'ny'
ny_2 <- ny_1[,-8] # Here i have removed the eighth column from the table 'ny_1'

chi_1 <- chi[,-9] # Here i have removed the ninth column from the table 'chi'
chi_2 <- chi_1[,-8] # Here i have removed the eighth column from the table 'chi_1'

cities_data <- rbind(ny_2,wash,chi_2)  # Now i have combined all the rows of the three tables
cd <- cities_data
head(cities_data)

# First of all we have made a function named "myplot"using the pipe operator 
# from dplyr library, we are applying count, choosing it's top 10 count and then 
# arranging them in descending order respectively on the data variable cities_data
# according to the questions asked. 

# Also then from ggplot where we have plotted the bar graphs here from the
# above accordingly derived data variables where names is on x-axis and counts 
# is on y-axis. Also all proper labellingof the whole graph is done using 
# ggtitle(), xlab() and ylab(). Some adjustments are done too in the text used 
# on x-axis to avoid unnecessary clutter.  


myplot <- function(cl, cnt,colour){
  column_text <- gsub("\\."," ",cl)
  cl<- ensym(cl)
  ggplot(cities_data %>%
           count(!!cl) %>%
           top_n(cnt) %>%
           arrange(desc(n)),aes(x=!!cl, y=n))+
    geom_col() +
    ggtitle(paste("Top", cnt,column_text))+
    xlab(column_text)+
    ylab(paste("Count of",column_text))+
    theme(axis.text.x = element_text(angle=45, hjust=1 , size =8))
}

## Let us check out the summary of our whole dataset which gives us a quick
# exploration as well as good insights about our data

summary(cities_data)

# This summary gives us a very good briefing about our data. It has given us all
# the meaures like Min, 1st Quartile, 3nd Quartile, Mean, Median, Mode, Max for
# the numeric part of data and gives us the ordered numbers for count of each
# Start station, End Station, User type. 




## What is the most common start station?


## Also we are plotting here only for the top 10 most common Start.Stations 
## instead of all because the number of observations for all is very large
## and it cannot be visualised properly in a neat manner on our plot. 

# Now using the function myplot which we have derived above we are plotting the
# graph of Start Stations. 

## Also here the top 10 start stations are 11 in number because two of these stations have 
## same count of stations

myplot('Start.Station',11)

# Now for Verification to check that the name and count of the most common
# Start.Station matches that shown by our plot, we have coded as follows.
# And also we check out some of our summary statistics to analyse more about data

head(summary(cities_data$Start.Station),1)

mean(summary(cities_data$Start.Station))
median(summary(cities_data$Start.Station))

# The MEAN of the Start Station tells us the average Number of Passengers who 
# started their journey from a station. But, when there is such a large dataset
# then MEDIAN provides a better measure of central tendency.



## What are the counts of each user type


# Now using the function myplot which we have derived above we are plotting the
# graph of User Type. 

myplot('User.Type',3)

# Now for Verification to check that the count of each user type matches that 
# shown by our plot, we have coded as follows,therefore also analysing some of
# our summary statistics to analyse more. 

table(cities_data$User.Type)

# Now this tells us that there is also another class which is neither a customer
# or a subscriber, althogh the count of this class is very less (121). 




## What is the most common end station?


## Again, we are plotting here only for the top 10 most common End.Stations 
## instead of all because the number of observations for all is very large
## and it cannot be visualised properly in a neat manner on our plot. 

# Now using the function myplot which we have derived above we are plotting the
# graph of End Stations. 

myplot('End.Station',10)

# Similarly again, for verification to check that the name and count of the most
# common End.Station matches that shown by our plot, we have coded as follows.
# And also we check out some of our summary statistics to analyse more about data

head(summary(cities_data$End.Station),1)

mean(summary(cities_data$End.Station))
median(summary(cities_data$End.Station))

# The MEAN of the End Station tells us the average Number of Passengers who 
# ended their journey from a station. But, when there is such a large dataset
# then MEDIAN provides a better measure of central tendency.



                        # SOME MORE INTERPRETATION

# Obviously the MEAN of both, the start stations as well as the end stations 
# comes out to be same since we know that the mean number of inflow of passengers
# is equal to the mean number of outflow of passengers which is very clear.
# But, the MEDIAN of end station data is more than that of start stations which
# tells us that the end station data is more centralized, the scatter is 
# comparatively lesser and the outliers too are comparatively lesser than from
# the start station. 



