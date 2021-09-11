
#dplyr introduction - Based on : http://rpubs.com/justmarkham/dplyr-tutorial
#reference Youtube tutorial - https://www.youtube.com/watch?v=jWjqLW-u3hc&t=1511s
#Video minutes - https://www.dataschool.io/dplyr-tutorial-for-faster-data-manipulation-in-r/

#All materials belong to dataschool.io . This is just an augmentation material for the references provided. 

suppressMessages(library(dplyr))
library(hflights)

if (!require(hflights)) install.packages("hflights")
suppressMessages(library(hflights))

# explore data
data(hflights)
head(hflights)


#tbl_df for pretty printing
flights <- tbl_df(hflights)

##########################  filter verb

# base R approach to view all flights on January 1
flights[flights$Month==1 & flights$DayofMonth==1, ]

filter(flights, Month==1, DayofMonth==1)

#Using chaining operator %>%
flights %>% filter(Month==1, DayofMonth==1)

#Stroting results
firstDayOfYearFlights<- flights %>% filter(Month==1, DayofMonth==1)

#flights of carrier AA and UA (union of selection. Hence OR is being used)
# use pipe for OR condition
filter(flights, UniqueCarrier=="AA" | UniqueCarrier=="UA")


##########################  select verb
#select: Pick columns by name

# base R approach to select DepTime, ArrTime, and FlightNum columns
flights[, c("DepTime", "ArrTime", "FlightNum")]

# dplyr approach
select(flights, DepTime, ArrTime, FlightNum)


#Using chaining operator %>%
flights %>% select(DepTime, ArrTime, FlightNum)


# use colon to select multiple contiguous columns, and use `contains` to match columns by name
# note: `starts_with`, `ends_with`, and `matches` (for regular expressions) can also be used to match columns by name
select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay"))




#### Using chaining operator for multiple operations

# dplyr nesting method to select UniqueCarrier and DepDelay columns and filter for delays over 60 minutes
filter(select(flights, UniqueCarrier, DepDelay), DepDelay > 60)

# chaining method
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  filter(DepDelay > 60)

#traditional R method
temp<- flights[,c("UniqueCarrier", "DepDelay")]
temp<- temp[temp$DepDelay>60,]
temp<- drop_na(temp)

##Chaining can be used anywhere in R
# Traditional Method: create two vectors and calculate Euclidian distance between them
x1 <- 1:5; x2 <- 2:6
sqrt(sum((x1-x2)^2))


# chaining method
(x1-x2)^2 %>% sum() %>% sqrt()


##########################  arrange verb
#arrange: Reorder rows

# base R approach to select UniqueCarrier and DepDelay columns and sort by DepDelay
flights[order(flights$DepDelay), c("UniqueCarrier", "DepDelay")]

# dplyr approach
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(DepDelay)

# use `desc` for descending
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(desc(DepDelay))

##########################  mutate verb
#mutate: Add new variables

# base R approach to create a new variable Speed (in mph)
flights$Speed <- flights$Distance / flights$AirTime*60
flights[, c("Distance", "AirTime", "Speed")]

# dplyr approach (prints the new variable but does not store it)
flights %>%
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime*60)


##########################  summarise with group_by verb
# summarise: Reduce variables to values
# Primarily useful with data that has been grouped by one or more variables
# group_by creates the groups that will be operated on
# summarise uses the provided aggregation function to summarise each group

#create custom dataset for example
smallData<- firstDayOfYearFlights %>%
  filter(UniqueCarrier %in% c('AA','AS','B6')) %>% 
  select(UniqueCarrier,AirTime,ArrDelay,DepDelay)

smallData

#No of flights by each carrier
smallData %>% group_by(UniqueCarrier) %>% summarise(n_flights = length(UniqueCarrier))

#Alternative
smallData %>% group_by(UniqueCarrier) %>% summarise(n_flights = n())

#Average Arrival Delay for all carriers
smallData %>% group_by(UniqueCarrier) %>% summarise(arrDelay = mean(ArrDelay))

######### end
