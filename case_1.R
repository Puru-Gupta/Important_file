library(dplyr)
library(hflights)

flight = data.frame(hflights)
head(flight)
#filter

filter(flight, Month ==1 & DayofMonth==1, )
filter(flight, Month==1 | DayofMonth ==1, )

#Select
select(flight, Year:DayofMonth, DayOfWeek, DepTime)
colnames(select(flight, Year:DayofMonth, contains("Taxi"), contains("Delay")))


#chaining method
flight %>% select(UniqueCarrier, DepDelay) %>% filter(DepDelay >60)

#arrange

flight %>%  select(UniqueCarrier, DepDelay) %>% arrange(DepDelay)       
flight %>%  select(UniqueCarrier, DepDelay) %>% arrange(desc(DepDelay))

#approach (print the new variable but does not store it)
flight %>% 
    select(Distance, AirTime) %>% mutate(Speed = Distance/AirTime*60)
    
#to store new variable
flight <- flight %>% mutate(Speed = Distance/AirTime*60)
flight  
  

#create the table group by destination and then summaries each group 
#by taking the mean of the ArrDelay

flight %>% group_by(Dest) %>% summarise(avg_delay = mean(ArrDelay, na.rm = T))

# for each carrier, calculate the percetage of flights cancelled or diverted
# for each means use group by

flight %>%  group_by(UniqueCarrier) %>%  summarise_each(funs(mean), Cancelled, Diverted)

#for each carrier, calculate the min and max arrivel and departure delays

flight %>% group_by(UniqueCarrier) %>% summarise_each(funs(min(., na.rm=T),max(. ,na.rm = T)), matches("Delay"))

# Helper function n() counts the number of rows in group
# helper function n_distinct(vector) counts the number of unique items in that vector

#for each day  of the year, count the total number of flight and sort in decending order
flight %>% group_by(Month, DayofMonth) %>% summarise(flight_count = n()) %>% arrange(desc(flight_count))

#Rewrite more simply with tally function
flight %>% group_by(Month, DayofMonth) %>% tally(sort = TRUE)

# for each destination, count the total number of flights and number of distinct planes that flew there
flight %>% group_by(Dest) %>% summarise(flight_count =n(), plane_counts = n_distinct(TailNum))

# for each destination, show the number of cancelled and not cancelled flight

flight %>% group_by(Dest) %>% select(Cancelled) %>% table() %>%  head()
