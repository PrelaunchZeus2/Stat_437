library(nycflights13)
library(dplyr)
head(flights) #check first rows
str(flights) #check object types
#filter(filter{dpylr}), if code breaks try dplyr::filter(data, ...)
dplyr::filter(flights, year == 2013, month %in% c(11,12), tailnum == 'N14228') #select rows from flights where the year is 2013, month is 11 or 12 and tail number is N14228

#or select operation where arr_delay is <= 120 and dep_delay is <= 120
filter(flights, !arr_delay > 120 | dep_delay > 120)# | is or for single values, for vectors use || for element wise and/or

#arrange is the order operation
#arrange(arrange{dplyr}); arrange(data, ...)
#order rows of flights by year number, month number, and day number
arrange(flights, year, month, day)

#select keeps columns whose names are specified
#select(select{dplyr}); select(data, ...)
#keep columns of flights with column names year up till day
select(flights, year:day)

#select all columns EXCEPT (-) those from year to day
select(flights, -(year:day))

#mutate adds columns to the end of the dataset (tibble)
#mutate(mutate{dplyr}); mutate(data, ...)
#similar to apply or sapply in that it can take a simple user defined function

#create smaller df by selecting some columns
flights_sml = select(flights, year:day, ends_with("delay"), distance,air_time)
head(flights_sml)

#add a new column that calculates the speed of the flight as the distance / air tme * 60 for minutes
newA = mutate(flights_sml, gain = arr_delay - dep_delay, speed = distance / air_time * 60)
newA[1:3,(ncol(newA)-3):ncol(newA)]

#Pipe Operator
# %>%(%>%{dplyr}); value %> operation

#pick some columns to create smaller data set
flights_sml = flights %>% select(year:day) #performs the select operation on the flights dataset
head(flights_sml)

#sound barrier
sound = newA %>% filter(speed >= 720)
head(sound)
#no flights break sound barrier

#summarise grouped data created by group_by the output has one row for each group
#summarise(summarise{dplyr}); summarise(data, ...)
summarise(newA) #doesnt do anything because there are no groups

#related to HW1
#obtain mean arr_delay by moth and carrier for 2 months and 3 carriers
tmp = flights %>% select(month, arr_delay, carrier) %>% filter(month %in% 1:2, carrier %in% c("UA", "AA", "US"))
tmp = na.omit(tmp)
sm = tmp %>% group_by(month, carrier) %>% summarise(mean_arr_delay = mean(arr_delay)) %>% as.data.frame
head(sm)

#carriers
flights %>% count(carrier)

#unloadNamespace("package") deloads packages
#for example if plyr is loaded and dplyr is loaded they share the namespace for the group_by command
#so plyr might need to be deloaded or the commands need to be dplyr::command(args, kargs)
