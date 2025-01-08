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

