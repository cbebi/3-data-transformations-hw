#PSYC 259 Homework 3 - Data Transformation
#For full credit, provide answers for at least 7/11

#List names of students collaborating with: 
# Maddy,Aylin, and Megan (our row in the front haha)

### SETUP: RUN THIS BEFORE STARTING ----------

#Load packages
library(readr)
ds <- read_csv("Desktop/Data Science Fall 2026/Homework for Class/3-data-transformations-hw/data_raw/rolling_stone_500.csv")
View(ds)

### Question 1 ---------- 

#1. Use a function to check the class of all of the variables in 'ds' 
#2. Convert "Year" to a numeric, saving it back to 'ds'
#3. Use a function to check that your conversion succeeded

#ANSWER
#1.
class(ds$Rank)
class(ds$Year)
class(ds$Song)
class(ds$Artist)

#2.
ds$Year <- as.numeric(ds$Year)

#3.
glimpse(ds)

### Question 2 ---------- 

# Using a dplyr function,
# change ds so that all of the variables are lowercase

#ANSWER
library(dplyr)
install.packages("janitor")
library(janitor)
ds <- ds %>% rename(rank=Rank)
ds <- ds %>% rename(year=Year)
ds <- ds %>% rename(artist=Artist)
ds <- ds %>% rename(song=Song)
glimpse(ds)

### Question 3 ----------

# Use mutate to create a new variable in ds that has the decade of the year as a number
# For example, 1971 would become 1970, 2001 would become 2000
# Hint: read the documentation for ?floor and make sure year is numeric (from Question 1)

#ANSWER

#1.
?floor
class(ds$year)
#Verified that year is numeric
ds <- ds %>%
  mutate(decade = floor(year/10)*10)
glimpse(ds)
#now there is a new row called decade in the ds data frame

### Question 4 ----------

# Sort the dataset by rank so that 1 is at the top

#ANSWER
ds<-ds %>%
  arrange(rank)
glimpse(ds)
#we can see that rank is organized 1-500 now 
### Question 5 ----------

# Use filter and select to create a new tibble called 'top10'
# That just has the artists and songs for the top 10 songs

#ANSWER
filter(ds,rank<=10)
select(ds, rank,song,artist)
#a new tibble was made, but I did not assign back to ds
### Question 6 ----------

# Use summarize to find the earliest, most recent, and average release year
# and save the results to a new tibble called "ds_sum"
# Hint, the function drop_na() might help. Read ?drop_na

#ANSWER
?drop_na
#Can I also use the na.rm=T to acknowledge that there are NA's?

#I tried both ways 
#The first way
ds_sum <- ds %>% summarise(min_year=min(year, na.rm = T), 
                 max_year= max(year, na.rm = T), 
                 average_year= mean(year, na.rm = T))
glimpse(ds_sum)


#the second way
install.packages("tidyr")
library(tidyr)
ds_sum <- ds %>%
  drop_na(year) %>%
  summarise(
    min_year = min(year),
    max_year = max(year),
    average_year = mean(year)
  )
glimpse(ds_sum)

### Question 7 ----------

# Use filter on the original dataset 'ds' to find all of the artists/song titles from 
# the min/max/mean years that you identified in 'ds_sum'
# Use one filter command only, and sort the responses by year
# Bonus: find a way to do it without hard coding the years (i.e. pull them from ds_sum)
# (this might take some searching to find functions that will help)

#ANSWER
ds %>%
  filter(year== ds_sum$min_year) %>%
  select( artist, song, year)

ds %>%
  filter( year== ds_sum$max_year) %>%
  select(artist,song, year)

ds %>%
  filter(year==ds_sum$average_year) %>%
  select(artist,song, year)

# or like this if we put it all together
ds %>%
  filter(year %in% c(ds_sum$min_year,
                     ds_sum$max_year,
                     round(ds_sum$average_year))) %>%
  select(year, artist, song) %>%
  arrange(year)


### Question 8 ---------- 

# There's an error here. The oldest song "Brass in Pocket"
# is from 1979! Use mutate and ifelse to fix the error in 'ds'
# then recalculate decade, and then
# recalculate the responses from Questions 6-7 to
# find the correct oldest, averag-ist, and most recent songs

#ANSWER
ds <- ds %>%
  mutate(year = ifelse(song == "Brass in Pocket", 1979, year))
#Yes, this shows us it worked!
ds %>%
  filter(song == "Brass in Pocket") %>%
  select(artist, song, year)

#The correct, oldest, averag-ist songs
# Cross Road Blues 1937
# Safaera, Dynamine, and Savage are all from 2020


### Question 9 ---------

# Use group_by and summarize to find the average rank and 
# number of songs on the list by decade. To make things easier
# filter out the NA values from decade before summarizing
# You don't need to save the results anywhere
# Use the pipe %>% to string the commands together

#ANSWER
ds_filtered <- filter_out(ds,NA)
glimpse(ds_filtered)
ds %>% group_by(song, decade) %>% summarise(average_rank = mean(rank, ns.rm=T))

### Question 10 --------

# Look up the dplyr "count" function
# Use it to count up the number of songs by decade
# Then use slice_max() to pull the row with the most songs
# Use the pipe %>% to string the commands together
# Check against your answers from Question 9 to make sure it matches


#ANSWER
count( ds_filtered, decade)
slice_max(ds_filtered, decade)


#piped together
ds_filtered %>%
  count(decade) %>%
  slice_max(decade)

### Question 11 ------
# Use RStudio's styler to style all the code in your homework

install.packages("styler")


