# GLEON Workshop on Rdatatable 
# GLEON Limnoseries 2021 - August
# Author: Jorrit Mesman

# data.table website: https://rdatatable.gitlab.io/data.table/

##### Load packages and set environment #####

Sys.setenv(TZ = "UTC")

# Set working directory (the "data" folder should be inside this folder)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Set seed for reproducibility
set.seed(1234)

# Packages
library(data.table)
library(rLakeAnalyzer)
library(ggplot2)
library(lubridate)

##### Exercises #####
# The first four exercises are using one of the standard R datasets
# The last four exercises are about calculating onset of stratification
# using the dt_lake dataset. 

# Note that there might be multiple ways of doing these exercises.
# Try to use data.table where possible, but if you prefer to mix 
# with other packages; go ahead! 

### Exercise 1: Importing -----
# Import "mtcars.csv" in the "data" folder using fread, and name it "dt_cars"
# The data comes from the standard R "mtcars" dataset
# Source: Henderson and Velleman (1981). Biometrics, 37, 391-411.

# The first row are the headers, the second row the units
# Call fread such that the headers are read into R, 
# but the units are not.
# (hint: there are two ways;
#        1) (easiest) first importing without the two first rows (check
#        fread arguments), and then importing only the header and
#        setting the column names, or
#        2) (more difficult) importing it "normally", removing the row
#        with units, and then changing the column types)

## Answer
# Method 1
dt_cars = fread("./data/mtcars.csv", skip = 2)
setnames(dt_cars, unlist(fread("./data/mtcars.csv", nrows = 1, header = F)))

# Method 2
dt_cars = fread("./data/mtcars.csv")
dt_cars = dt_cars[-1] # Remove first row
mycols = names(dt_cars)[-1] # First column is not numeric
dt_cars[, (mycols) := lapply(.SD, as.numeric), .SDcols = mycols]

### Exercise 2 - Using .SD: -----
# Calculate the mean of the mpg, cyl, and disp columns in dt_cars,
# using .SD, but only for Mercedes cars ("Merc")

## Answer
dt_cars[grepl("Merc", name),
        lapply(.SD, mean),
        .SDcols = c("mpg", "cyl", "disp")]

# data.table provides a fast alternative for grepl as well
dt_cars[name %like% "Merc",
        lapply(.SD, mean),
        .SDcols = c("mpg", "cyl", "disp")]

### Exercise 3: Grouping -----
# In dt_cars, the "am" column stands for transmission (0 = automatic,
# 1 = manual). Change the "am" column from 0/1 to "automatic"/"manual",
# and then calculate the average horsepower ("hp" column) for each group

## Answer
dt_cars[, am := as.character(am)]
dt_cars[am == "0", am := "automatic"]
dt_cars[am == "1", am := "manual"]
dt_cars[, .(av_hp = mean(hp)), by = am]

# Alternative using data.table's fifelse
dt_cars[, am := fifelse(am == "0", "automatic", "manual")]
dt_cars[, .(av_hp = mean(hp)), by = am]

### Exercise 4: Pivoting -----
# First remove the "am" column used in the previous exercise

# Put dt_cars in long format, so that there are only three
# columns; name, quality, and value (the values of "quality" being
# "mpg", "cyl", "disp", etc.). Name this data.table "dt_cars_long"
# Sort dt_cars_long by "name"

# You might get a warning message that not all columns are the same type
# (because some are integers and others are "double"), but you may ignore that

## Answer
dt_cars[, am := NULL]

dt_cars_long = melt(dt_cars, id.vars = "name",
                    variable.name = "quality",
                    value.name = "value")

# Or, identical:
dt_cars_long = melt.data.table(dt_cars, id.vars = "name",
                    variable.name = "quality",
                    value.name = "value")

setorder(dt_cars_long, name)

### Exercise 5: Calculate onset of stratification Part 1 -----
# We'll park our cars and from now on we'll work with the Lake Erken
# water temperature dataset:
dt_lake = fread("./data/MorasEtAl_ErkenObsWTemp_1961-2017.obs")
setnames(dt_lake, c("date", "depth", "temp")) 
dt_lake[, depth := abs(depth)]

df_hyps = fread("./data/hypsograph_erken.dat")
df_hyps[, depths := abs(depths)]

# Let's define the lake as "stratified" when the difference
# between surface and deep-water temperature exceeds 1 °C
# "Surface" is not deeper than 1 m, "Deep-water" is at least
# 18 m depth (the lake's total depth is 21 m). 

# Finding the onset of stratification using data.table is rather easy,
# but when including some quality-control, as we'll do in exercises
# 7 and 8, it becomes complicated rather fast. Still, data.table offers
# a concise, understandable, and consistent way of dealing with these issues. 

# First, calculate the average surface and deep-water temperature
# for each date (you can take the simple arithmetic mean, but if you
# want to use rLakeAnalyzer's layer.temperature function and the hypsograph,
# you can, of course).
# Merge this information in a single dataset with columns date - surfT - deepT,
# and name it "dt_strat"

## Answer
dt_surf = dt_lake[depth <= 1, mean(temp), by = date]
dt_deep = dt_lake[depth >= 18, mean(temp), by = date]

# Using rLakeAnalyzer
dt_surf = dt_lake[, layer.temperature(0, 1, temp, depth,
                                      df_hyps$areas, df_hyps$depths),
                  by = date]
dt_deep = dt_lake[, layer.temperature(18, 21, temp, depth,
                                      df_hyps$areas, df_hyps$depths),
                  by = date]

# Merge
dt_strat = merge(dt_surf, dt_deep, by = "date", all = T)
setnames(dt_strat, c("date", "surfT", "deepT"))

### Exercise 6: Calculate onset of stratification Part 2 -----
# Using the datatable generated in exercise 5, make a new column named 
# "stratified" with TRUE if it the lake is stratified, and FALSE if it is not.
# Then, determine the first day of each year that the lake is stratified.
# (hint: use the first() function provided by the data.table package,
#  and the "by" argument of data.table)
# Name this resulting table "dt_result", which has columns "year" and "onset"

dt_strat[, stratified := surfT - deepT > 1]

dt_result = dt_strat[stratified == T,
                     first(date),
                     by = year(date)]
setnames(dt_result, c("year", "onset"))

### Exercise 7: Calculate onset of stratification Part 3 -----
# This looks good, but we're not there yet. Some years are missing from
# the dt_lake dataset, and sometimes the first measurement of the year 
# was already stratified, so onset of stratification can't be determined.

# 1) Add the missing years to dt_result
# (hint: dt_lake ranges from 1961 to 2017. You can use the merge function, and
# one of the "all*" arguments)

## Answer
dt_result = merge(dt_result, list("year" = 1961:2017), by = "year", all.y = T)

# 2) Set "onset" to NA if the previous measurement is longer ago than
#    7 days
#    (hint: use .I to get the row numbers of the onset in dt_strat, and check
#    the row number minus 1. You can use the lubridate package to help 
#    with calculations involving POSIXct)

## Answer
dt_indices = dt_strat[stratified == T,
                      .I[date == first(date)],
                      by = year(date)]
indices_measurement_before = dt_indices[, V1] - 1 # You could also use
                                                  # the "shift" function
dt_indices[, measurement_before := dt_strat[indices_measurement_before, date]]
dt_indices[, V1 := NULL]

dt_result = merge(dt_result, dt_indices, by = "year", all = T)

dt_result[difftime(onset, measurement_before) > lubridate::days(7),
          onset := NA]

dt_result[, measurement_before := NULL]
# So in the end, we only have reliable onset data from 1996 onwards
# And even after that date, onset cannot be determined from measurements
# in some years. 

### Exercise 8: Calculate onset of stratification Part 4 -----
# Lastly, we would like to only consider periods where the lake
# is stratified continuously for at least 10 days. Calculate onset of
# stratification using this criterion. Start with dt_strat (use the column
# that has TRUE/FALSE whether it is stratified or not). The first steps have
# been done.

# Ensure a fixed time step of 1 day, using merge
dt_strat = merge(dt_strat, list(date = seq.POSIXt(dt_strat[1L, date],
                                                  dt_strat[.N, date],
                                                  by = "1 day")),
                 by = "date",
                 all.y = T)

# Use base-R's rle() function to get lenths and values of adjacent
# numbers in a string; see what happens rle(c(1,1,1,2,2,3,1))
# Calculate by year
dt_strat_rle = dt_strat[, rle(stratified), by = year(date)]

# To avoid setting onset on a date with NA's before it, set all TRUE
# values to FALSE if the preceding measurement is NA (i.e. missing
# measurement), using data.table's shift function
preceding_measurement = shift(dt_strat_rle, n = 1L)[[3]]
dt_strat_rle[values == T & is.na(preceding_measurement),
             values := F]

# Next steps:
# 1) Find the index of the onset of stratification in dt_strat_rle 
#    for each year, assuming length should be >= 10

## Answer
index_onset = dt_strat_rle[lengths >= 10L &
                             values == T,
                           .I,
                           by = year]
index_onset = index_onset[, .(I = first(I)), by = year]

# 2) Using this information, find the date of onset of
#    stratification in dt_strat (hint: have a look at dt_strat_rle to
#    understand how to find the right index)

## Answer
# You need to sum all preceding indices
index_onset[, index_dt_strat := lapply(I,
                                       function(x) sum(dt_strat_rle[1:(x - 1),
                                                                    lengths]) + 1)]
index_onset[, index_dt_strat := unlist(index_dt_strat)]

# Then you know the index of the onset of stratification in dt_strat
index_onset[, onset := dt_strat[index_dt_strat, date]]

# 3) Plot the results (only the time period you feel is relevant)
#    (hint: you can use lubridate's yday() function to convert dates
#    to day-of-the-year)
## Answer

# Create a clean table and plot the results
dt_result2 = index_onset[, .(year, onset)]
dt_result2 = merge(dt_result2, list(year = 1961:2017), by = "year", all = T)
dt_result2[, DOY := lubridate::yday(onset)] # Convert to day-of-the-year

ggplot(dt_result2[year >= 1996])+
  geom_point(aes(year, DOY))+
  labs(x = "Year", y = "Day of the year",
       title = "Onset of stratification in Lake Erken")+
  theme_light()

### End of exercises -----
