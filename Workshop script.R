# GLEON Workshop on Rdatatable 
# GLEON Limnoseries 2021 - August
# Author: Jorrit Mesman

# data.table website: https://rdatatable.gitlab.io/data.table/

##### Load packages and set environment #####

# Set time zone to UTC to avoid issues with local settings.
# For using data.table, there is an additional advantage as
# reading times in UTC is faster (but this becomes default soon
# anyways). 
Sys.setenv(TZ = "UTC")

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Set seed for reproducibility
set.seed(1234)

# Packages (if needed, install using the install.packages function)
library(data.table)

# We use rLakeAnalyzer to show that data.table can be used together with other packages
library(rLakeAnalyzer)

# And we'll use ggplot2 to plot the results
library(ggplot2)

# Optional: you can use lubridate to work with POSIXct data
#           in the exercises at the end. 
#           (although data.table also provides some functions for POSIXct handling)
library(lubridate)

##### Basic data.table functionality #####

### Creating data.tables ----
## Manually
dt_small = data.table("letter" = rep(c("A", "B", "C", "D"), each = 4),
                      "rank" = 1:16,
                      "number" = rnorm(16))
# Also as.data.table to convert from data.frame

class(dt_small)

df_small = data.frame(dt_small)

## Read from data
dt_lake = fread("./data/MorasEtAl_ErkenObsWTemp_1961-2017.obs") 
# Daily data of water temperature profiles from Lake Erken
# Downloaded from: https://doi.org/10.4211/hs.54375615d258461086125d5fc85a4c32
# Moras et al. (2019). Hydrology and Earth System Sciences 23(12): 5001-5016.

str(dt_lake)

colnames(dt_lake) = c("date", "depth", "temp")

dt_lake$depth = abs(dt_lake$depth) # Later we learn a better way of doing this

# fread: One function to read them all (any delimiter) and very fast
# Also reads URLs and you can even give a command to command
# prompt/terminal e.g. to unpack zips

# Dates already read in POSIXct, because we set our time zone to UTC!
# Also option to use argument tz = "UTC" (will become default)
# Otherwise it will be in character

### Subsetting (i) ----
dt_small[rank < 10]
# No need for "dt_small$"
# A comma at the end as in data.frames is optional, no longer required

dt_small[3:5]

### Select columns and calculate (j) ----
## Selecting columns
dt_small[, 1:2]

dt_small[, c("letter", "rank")]
# dt

dt_small[, list(letter, rank)]
# dt

dt_small[, "letter"]
# dt

df_small[, "letter"]
# data.frame: vector

dt_small[, list(letter)]
# dt

dt_small[, letter]
# vector
# Or:
# dt_small[, "letter"][[1]]


## Calculations
dt_small[, mean(number)]

dt_small[, list(mu = mean(number),
                sigma = sd(number))]

# data.table's alternative for list() is .()
dt_small[, .(mu = mean(number),
             sigma = sd(number))]

# Combine with i
dt_small[rank < 10, mean(number)]

### Grouping (by) ----
# Always in conjunction with j; what do you want to do per group
dt_small[, min(rank), by = letter]

# Can also be multiple groups and conditions
ans = dt_lake[, mean(temp), by = .(date, depth < 10)]
View(ans) # TRUE is shallower than 10 m

# i, j, by
ans = dt_lake[depth <= 1,
              mean(temp),
              by = year(date)]

ans = dt_lake[depth <= 1,
              mean(temp),
              by = .(year(date), month(date))]

ans = dt_lake[depth <= 1,
              .(av_temp = mean(temp)),
              by = .(year(date), month(date))]

### .N ----
# You will often want the count per group.
# Instead of using nrow() all the time, you can use .N

dt_small[, .N, by = letter]

dt_small[, .N, by = number > 0]

# It can also be used to get the last row
dt_small[.N]

### Updating by reference ---- 
# This is an important aspect of data.table, as it is one of the
# main things that makes it so fast.
# For this, we use the walrus operator :=

## Make a new column by reference
dt_small[, new_col := rnorm(16, 0.5, 0.1)]
# Rstudio hasn't updated...
ncol(dt_small)
View(dt_small)

## Modify a column by reference
dt_small[rank <= 3, new_col := new_col + 5]

## Remove a column by reference
dt_small[, new_col := NULL]

# Important: as part of updating by reference, you will not have to
# use "dt =" in front of a statement with the walrus operator! 

## Add/modify/remove multiple columns at once
dt_small[, `:=`(new_col = 16:1,
                another_one = "Z")]
dt_small[, `:=`(new_col = NULL,
                another_one = NULL)]

### Updating by reference using set_ functions ----
# data.table provides several functions that start with "set"
# All these functions also work by reference. So no "dt ="

# Order by certain columns (- for reverse ordering)
# Multiple columns can be added
setorder(dt_small, -rank)
View(dt_small)
setorder(dt_small, rank)

# Change column names
setnames(dt_small, c("Letter", "Rank", "Number"))
View(dt_small)

# Change a data.frame/tibble/list to data.table (no more as.data.table)
# or back to data.frame
setDT(df_small)
class(df_small)
setDF(df_small)
class(df_small)

## set-function
# set() can be used e.g. inside loops to change a single value in a data.table
# It avoids computational overhead of calling `:=` every time,
# As in base R, you should generally avoid modifying columns in a for-loop
# but this is for situations where this can't be avoided.

# Set Number (i.e. 3rd column) in line 8 to 100
set(dt_small, i = 8L, j = 3L, 100)
dt_small[8L, Number]

# Would do the same thing, but slightly slower if repeated often
dt_small[8L, Number := 100]

### Writing data.tables ----

# Write the lake data of the year 2000 to a file
fwrite(dt_lake[year(date) == 2000], "./data/lakedata_2000.txt", sep = "\t")

# The default format of POSIXct is ISO-format (yyyy-mm-ddTHH:MM:SSZ) (Z for UTC)
# If you want to write yyyy-mm-dd HH:MM:SS, you need to add this as an argument
fwrite(dt_lake[year(date) == 2000], "./data/lakedata_2000.txt",
       sep = "\t", dateTimeAs = "write.csv")

## Let's have a break!

##### More advanced data.table functionality #####
# To start with data.table, the above functions are likely enough.
# However, there are some other handy things data.table can do.

### Switch between long and wide format (reshaping/pivoting) ----
# Use data.table::melt and dcast functions (same name as base R)
# Same use, just faster

# Put in rLakeAnalyzer format
dt_lake_wide = dcast(dt_lake, date ~ depth, value.var = "temp")
setnames(dt_lake_wide, c("date",
                         paste0("wtr_", names(dt_lake_wide)[-1])))

# Put back into long format
dt_lake_long = melt(dt_lake_wide,
                    id.vars = "date",
                    variable.name = "depth",
                    value.name = "temp")
# Depth (also if using base R melt) is now in factors, so we need to correct that
dt_lake_long[, depth := as.character(depth)]
dt_lake_long[, depth := as.numeric(gsub("wtr_", "", depth))]
setorder(dt_lake_long, date, depth)

### .SD - Do an operation on multiple columns ----
# data.table introduces the .SD special symbol, to help applying a function
# to multiple columns. SD stands for "Subset of Data"
# .SD refers to all columns in the data.table itself, excluding
# those mentioned in the "by" column. Columns can be specified
# using the .SDcols argument. Let's see how this works. 

dt_small[, .SD]

dt_small[, .SD, .SDcols = c("Letter", "Number")]

# What you want to avoid:
dt_lake_wide[, .(is.numeric(date),
                 is.numeric(wtr_0.05),
                 is.numeric(wtr_0.5),
                 is.numeric(wtr_1))] # etc. for all columns

# What you can do:
dt_lake_wide[, lapply(.SD, is.numeric)]

# Only on certain columns
mycols = c("wtr_1", "wtr_5", "wtr_8")
dt_lake_wide[, lapply(.SD, is.numeric), .SDcols = mycols]

# .SDcols can also be a condition
dt_lake_wide[, lapply(.SD, function(x) mean(x, na.rm = T)),
             .SDcols = is.numeric]

# When you group, you usually don't want to apply a function
# to the grouping variable, which is why grouped variables
# are not included in .SD
dt_small[, lapply(.SD, mean), by = "Letter"]

# Is identical (but more typing especially with many columns) to:
dt_small[, .(Rank = mean(Rank),
             Number = mean(Number)),
         by = "Letter"]

### Aliasing and copy() ----

# This is something that may lead to confusion if you are not
# aware of this. Since data.tables work with referencing, making
# a copy of the data may lead to aliasing; two references to the same object. 

dt_small[2:4, Number]
dt_alias = dt_small
dt_alias[2:4, Number := Number * 2]
dt_small[2:4, Number]
# !!!
# You almost never want this. 

# This can also happen in functions. Although this allows you to
# make you own set_ functions, often this is unintended. 

calc_largest_diff = function(dt, col_name){
  # You plan to calculate the largest difference in a column
  # of a data.table. To do this, you'd like to add a new column.
  dt[, diff := c(NA, diff(dt[[col_name]]))]
  
  max(dt[, diff], na.rm = T)
}

calc_largest_diff(dt_small, "Number")
View(dt_small)
# Now a new column, diff, has been added to dt_small!
dt_small[, diff := NULL]

## How to avoid this?
# Make a hard-copy using the copy() function
dt_small[2:4, Number]
dt_notalias = copy(dt_small)
dt_notalias[2:4, Number := Number / 2]
dt_small[2:4, Number]

# Also inside function
calc_largest_diff = function(dt, col_name){
  dt = copy(dt)
  dt[, diff := diff(c(NA, dt[[col_name]]))]
  
  max(dt[, diff], na.rm = T)
}
calc_largest_diff(dt_small, "Number")
View(dt_small)

### Get indices using .I ----
# Sometimes you want to extract indices from a data.table. You
# might be able to use the base R which() function, but data.table
# also provides .I, which is especially helpful when grouping. 

dt_small[, .I[Letter == "B"]]

# Get the index in dt_lake for every maximum temperature per year
dt_lake[, .I[temp == max(temp)], by = year(date)]

### Binding data.tables together ----
# data.table provides a very intuitive way of binding data.tables
# (or data.frames) together, which takes multiple data.tables at 
# once and allows filling of columns that don't exist in some data.tables.

dt1 = copy(dt_small)
dt2 = copy(dt1)
dt2[, Rank := 17:32]
dt3 = copy(dt1)
dt3[, Rank := 33:48]
dt3[, new_column := "Z"]

dt_all = rbindlist(list(dt1, dt2, dt3), fill = T)

### Merging data.tables ----
# Same name as base R merge, but optimised

dt4 = copy(dt1)
dt4[, Number := rnorm(16)]
dt_merged = merge(dt1, dt4, by = "Rank", suffixes = c("_X", "_Y"))
# Using the arguments all.x, all.y, and all, you can do left, right,
# inner and outer joins, and more complex joins.

# data.table also offers a different syntax for this:
dt_merged = dt1[dt4, on = "Rank" , nomatch = 0] # Inner
dt_merged = dt4[dt1, on = "Rank"] # Left
# However, I tend to prefer the merge function.
# See for more info:
# https://rstudio-pubs-static.s3.amazonaws.com/52230_5ae0d25125b544caab32f75f0360e775.html

### Using variable names instead of column names ----
# Sometimes you might want to use variable names to refer
# to columns. 

# This gives an error:
mycol = "Letter"
dt_small[, mycol]

# You need to do this
dt_small[, ..mycol]
# The reasoning behind this is that ".." in command prompt moves
# you up an environment, so instead of looking in the data.table
# environment, it checks your R environment. 

# Alternative:
dt_small[, mycol, with = FALSE]

# Multiple columns are also possible
mycols = c("wtr_0.5", "wtr_5")
dt_lake_wide[, ..mycols]

## Define new columns
# Confusingly, if you want to define a new column, this doesn't work
newcol = "New_column"

# This doesn't do what we want
dt_small[, ..newcol := "Z"]
# Names the new column "..newcol"
dt_small[, ..newcol := NULL]

# Instead, you need to put the variable between ()
dt_small[, (newcol) := "Z"]

### Some things we won't explore in this walkthrough ----

## Keys
# Pre-sort one or multiple column to speed up repeated searches
# or joins. 
setkey(dt_small, Letter)

# Can result in significant speed-ups if using repeated operations
# on large datasets, but more of an advanced feature (I never had to use it). 
# "use keys if you're really serious about performance" - Grant McDermott, Univ. of Oregon

## Chaining
# Sort of data.table's alternative to pipes (although you can still use
# pipes with data.tables)
# Pass the result of one data.table operation into the next

dt_small[, Number := Number * 2
         ][, Number := round(Number, 2)]

# Personally, I feel this goes at the expense of readibility
# while the updating by reference makes the speed increase due 
# to this notation minimal. Sometimes handy. 

## Fast alternatives for base R operations
# fifelse -> ifelse
# fcase -> nested fifelse, similar to dplyr::case_when
# like (or %like%) -> similar to grepl, but faster in i
# fintersect -> similar to intersect
# And many more...

a = 1:10
fifelse(a%%2 == 0, "Even", "Odd")

# Also more advanced features; while they can result in speeding
# up your code, it is less likely that any bottlenecks arise from
# such functions than from reading, merging, grouping, etc. 
# Still, worthwhile to remember if you have many repeated operations

##### Limnology application - combination with rLakeAnalyzer #####

# Read hypsograph
df_hyps = fread("./data/hypsograph_erken.dat")
df_hyps[, depths := abs(depths)]

# Use long-format and grouping instead of ts. functions (faster)
# Calculate Schmidt stability (from long format)
dt_schmidt = dt_lake[, schmidt.stability(temp, depth,
                                         df_hyps$areas, df_hyps$depths),
                     by = date]
setnames(dt_schmidt, c("date", "schmidt"))

# And let's plot the result (data.table works without issue with ggplot)
# (only using from 1999 onwards, due to frequent data gaps)
ggplot(dt_schmidt[date >= as.POSIXct("1999-01-01")])+
  geom_line(aes(date, schmidt))+
  labs(y = "Schmidt stability (J/m2)")+
  theme_light()


##### End of walkthrough part #####
# (Exercises at the end of the script)


##### Further reading #####
### Where can you find more information about data.table?
# https://rdatatable.gitlab.io/data.table/
# (Hint: check out the vignettes, they are really good!)

### Want to know how fast data.table is?
# https://h2oai.github.io/db-benchmark/

### Do you like data.table's speed, but want tidyverse syntax?
# The dtplyr package translates dplyr code into data.table
# Not quite as fast, but comes a long way!
# https://cran.r-project.org/web/packages/dtplyr/index.html

### You want even more speed!!!
# Some packages provide fast alternatives for certain operations
# Convert to POSIXct (1970-2100): fasttime 
# (repeated) matching: fastmatch
# (and there are probably more)

### Your scripts are fast now thanks to data.table, but storage is a problem!
# The arrow package allows storing tables in feather or parquet format. 
# Feather can be read in Python or R, and Parquet by a large variety
# of software. File size reduction of e.g. factor 5 are possible.
# https://arrow.apache.org/docs/r/

### You mainly work in Python
# In 2017, a Python-alternative for R data.table was created
# https://datatable.readthedocs.io/en/latest/index.html


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


### Exercise 2 - Using .SD: -----
# Calculate the mean of the mpg, cyl, and disp columns in dt_cars,
# using .SD, but only for Mercedes cars ("Merc")


### Exercise 3: Grouping -----
# In dt_cars, the "am" column stands for transmission (0 = automatic,
# 1 = manual). Change the "am" column from 0/1 to "automatic"/"manual",
# and then calculate the average horsepower ("hp" column) for each group


### Exercise 4: Pivoting -----
# First remove the "am" column used in the previous exercise

# Put dt_cars in long format, so that there are only three
# columns; name, quality, and value (the values of "quality" being
# "mpg", "cyl", "disp", etc.). Name this data.table "dt_cars_long"
# Sort dt_cars_long by "name"

# You might get a warning message that not all columns are the same type
# (because some are integers and others are "double"), but you may ignore that


### Exercise 5: Calculate onset of stratification Part 1 -----
# We'll park our cars and from now on we'll work with the Lake Erken
# water temperature dataset:
dt_lake = fread("./data/MorasEtAl_ErkenObsWTemp_1961-2017.obs")
setnames(dt_lake, c("date", "depth", "temp")) 
dt_lake[, depth := abs(depth)]

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


### Exercise 6: Calculate onset of stratification Part 2 -----
# Using the datatable generated in exercise 5, make a new column named 
# "stratified" with TRUE if it the lake is stratified, and FALSE if it is not.
# Then, determine the first day of each year that the lake is stratified.
# (hint: use the first() function provided by the data.table package,
#  and the "by" argument of data.table)
# Name this resulting table "dt_result", which has columns "year" and "onset"


### Exercise 7: Calculate onset of stratification Part 3 -----
# This looks good, but we're not there yet. Some years are missing from
# the dt_lake dataset, and sometimes the first measurement of the year 
# was already stratified, so onset of stratification can't be determined.

# 1) Add the missing years to dt_result
# (hint: dt_lake ranges from 1961 to 2017. You can use the merge function, and
# one of the "all*" arguments)


# 2) Set "onset" to NA if the previous measurement is longer ago than
#    7 days
#    (hint: use .I to get the row numbers of the onset in dt_strat, and check
#    the row number minus 1. You can use the lubridate package to help 
#    with calculations involving POSIXct)


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
# numbers in a string; see what happens rle(c(1, 1, 1, 2, 2, 3, 1))
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


# 2) Using this information, find the date of onset of
#    stratification in dt_strat (hint: have a look at dt_strat_rle to
#    understand how to find the right index)


# 3) Plot the results (only the time period you feel is relevant)
#    (hint: you can use lubridate's yday() function to convert dates
#    to day-of-the-year)


### End of exercises -----
