library(tidyverse)

#### 1. GDP GROWTH #### 
# loading the dataset downloaded from https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG
df1 <- read.csv('b1.gdp_growth.csv')

# gathering data
df1 <- df1  %>% gather(year, gdp_growth, X1960:X2019)

# cleaning year column
df1$year <- sub(".", "", df1$year)

# changing year column into interger
df1$year <- as.integer(df1$year)

# subsetting complete observations
df1 <- na.omit(df1)

# renaming columns
names(df1)[names(df1) == 'Country.Name'] <- 'country_name'
names(df1)[names(df1) == 'Country.Code'] <- 'country_code'
names(df1)[names(df1) == 'Indicator.Name'] <- 'indicator_name'

# exporting csv file
write.csv(df1, 'gdp_growth.csv', row.names=FALSE)

# create a subset world
world <- df1[df1$country_name == 'World', ]
View(world)
write.csv(world, 'world_gdp_growth.csv', row.names = F)

# create a subset with different income groups
income <- df1[df1$country_name == c('World', 'United States', 'High income', 'Low income', 'Lower middle income', 'Upper middle income'), ]
View(income)

# create a subset US
us <- df1[df1$country_name == 'United States', ]
View(us)
write.csv(us, 'us_gdp_growth.csv', row.names = F)

# merge the world and US
us_world <- subset(df1, country_name == 'United States' | country_name == 'World')

# export file us's gdp growth vs world's gdp growth
write.csv(us_world, 'us_world_gdp_growth.csv', row.names = F)

#### 2. gdp in dollars ####
# getting the dataset from https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
# load the data set 
df2 <- read.csv('b2.gdp_in_dollars.csv')
df2 <- df2  %>% gather(year, gdp, X1960:X2019)

# change the year
df2$year <- sub(".", "", df2$year)
df2$year <- as.integer(df2$year)

# drop columns
df2 <- subset(df2, select = -c(Indicator.Code))

# change the columns' names
names(df2)[names(df2) == 'Country.Name'] <- 'country_name'
names(df2)[names(df2) == 'Country.Code'] <- 'country_code'
names(df2)[names(df2) == 'Indicator.Name'] <- 'indicator_name'

# choose US & World 
df2 <- subset(df2, country_name == 'United States' | country_name == 'World')
df2 <- subset(df2, select = -c(country_code))

# change World to Others
df2$country_name <- as.character(df2$country_name)
df2[df2$country_name == 'World',]$country_name <- 'Others'

# change values for Others
for(i in 1960:2019){
  df2[df2$country_name == 'Others' & df2$year == i,]$gdp <- 
    df2[df2$country_name == 'Others' & df2$year == i,]$gdp - df2[df2$country_name == 'United States' & df2$year == i,]$gdp
}

# clean the dataframe in the final round
df2 <- df2[df2$year >= 1976, ]

# export 
write.csv(df2, 'us_vs_others_gdp.csv', row.names = F)


#### 3. Income classification ####
library(tidyverse)
library(readxl)
library(stringr)
# the data getting from this website https://datatopics.worldbank.org/world-development-indicators/the-world-by-income-and-region.html
# it is not directly downloaded
# some manipulations for the Tableau in the website so that download is possible
df3 <- read_excel('The_World_by_Income_data.xls')
View(df3)

# Remove the "Fiscal Year" charater 
df3$Year <- str_remove_all(df3$Year, "[FiscalYear ]")

# Convert Year to numeric
df3$Year <- as.numeric(df3$Year)

# Correct fiscal year to the year of getting the data
df3$Year <- df3$Year - 2

# change the columns' names df3
names(df3)[names(df3) == 'Country Code'] <- 'country_code'
names(df3)[names(df3) == 'Economy'] <- 'country_name'
names(df3)[names(df3) == 'Income Group'] <- 'income_group'
names(df3)[names(df3) == 'Year'] <- 'year'
str(df3)

# load the data set to get the gdp
df4 <- read.csv('b2.gdp_in_dollars.csv')
df4 <- df4  %>% gather(year, gdp, X1960:X2019)

# change the year
df4$year <- sub(".", "", df4$year)
df4$year <- as.integer(df4$year)

# drop column in df4
df4 <- subset(df4, select = -c(Indicator.Code))

# change the columns' names
names(df4)[names(df4) == 'Country.Name'] <- 'country_name'
names(df4)[names(df4) == 'Country.Code'] <- 'country_code'
names(df4)[names(df4) == 'Indicator.Name'] <- 'indicator_name'
str(df4)

# characterise the country_code column in df4 
df4$country_code <- as.character(df4$country_code)

# join df3 vs df4
df5 <- left_join(df3, df4, by = c('year', 'country_code'))
View(df5)

# drop column in df5
df5 <- subset(df5, select = -c(country_name.y))

# change the columns' names df5
names(df5)[names(df5) == 'country_name.x'] <- 'country_name'

# correct income_group column in df5
df5[df5$income_group == 'NA', ]$income_group <- NA

# drop all rows having missing values in df5
df5 <- na.omit(df5)

# export csv file df5
write.csv(df5, 'income_groups_by_years.csv', row.names = F)


