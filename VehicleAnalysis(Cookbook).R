# install needed packages
install.packages('plyr')
install.packages('ggplot2')
install.packages('reshape2')
install.packages('Rcpp')

# load libraries
library(plyr)
library(ggplot2)
library(reshape2)
library(Rcpp)

# check wd
getwd()

# load csv into R w/o converting strings into factors
vehicles <- read.csv('vehicles.csv', stringsAsFactors = FALSE)
head(vehicles)

# load labels for the dataset
labels <- read.table('varlabels.txt', sep='-', header=FALSE)

# Error on line 11 due to two '-' on this line
# need to ignore hyphenated words
labels <- do.call(rbind, strsplit(readLines('varlabels.txt'), ' - '))
head(labels)

# get the number of records in the dataset
nrow(vehicles)

# get the number of features in the dataset
ncol(vehicles)

# explore feature names
names(vehicles)

# get the number of years this dataset covers
length(unique(vehicles[,'year']))

# get the min year & max year
min_year <- min(vehicles[,'year'])
min_year

max_year <- max(vehicles[,'year'])
max_year

# get unique primary fuel types and count
table(vehicles$fuelType1)

# get transmission types
table(vehicles$trany)

# set all missing values for transmission to NA
vehicles$trany[vehicles$trany == ''] <- NA

# standardize values for transmission types
# set any automatic to 'auto' and everything else to manual
vehicles$trany2 <- ifelse(substr(vehicles$trany, 1, 4) == 'Auto', 'Auto', 'Manual')

# convert the new transmission column to a factor
# get counts of each factor
vehicles$trany2 <- as.factor(vehicles$trany2)
table(vehicles$trany2)

## visualizing the dataset

# use split-apply-combine to analyze mpg by year
mpg_by_year <- ddply(vehicles, ~year, summarize, avgMPG = mean(comb08),
                     avgHwy = mean(highway08), avgCity = mean(city08))
head(mpg_by_year)

# create a scatterplot to view avgMPG change over time
# avgMPG on the y-axis & year on the x-axis
ggplot(mpg_by_year, aes(year, avgMPG)) + geom_point() +
         geom_smooth() + xlab('Year') + ylab('Average MPG') +
         ggtitle('All cars') + theme(plot.title = element_text(hjust = 0.5))

# the introduction of electric & hybrid cars can skew this data
# limiting the analysis to only gas powered cars
# get all unique values of fuelType1
table(vehicles$fuelType1)

# create a subset using only gasoline fuelType1 values
# exclude hybrids by limiting field atvtype
gas_cars <- subset(vehicles, fuelType1 %in% c('Midgrade Gasoline',
                    'Premium Gasoline', 'Regular Gasoline')
                   & fuelType2 == '' & atvType != 'Hybrid')

# split-apply-combine the gas_cars df to summarize by mpg
gas_mpg <- ddply(gas_cars, ~year, summarize, avgMPG = mean(comb08))
head(gas_mpg)

# plot MPG v year for gas cars
ggplot(gas_mpg, aes(year, avgMPG)) + geom_point() + geom_smooth() +
  xlab('Year') + ylab('Average MPG') + ggtitle('Gasoline cars') +
  theme(plot.title = element_text(hjust = 0.5))

# do larger engines get worse gas mileage?
# displ represents displacement of engine in liters
typeof(gas_cars$displ)

# plot displ v avgMPG
ggplot(gas_cars, aes(displ, comb08)) + geom_point() +
        geom_smooth() + xlab('Displacement') +
        ylab('MPG') + ggtitle('Displacement v MPG (Gasoline Cars)') +
        theme(plot.title = element_text(hjust = 0.5))

# has the number of smaller cars increased in recent years?
# this could explain the uptick in avgMPG
avg_car_size <- ddply(gas_cars, ~year, summarize,
                      avgDispl = mean(displ))

# plot avgDispl v Year
ggplot(avg_car_size, aes(year, avgDispl)) + geom_point() +
        geom_smooth() + xlab('Year') + ylab('AVG Displacement') +
        ggtitle('Average Car Size') + theme(plot.title = element_text(hjust = 0.5))


# create and plot a dataset to compare avgMPG & avgDispl
mpg_displ <- ddply(gas_cars, ~year, summarize,
                           avgMPG = mean(comb08),
                           avgDispl = mean(displ))

# use melt to change the df to long format
mpg_displ_melt <- melt(mpg_displ, 'year')

# set the levels of the long df
levels(mpg_displ_melt$variable) <- c('Average MPG',
                                  'Average Engine Displacement')
head(mpg_displ_melt)

# create split plots for each value in the long format df
ggplot(mpg_displ_melt, aes(year, value)) + geom_point() +
  geom_smooth() + facet_wrap(~variable, ncol = 1,
                             scales = 'free_y') +
  xlab('Year') + ylab('')

# engines have trended smaller over time
# are manual or auto engines more efficient for 4 cylinder cars
gas_cars4 <- subset(gas_cars, cylinders == '4')

# create boxplots to compare mpg by year for each type of transmission
ggplot(gas_cars4, aes(factor(year), comb08)) + geom_boxplot() +
  facet_wrap(~trany2, ncol = 1) + theme(axis.text.x = 
                                          element_text(angle = 45)) +
  labs(x = 'Year', y = 'MPG')

# change in proportion of of manual cars each year
ggplot(gas_cars, aes(factor(year), fill = factor(trany2))) +
  geom_bar(position = 'fill') + labs(x = 'Year', y = 'Proportion of Cars',
                                     fill = 'Transmission') +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_hline(yintercept = 0.5, linetype = 2)


# explore the number of different car makes available
# each year
cars_make <- ddply(gas_cars, ~year, summarize, make_count = length(unique(make)))

ggplot(cars_make, aes(year, make_count)) + geom_point() +
  geom_smooth() + labs(x = 'Year', y = 'Different Makes Available') +
  ggtitle('Four Cylinder Cars') + theme(plot.title = element_text(hjust = 0.5))


# how many manufacturers have cars included in each year?
uniq_makes <- dlply(gas_cars4, ~year, function(x)
  unique(x$make))

common_makes <- Reduce(intersect, uniq_makes)
common_makes

# how has fuel efficiency changed by make?
# create a subset of the gas_cars data for manufacturers
# identified in the common_makes data
common_makes4 <- subset(gas_cars4, make %in% common_makes)

# find avgMPG by make per year
avg_mpg_common_makes4 <- ddply(common_makes4, ~year + make, summarize,
                               avgMPG = mean(comb08))

ggplot(avg_mpg_common_makes4, aes(year, avgMPG)) + geom_line() +
  facet_wrap(~make, nrow = 3)