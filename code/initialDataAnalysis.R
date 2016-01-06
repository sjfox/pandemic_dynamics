# Initial analysis for pandemic influenza data
rm(list=ls())
if(grepl('spencerfox', Sys.info()['login'])) setwd('~/projects/flu/pandemic_dynamics/code')
if(grepl('vagrant', Sys.info()['user'])) setwd('/vagrant/flu/pandemic_dynamics/code')
if(grepl('sjf826', Sys.info()['login'])) setwd('/home1/02958/sjf826/pandemic_dynamics/code')
if(grepl('tacc', Sys.info()['nodename'])) setwd('/home1/02958/sjf826/pandemic_dynamics/code')
if(grepl('aksharaanand', Sys.info()['login'])) setwd('~/pandemic_dynamics/code')

## Load up the R packages that will be used in the code
library(ggplot2)
library(reshape2)
library(plyr)

## Read in the data
pdmData <- read.csv(file = "../data/Deaths by City Level.csv", na.strings = "-")

## Converts the week/year format to a date format
## This will need to be updated to be more accurate in future
pdmData$date <- as.Date(paste0(pdmData$YEAR," ", pdmData$WEEK, " 1"), format = "%Y %U %u")

## Select only the years surrounding the pandemic 1968
sxtyDat <- pdmData[pdmData$YEAR %in% c(1967,1968,1969),]

## Now aggregate the data by state, to reduce the noisiness of the city data
temp2 <- melt(sxtyDat, id.vars = c("WEEK", "YEAR", "date"))
temp2$state <- sapply(strsplit(as.character(temp2$variable), split = "..", fixed=T), "[", 2)
temp2$WEEK <- temp2$YEAR <- temp2$variable <-NULL

## Add up all of the weekly deaths by city into the individual state
stateData <- ddply(temp2, .variables = .(date, state), .fun =  function(x) sum(x$value, na.rm=T))
## removes temporary data frame
rm(temp2)

## Add up the state data into national data for plotting
natData <- ddply(stateData, .variables = .(date), .fun =  function(x) sum(x$V1, na.rm=T))
ggplot(natData, aes(date, V1))+ geom_line()+theme_bw()

## Plot the state-wide data
ggplot(stateData, aes(date, V1, color=state)) + geom_line() + theme_bw()


