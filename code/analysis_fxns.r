## Read in the data
pdmData <- read.csv(file = "../data/Deaths by City Level.csv", na.strings = "-")


## Functions for working with data

season_of = function(data){ 
  ## Returns flu season year
  ifelse(data$WEEK >=40, data$YEAR, ifelse(data$WEEK<=25, data$YEAR-1, NA))
}



## Data Cleaning 
doubleDates <- rle(pdmData$WEEK)
which(doubleDates$lengths>=2)
indexToRemove <- c(sum(doubleDates$lengths[1:53]), 
                   sum(doubleDates$lengths[1:251]), 
                   sum(doubleDates$lengths[1:322]),
                   sum(doubleDates$lengths[1:434]),
                   sum(doubleDates$lengths[1:449]),
                   sum(doubleDates$lengths[1:591]))
df <- pdmData[-indexToRemove, ]
doubleDates <- rle(df$WEEK)
which(doubleDates$lengths==2)
df <- df[-sum(doubleDates$lengths[1:322]),]
pdmData <- df
rm(df)

## Start date code
originDate <- "1965-1-8"
pdmData$cumWeek <- seq_along(pdmData$WEEK)
pdmData$date <- as.Date((pdmData$cumWeek-1)*7, origin = originDate, na.rm=T)

## Gives us the flu year for each date
pdmData$fluYear <- season_of(pdmData)
pdmData <- pdmData[which(!is.na(pdmData$fluYear)),]
pdmData <- pdmData[which(pdmData$fluYear>1964),]

datesTable <- data.frame(date = pdmData$date,
                         cdcWeek = pdmData$WEEK,
                         cdcYear = pdmData$YEAR,
                         fluYear = pdmData$fluYear,
                         rDate = as.numeric(pdmData$date)+1819)

start_weeks <- datesTable$rDate[datesTable$cdcWeek==40]
shifted_start_weeks = c(start_weeks[1], start_weeks[-length(start_weeks)])

# Converts date(s) (r format) into number of weeks from the start of the current season
num_to_linear_week = function(date){

  #Temporarily get rid of NA weeks for now
  date_num <- datesTable$rDate[match(date, datesTable$date)]
  
  #Looks for the closest of the start_weeks to each date in R format
  ind_closest = sapply(date_num, function(x) which.min(abs(x - start_weeks)))
  
  #if the rdate is larger than the closest start week (then it is in that start weeks season
  #This means you can simply subtract the corresponding start week, divide by zero and add 1
  dist = ifelse(date_num >= start_weeks[ind_closest], 
                ((date_num - start_weeks[ind_closest]) / 7 + 1), 
                ((date_num - shifted_start_weeks[ind_closest]) / 7 + 1) )
  
  #Replace the NAs from the beginning in the correct locations
  return(dist)
}


findPeak <- function(data) {
  ## function takes in single season and signle state data
  ## returns the week of the flu
  data
  peak_intensity <- max(data$death, na.rm=T)
  peak_week <- data$fluWeek[which.max(data$death)]
  return(data.frame(magnitude=peak_intensity, peakWeek=peak_week))
}