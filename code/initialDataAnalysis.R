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
library(cowplot)

source("../../../flu/pandemic_dynamics/code/analysis_fxns.r")


## Combine cities into states
temp2 <- melt(pdmData, id.vars = c("date", "fluYear"))
temp2$state <- sapply(strsplit(as.character(temp2$variable), split = "..", fixed=T), "[", 2)
temp2$variable <-NULL

## Add up all of the weekly deaths by city into the individual state
stateData <- ddply(temp2, .variables = .(date, state), 
                   .fun =  function(x) data.frame(deaths=sum(x$value, na.rm=T), fluYear=x$fluYear[1]))
## removes temporary data frame
rm(temp2)

stateData$fluWeek <- num_to_linear_week(stateData$date)

peakData <- ddply(stateData, .variables = .(state, fluYear), .fun =  findPeak)


## Looking at inidividual histogram for IL
hist(peakData[which(peakData$state=="IL"), "peakWeek"], breaks=15, freq = FALSE)

## Selecitng the pandemic and seasonal data separately
seasonalPeaks <- peakData[-which(peakData$fluYear %in% c(1968, 1977,2009)), ]
pdmPeaks <- peakData[which(peakData$fluYear %in% c(1968, 1977,2009)), ]

## Plotting the facet grid of all peak week densties
ggplot(data=seasonalPeaks, aes(peakWeek)) + geom_density() + facet_wrap(~state) +
  geom_vline(data=pdmPeaks, aes(xintercept= peakWeek, color = as.factor(fluYear)))



## run from here to get graph
## edit the year to see succession of years
test <- peakData[which(peakData$fluYear==2009), ]

us <- map_data("state")

test$region <- tolower(state.name[match(test$state, state.abb)])

gg <- ggplot() + geom_map(data=us, map=us,
                    aes(x=long, y=lat, map_id=region),
                    fill="gray", color="#ffffff", size=0.15)
gg <- gg + geom_map(data=test, map=us,
         aes(fill=magnitude, map_id=region),
         color="#ffffff", size=0.15)
gg <- gg + scale_fill_continuous(low='blue', high='red', 
                                 guide='colorbar')
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(axis.text = element_blank())
print(gg)

# ## Add up the state data into national data for plotting
# natData <- ddply(stateData, .variables = .(date), .fun =  function(x) sum(x$V1, na.rm=T))
# ggplot(natData, aes(date, V1)) + geom_line() + theme_bw()
# 
# ## Plot the state-wide data
ggplot(stateData, aes(date, deaths, color=state)) + geom_point() + theme_bw()


