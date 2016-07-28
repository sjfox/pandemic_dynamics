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

source("analysis_fxns.r")


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

## Selecting the pandemic and seasonal data separately
seasonalPeaks <- peakData[-which(peakData$fluYear %in% c(1968, 1977,2009)), ]
pdmPeaks <- peakData[which(peakData$fluYear %in% c(1968, 1977,2009)), ]

##Creating a rank box plot
library(dplyr)
library(tidyr)
pdmRanks <- pdmPeaks %>%
              group_by(fluYear) %>%
              mutate(ranked_states=rank(peakWeek, ties.method="average"))

seasonalPeaks %>%
  group_by(fluYear) %>%
  mutate(ranked_states=rank(peakWeek, ties.method="average")) %>%
  ggplot(aes(x=state, y=ranked_states)) +geom_boxplot() +
    geom_point(data=pdmRanks, aes(x=state, y = ranked_states, color=as.factor(fluYear)))


##Creating a median seasonal graph
## Only Alabama data
seasonalPeaks %>%
  filter(state=="AL") %>%
  summarise(median(peakWeek))

## All states  
median_state_seasonal <- seasonalPeaks %>%
                            group_by(state) %>%
                            summarise(med_pk_week=median(peakWeek)) 
              


pandemic_1968_peak_week <- pdmPeaks %>%
                            filter(fluYear== 1968) 

## Creating columns for each pandemic year data
sixtyeight <- pdmPeaks %>%
                filter(fluYear== 1968) %>%
                select(peakWeek)

seventyseven <- pdmPeaks %>%
                  filter(fluYear== 1977) %>%
                  select(peakWeek)


twothousandnine <- pdmPeaks %>%
                      filter(fluYear== 2009) %>%
                      select(peakWeek)
median_state_seasonal$sixty <- sixtyeight$peakWeek
median_state_seasonal$seventy <- seventyseven$peakWeek
median_state_seasonal$onine <- twothousandnine$peakWeek

median_state_seasonal_long <- median_state_seasonal %>%
                                gather(key = pandemic_year, value=pandemic_peak, sixty, seventy, onine)

#Run to plot all three MEDIAN pandemic years together
ggplot(median_state_seasonal_long, aes(x=med_pk_week, y=pandemic_peak, color=pandemic_year)) + 
  geom_point() + theme_bw() +  theme(axis.line.x = element_line(color="black", size = 2),
                                     axis.line.y = element_line(color="black", size = 2))

#Run to plot only MEDIAN 1968 
plot(median_state_seasonal$med_pk_week, pandemic_1968_peak_week$peakWeek, main = "Figure 1", xlab = "Median Seasonal Peak", ylab = "Pandemic Peak Week", pch = 19)


## Plotting the facet grid of all peak week densties
ggplot(data=seasonalPeaks, aes(peakWeek)) + geom_density() + facet_wrap(~state) +
  geom_vline(data=pdmPeaks, aes(xintercept= peakWeek, color = as.factor(fluYear))) 

#Creating a mean seasonal graph
mean_state_seasonal <- seasonalPeaks %>%
  group_by(state) %>%
  summarise(mean_pk_week=mean(peakWeek))


pandemic_1968_peak_week <- pdmPeaks %>%
  filter(fluYear== 1968) 


## Creating columns for each pandemic year data
sixeight <- pdmPeaks %>%
  filter(fluYear== 1968) %>%
  select(peakWeek)

seven <- pdmPeaks %>%
  filter(fluYear== 1977) %>%
  select(peakWeek)

nine <- pdmPeaks %>%
  filter(fluYear== 2009) %>%
  select(peakWeek)
mean_state_seasonal$sixty <- sixtyeight$peakWeek
mean_state_seasonal$seventy <- seventyseven$peakWeek
mean_state_seasonal$onine <- twothousandnine$peakWeek

mean_state_seasonal_long <- mean_state_seasonal %>%
  gather(key = pandemic_year, value=pandemic_peak, sixty, seventy, onine)

#Run to plot all three MEAN pandemic years together
ggplot(mean_state_seasonal_long, aes(x=mean_pk_week, y=pandemic_peak, color=pandemic_year)) + 
  geom_point() + theme_bw() +  theme(axis.line.x = element_line(color="black", size = 2),
                                     axis.line.y = element_line(color="black", size = 2))

## run from here to get map
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


