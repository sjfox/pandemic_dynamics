#Converting wide-format data to long-format 
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
library(tidyr)

source("analysis_fxns.r")

## Combine cities into states
temp2 <- melt(pdmData, id.vars = c("date", "fluYear"))
temp2$state <- sapply(strsplit(as.character(temp2$variable), split = "..", fixed=T), "[", 2)
temp2$variable <-NULL

## Add up all of the weekly deaths by city into the individual state --> need to edit
stateData <- ddply(temp2, .variables = .(date, state), 
                   .fun =  function(x) data.frame(deaths=sum(x$value, na.rm=T), fluYear=x$fluYear[1]))

#convert data to 4 columns
year <- "year"
week <- "week"
state <- "state"
deaths <- "stateData"
data_long <- gather_(pdmData, year, week, state, deaths, factor_key =TRUE)
