#Converting wide-format data to long-format 
# Initial analysis for pandemic influenza data
rm(list=ls())
if(grepl('spencerfox', Sys.info()['login'])) setwd('~/projects/flu/pandemic_dynamics/code')
if(grepl('vagrant', Sys.info()['user'])) setwd('/vagrant/flu/pandemic_dynamics/code')
if(grepl('sjf826', Sys.info()['login'])) setwd('/home1/02958/sjf826/pandemic_dynamics/code')
if(grepl('tacc', Sys.info()['nodename'])) setwd('/home1/02958/sjf826/pandemic_dynamics/code')
if(grepl('aksharaanand', Sys.info()['login'])) setwd('~/pandemic_dynamics/code')

## Load up the R packages that will be used in the code
##library(ggplot2)
##library(reshape2)
##library(plyr)
##library(cowplot)
library(tidyverse)

source("analysis_fxns.r")

#combined cities into states
year <- "year"
week <- "week"
state <- "state"
deaths <- "stateData"
##temp2 <- gather_(pdmData, year, week, state, deaths, factor_key = year)
##temp2 <- gather_(pdmData, "state", "year", 2:3, factor_key = city)
pdmDataLong <- gather(pdmData, city, deaths, BIRMINGHAM..AL:MILWAUKEE..WI , na.rm = FALSE, convert = FALSE, factor_key = FALSE) %>%
separate(city, into =c("city", "state"), sep = "\\..") 
head(pdmDataLong)  

#group data and then pipe it into summarize function
pdmDataLong %>% group_by(city, fluYear) %>%
  summarise(total_deaths = sum(deaths, na.rm=T)) %>%
ggplot(aes(x = fluYear, y = total_deaths, color = city) + geom_line())
