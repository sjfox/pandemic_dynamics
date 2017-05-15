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
library(tidyr)

source("analysis_fxns.r")

#combine cities into states
year <- "year"
week <- "week"
state <- "state"
deaths <- "stateData"
##temp2 <- gather_(pdmData, year, week, state, deaths, factor_key = year)
##temp2 <- gather_(pdmData, "state", "year", 2:3, factor_key = city)
gather(pdmData, city, deaths, 3:123 , na.rm = FALSE, convert = FALSE, factor_key = FALSE)

