# Initial analysis for pandemic influenza data
rm(list=ls())
if(grepl('spencerfox', Sys.info()['login'])) setwd('~/projects/flu/pandemic_dynamics/code')
if(grepl('vagrant', Sys.info()['user'])) setwd('/vagrant/flu/pandemic_dynamics/code')
if(grepl('sjf826', Sys.info()['login'])) setwd('/home1/02958/sjf826/pandemic_dynamics/code')
if(grepl('tacc', Sys.info()['nodename'])) setwd('/home1/02958/sjf826/pandemic_dynamics/code')
if(grepl('aksharaanand', Sys.info()['login'])) setwd('~/Desktop/epidemiology/pandemic_dynamics/code')



pdmData <- read.csv(file = "../data/Deaths by City Level.csv", na.strings = "-")

plot(pdmData$MONTGOMERY..AL, ylim=c(0,40))
mean(pdmData$MOBILE..AL, na.rm=T)

