# SDbayTurtle.R

# Tomo Eguchi
# 30 April 2004

rm(list=ls())

all.dat<-read.table('C:/TomosFolders/turtles/greens/SDBayAbundance/SDBayAll.txt',
    header = TRUE,
    sep = "\t",
    fill = TRUE,
    na.strings = 'NA')

# ID, Date, Sex, Mass, SCL, SCW, CCL, CCW, BD

