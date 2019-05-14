#captureEffort


# extracts capture effort data from the database

rm(list=ls())
library(RODBC)
library(tidyverse)
ifelse(Sys.info()[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))

turtle <- odbcConnect(dsn = 'Turtle', uid = '', pwd = '')
turtle.tbls <- sqlTables(turtle)

# TO FIND ALL TABLE NAMES:
# turtle.tbls.names <- turtle.tbls$TABLE_NAME[grep(pattern = 'tbl_',
#                                                  turtle.tbls$TABLE_NAME)]
#
# turtle.vw.names <- turtle.tbls$TABLE_NAME[grep(pattern = 'vw_',
#                                                turtle.tbls$TABLE_NAME)]

# get SD Bay results:
turtle.SDB <- sqlQuery(turtle, 'select * from tbl_SD_Bay') %>%
  select(., NMFS_Tag, Turtle_ID, Year_caught,
         Month_caught, Day_caught, Caught_Dead,
         PIT_Tag_LFF, PIT_Tag_RFF, Inconel_Tag_LFF,
         Inconel_Tag_RFF, Sex, Weight_kg,
         Str_Carapace_Length_cm, Str_Carapace_Width_cm,
         Cur_Carapace_Length_cm, Cur_Carapace_Width_cm,
         Sat_Tag_Type, Sat_Tag_ID, Satellite_Tag_Added) %>%
  filter(., !is.na(NMFS_Tag)) %>%
  mutate(., Capture_Date = as.Date(paste0(Year_caught, '-',
                                          Month_caught, '-',
                                          Day_caught),
                                   format = '%Y-%m-%d'))

odbcClose(turtle)

# number of days with turtles:
n.days <- group_by(turtle.SDB, Capture_Date) %>%
  summarise(., n = n())


# Use effort data from Katie's version - data have been checked with
# datasheets - added Na1 and NA2 as two missing headings on 11/13/2017
col_def.effort <- cols(ID = col_integer(),
                      Field_Date = col_date(format = '%m/%d/%Y'),
                      Net_Color = col_character(),
                      Net_Deployment_Time = col_time(format = '%H:%M:%S'),
                      Net_Retrieval_Time = col_time(format = '%H:%M:%S'),
                      Net_Deployment_Location = col_character(),
                      GPS_N_Center_Nets = col_double(),
                      GPS_W_Center_Nets = col_double(),
                      Net_Lat_End1 = col_double(),
                      Net_Lon_End1 = col_double(),
                      Net_Lat_End2 = col_double(),
                      Net_Lon_End2 = col_double(),
                      High_Tide = col_character(),
                      Low_Tide = col_character(),
                      Net_Location_Notes = col_character(),
                      NA1 = col_character(),
                      NA2 = col_character(),
                      Comment = col_character())

turtle.effort <- read_csv(paste0(dirSelector()$Rdir,
                                 '/Cm_SDB_tides/data/Standardized Net data.csv'),
                          col_types = col_def.effort) %>%
  select(., Field_Date, Net_Deployment_Time, Net_Retrieval_Time,
         Net_Deployment_Location, GPS_N_Center_Nets, GPS_W_Center_Nets,
         Net_Lat_End1, Net_Lon_End1, Net_Lat_End2, Net_Lon_End2)

# to see all columns using head, do head(data.frame(turtle.effort))


