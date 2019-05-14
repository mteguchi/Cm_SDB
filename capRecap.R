# capRecap1.r
# This program converts SDBaycaptureRecapture.txt
# file into zero-one sighting history file

# Tomo Eguchi
# 18 June 2004

rm(list=ls())

# a function to convert yyyy-mm-dd to yyyy.ddd, where ddd is the day of the year
dayYear <- function(date){
    # numbers of days in months.
    days1 <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    days2 <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    day1 <- as.Date('1899-12-30')
    day1 + juldate
}


# this file is a rectangular with NA's in blanks.
dat<-read.delim('C:/TomosFolders/turtles/greens/SDBayCaptureRecapture2.txt', header=FALSE)

outFile <- 'C:/TomosFolders/turtles/greens/zeroOneSDBay.txt'

sze <- dim(dat)

dat2 <- matrix(nrow = sze[1], ncol = sze[2])

# convert all strings into dates
r <- 1
while (r <= sze[1]){
    c <- 1
    while (c <= sze[2]){
        if (c == 1){
            dat2[r,c] <- as.character(dat[r,c])
        } else {
            dat2[r,c] <- as.character(as.Date(as.character(dat[r,c]),
                                                format = "%m/%d/%Y"))
        }
        c <- c + 1
    }
    r <- r + 1
}

IDs <- as.vector(dat2[,1])
dates <- dat2[,2:sze[2]]
non.NA <- sort(dates[!is.na(dates)])

# find unique dates:
uniq.dates <- non.NA[!duplicated(non.NA)]
uniq.dates2 <- vector(length = length(uniq.dates), mode = 'numeric')
out.Matrix <- matrix(data = 0, nrow = sze[1]+1, ncol = length(uniq.dates)+1)

c <- 1
while (c <= length(uniq.dates)){
    t.date <- unclass(as.POSIXlt(as.Date(uniq.dates[c])))
    yr0 <- t.date$year
    if (yr0 >= 100){
        yr <- yr0 - 100 + 2000
    } else {
        yr <- yr0 + 1900
    }

    doy <- t.date$yday
    uniq.dates2[c] <- yr + doy/1000
    c <- c + 1
}

IDs2 <- seq(1, length(IDs), by = 1)
#out.Matrix[1, ] <- c('NA', uniq.dates)
out.Matrix[1, ] <- c(0.0, uniq.dates2)
out.Matrix[, 1] <- c(0.0, IDs2)

d <- 1
while (d <= length(uniq.dates)){
    r <- 1
    date0 <- uniq.dates[d]

    while (r <= sze[1]){
        if (date0 %in% dat2[r,]){
            out.Matrix[(r+1), (d+1)] <- 1
        }
        r <- r + 1
    }
    d <- d + 1
}

r <- 1
sze <- dim(out.Matrix)
while (r <= sze[1]){
    write(out.Matrix[r,], file = outFile, ncolumn = sze[2], append = TRUE)
    r <- r + 1
}
