# SDbayGrowth.R

# Tomo Eguchi.
# 5 May 2004
# 16 November 2009 - modified for new computer
#

# For GAM, use library(mgcv) or library(GAM)

rm(list=ls())
#library(mgcv)
library(gam)
library(splines)

# First dataset for fitting growth curves to paired-measurements.
all.dat<-read.table('D:/TomosFolders/publications/San Diego Bay somatic growth/Data/TurtleGrowth4R.txt',
    header = TRUE,
    sep = "\t",
    fill = TRUE,
    na.strings = 'NaN')

#all.dat should have the following columns:
# 1.ID/2.date1/3.date2/4.dDate/5.SCL1/6.SCL2/7.dSCL/8.Mass1/9.Mass2/10.dMass/11.meanLength/12.meanMass/
#13.dSCLperYr/14.dMperYr/15.Year1/16.Year/17.Sex

# Simple linear model:
lm.fit1 <- lm(dSCLperYr ~ meanLength, data = all.dat)

#von Bertalanffy (Bjorndal and Bolten 1988):
vB1.fit <- nls(dSCLperYr/meanLength ~ v1/meanLength - v2, start = list(v1 = 1.0, v2 = 1.0), data=all.dat)

#von Bertalanffy (Frazer and Ehrhart 1985)
vB2.fit <- nls(SCL2 ~ v1 - (v1 - SCL1)*exp(-v2 * (dDate/365)), start = list(v1 = 1.0, v2 = 1.0), data=all.dat)

#von Bertalanffy (Ratkowsky 1986)
n <- length(all.dat$SCL2)
L1 <- min(all.dat$SCL2)
Ln <- max(all.dat$SCL2)
#t1 <-
#m <- 1 + (n-1)*(())
#vB3.fit <- nls(SCL2 ~ v1 + (v2 - v1)*((1 - v3^(m-1))/(1-v3^(n-1))), start=list(v1=min(SCL2), v2=max(SCL2), v3=1.0), data=all.dat)

#Gompertz (Bjorndal and Bolten 1988):
g1.fit <- nls(dSCLperYr/meanLength ~ -g1 * (log(meanLength)) + g2, start = list(g1 = 1.0, g2 = 1.0), data=all.dat)

#Logistic (Frazer and Ehrhart 1985)
log2.fit <- nls(SCL2 ~ l1*SCL1/(SCL1 + (l1 - SCL1)*exp(-l2*(dDate/365))), start=list(l1=100.0, l2=0.1), data=all.dat)

#Logistic (Bjorndal and Bolten 1988):
log1.fit <- nls(dSCLperYr/meanLength ~ -l1 * meanLength + l2, start = list(l1 = 1.0, l2 = 1.0), data=all.dat)

scl <- seq(min(all.dat$meanLength, na.rm = 'TRUE'), max(all.dat$meanLength, na.rm = 'TRUE'), length = 100)
ddate<- seq(min(all.dat$dDate/365, na.rm = 'TRUE'), max(all.dat$dDate/365, na.rm = 'TRUE'), length = 100)
scl1<- seq(min(all.dat$SCL1, na.rm = 'TRUE'), max(all.dat$SCL1, na.rm = 'TRUE'), length = 100)

x11()
par(mfrow=c(1, 3))
plot(all.dat$meanLength, all.dat$dSCLperYr, xlab="Mean SCL (cm)", ylab = "Growth Rate (cm/yr)")
lines(scl, predict.lm(lm.fit1, data.frame(meanLength = scl)))

plot(all.dat$meanLength, all.dat$dSCLperYr/all.dat$meanLength, xlab="Mean SCL (cm)", ylab = "Specific Growth Rate (cm/yr)")
lines(scl, predict(vB1.fit, data.frame(meanLength = scl)), col = 'red1')

plot(all.dat$SCL2, all.dat$SCL1, xlab="SCL at time 1", ylab="SCL at time 2")
lines(scl1, predict(vB2.fit, data.frame(SCL1 = scl1)), col = 'red2')
#lines(scl1, predict(vB3.fit, data.frame(SCL1 = scl1)), col = 'red3')

lines(scl, predict(g1.fit, data.frame(meanLength = scl)), col = 'blue')
lines(scl, predict(log1.fit, data.frame(meanLength=scl)), col = 'yellow')

x11()
plot(all.dat$meanMass, all.dat$dMperYr, xlab="Mean Mass (kg)", ylab="Growth Rate (kg/yr)")

#GAM for SCL
gam.fit1a <- gam(dSCLperYr ~ s(meanLength,3) + s(dDate,3) + s(Year,3), family = "quasi", data=all.dat)
#gam.fit1a <- gam(dSCLperYr ~ Sex + s(meanLength,3) + s(dDate,3) + s(Year,3), data=all.dat)
gam.fit1b <- glm(dSCLperYr ~ meanLength + dDate + Year, data=all.dat)
x11()
par(mfrow=c(1,3))
plot.gam(gam.fit1a, se=TRUE)

x11()
par(mfrow=c(1,4))
plot(gam.fit1b)


x11()
par(mfrow=c(2,1))
plot(all.dat$meanLength, fitted(gam.fit1a), xlab="", ylab="Fitted Growth Rate (cm/yr)")
#lines(all.dat$SCL1, predict(gam.fit1a))
plot(all.dat$meanLength, all.dat$dSCLperYr, xlab = "Mean SCL (cm)", ylab = "Observed Growth Rate (cm/yr)")

# GAM for Mass
meanMass2 <- all.dat$meanMass[is.na(all.dat$meanMass)==F]
dMperYr2 <- all.dat$dMperYr[is.na(all.dat$meanMass)==F]
dDate2 <- all.dat$dDate[is.na(all.dat$meanMass)==F]
Year2 <- all.dat$Year[is.na(all.dat$meanMass)==F]

gam.fit2 <- gam(dMperYr2 ~ s(meanMass2) + s(dDate2) + s(Year2), na.action=na.omit)
x11()
par(mfrow=c(1,3))
plot.gam(gam.fit2, se=TRUE)

x11()
par(mfrow=c(2,1))
plot(meanMass2, fitted(gam.fit2), xlab="", ylab="Fitted Growth Rate (kg/yr)")
plot(meanMass2, dMperYr2, xlab = "Mean Mass (kg)", ylab = "Observed Growth Rate (kg/yr)")


# Second dataset for looking at relationships between various measurements
all.dat2 <- read.table('C:/Documents and Settings/T_E/My Documents/TomosFolders/turtles/greens/SDBay/SDBayGrowth/SDBayGrowth8.txt',
    header = TRUE,
    sep = "\t",
    fill = TRUE,
    na.strings = 'NA')

male.dat2 <- all.dat2[all.dat2$Sex == 'Male',]
female.dat2 <- all.dat2[all.dat2$Sex == 'Female',]
juv.dat2 <- all.dat2[all.dat2$Sex == 'Juvenile',]



x11()
plot(male.dat2$SCL, male.dat2$Mass, xlab="SCL (cm)", ylab="Mass (kg)", col="blue")
points(female.dat2$SCL, female.dat2$Mass, col="red")
points(juv.dat2$SCL, juv.dat2$Mass, col="black")


# Jeff's model
nlsEXP.par <- c(b0 = 2.86, b1 = 0.04)      # these are from Jeff's work
nlsEXP.male <- nls(Mass ~ b0 * exp(b1 * SCL), start = nlsEXP.par, data=male.dat2)
nlsEXP.female <- nls(Mass ~ b0 * exp(b1 * SCL), start = nlsEXP.par, data=female.dat2)

nlsEXP.fit <- nls(Mass ~ b0 * exp(b1 * SCL), start = nlsEXP.par, data=all.dat2)

# cubic model without outliers:
nlsCUBE.male <- nls(Mass ~ b0 + b1*(SCL^3), start = list(b0 = 0.0, b1 = 0.0), data=male.dat2)
nlsCUBE.female <- nls(Mass ~ b0 + b1*(SCL^3), start = list(b0 = 0.0, b1 = 0.0), data=female.dat2)

nlsCUBE.fit <- nls(Mass ~ b0 + b1*(SCL^3), start = list(b0 = 0.0, b1 = 0.0), data=all.dat2)

scl.vec <- seq(min(all.dat2$SCL, na.rm="T"), max(all.dat2$SCL, na.rm="T"), by = 0.1)
lines(scl.vec, predict(nlsEXP.male, data.frame(SCL=scl.vec)), lty = 1, col = 'black')
lines(scl.vec, predict(nlsEXP.fit, data.frame(SCL=scl.vec)), lty = 2, col = 'black')

lines(scl.vec, predict(nlsCUBE.male, data.frame(SCL=scl.vec)), lty = 3, col = 'black')
lines(scl.vec, predict(nlsCUBE.fit, data.frame(SCL=scl.vec)), lty = 4, col = 'black')

lines(scl.vec, 2.86 * exp(0.04 * scl.vec), lty = 5, col = 'black')

x11()
par(mfrow = c(2, 1))
plot(all.dat2$SCL, all.dat2$SCW, xlab="", ylab = "SCW (cm)", col='black')
points(female.dat2$SCL, female.dat2$SCW, col='red')
points(male.dat2$SCL, male.dat2$SCW, col='blue')
lin.fit1m <- lm(SCW ~ SCL, data=male.dat2)
lin.fit1f <- lm(SCW ~ SCL, data=female.dat2)
lin.fit1 <- lm(SCW ~ SCL, data=all.dat2)
lines(scl.vec, predict.lm(lin.fit1, data.frame(SCL=scl.vec)))
lines(scl.vec, predict.lm(lin.fit1m, data.frame(SCL=scl.vec)), col = 'black', lty = 2)
lines(scl.vec, predict.lm(lin.fit1f, data.frame(SCL=scl.vec)), col = 'black', lty = 3)

#x11()
plot(all.dat2$SCL, all.dat2$CCL, xlab="SCL (cm)", ylab="CCL (cm)", col='black')
points(female.dat2$SCL, female.dat2$CCL, col='red')
points(male.dat2$SCL, male.dat2$CCL, col='blue')
lin.fit2 <- lm(CCL ~ SCL, data=all.dat2)
lin.fit2m <- lm(CCL ~ SCL, data=male.dat2)
lin.fit2f <- lm(CCL ~ SCL, data=female.dat2)
lines(scl.vec, predict.lm(lin.fit2m, data.frame(SCL=scl.vec)), col='black', lty = 2)
lines(scl.vec, predict.lm(lin.fit2f, data.frame(SCL=scl.vec)), col='black', lty = 3)
lines(scl.vec, predict.lm(lin.fit2, data.frame(SCL=scl.vec)), col='black', lty = 4)

