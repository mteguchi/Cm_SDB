#plot_waterTemp


rm(list=ls())
source('Cm_SDB.R')

save.fig <- F
# read the data file and clean up some ridiculous data points
data.0 <- read.csv('data/IntakeAndDischargeTemp.csv', header = T)
data.0 <- na.omit(data.0)
data.0[data.0$Discharge < 59 | data.0$Discharge > 104, 'Discharge'] <- NA
data.0 <- na.omit(data.0)

data.0$Date <- as.Date(data.0$Date, format = '%d-%b-%y %H:%M:%S')

data.0$Month <- as.numeric(format(as.Date(data.0$Date), '%m'))
data.0$fMonth <- as.factor(data.0$Month)


p1 <- ggplot() +
  geom_boxplot(data = data.0,
               aes(x = fMonth, y = F2C(Intake)),
               color = 'blue',
               size = 1.5,
               alpha = 0.6) +
  geom_boxplot(data = data.0,
               aes(x = fMonth, y = F2C(Discharge)),
               color = 'red',
               size = 1.1,
               alpha = 0.4) +
  #geom_hline(yintercept = 15) +
  ylab("") +
  xlab("")  +
  #ggtitle() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

if (save.fig){
  ggsave(plot = p1,
         dpi = 1200,
         device = 'png',
         filename = 'figures/intake_discharge.png')

}

dif.temp <- F2C(data.0$Discharge) - F2C(data.0$Intake)

mean.dif.temp <- mean(dif.temp)
se.dif.temp <- SE(dif.temp)


