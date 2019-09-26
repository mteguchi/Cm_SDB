#plot_CapturedTurtles
# creates simple plots of body sizes for captured turtles

rm(list = ls())
# run a query with the updated database - probably not including the most
# recent captures

if (!file.exists(paste0('data/SDB_report_', Sys.Date(), '.csv'))){
  source("Sd_Bay_Report.R")
} else {
  turtle.haplo.SDB <- read.csv(file = paste0('data/SDB_report_', Sys.Date(), '.csv'))
}

library(ggplot2)

save.fig <- F

# provide a vector of captured turtle PIT numbers here for plotting later. 
turtle.haplo.SDB %>%
  mutate(SCL.regress = ifelse(is.na(SCL) & !is.na(CCL), 1, 0),
         CCL.regress = ifelse(is.na(CCL) & !is.na(SCL), 1, 0),
         SCL = ifelse(SCL.regress == 1,
                      (CCL - 0.64)/1.06, SCL),
         CCL = ifelse(CCL.regress == 1,
                      0.64 + 1.06 * SCL, CCL),
         Mass.regress = ifelse(is.na(Weight) & !is.na(SCL), 1, 0),
         Weight = ifelse(Mass.regress == 1,
                         exp(0.96 + 0.04 * SCL),
                         Weight),
         Date = as.Date(paste(Yr, Mo, Da, sep = "-"),
                        format = '%Y-%m-%d')) -> turtle.haplo.SDB

turtle.haplo.SDB %>% filter(Date > as.Date("2018-09-30")) -> new.captures

p.mass.all <- ggplot(data = turtle.haplo.SDB) + 
  geom_histogram(aes(x = Weight))

p.mass.all

p.mass.new <- ggplot(data = new.captures) + 
  geom_histogram(aes(x = Weight),
                 binwidth = 12)

p.mass.new


new.captures.PIT.LFF.unique <- na.omit(unique(new.captures$PIT_LFF))
new.captures.PIT.RFF.unique <- na.omit(unique(new.captures$PIT_RFF))

PITs <- c(as.character(new.captures.PIT.LFF.unique), 
          as.character(new.captures.PIT.RFF.unique))

# these are the capture histories of those that were caught during
# this reporting period
turtle.haplo.SDB %>% filter(PIT_RFF %in% PITs) -> PIT_RFF
turtle.haplo.SDB %>% filter(PIT_LFF %in%  PITs) -> PIT_LFF

# select one reading per animal:
turtle.haplo.SDB %>% group_by(Turtle_ID)-> turtle.size

turtle.size %>% summarise(max.weight = max(Weight, na.rm = T),
                          max.SCL = max(SCL, na.rm = T),
                          max.CCL = max(CCL, na.rm = T),
                          recent.date = max(Date)) -> turtle.size.max

# turtle.size %>% filter(PIT_RFF %in% PITs) -> PIT_RFF
# turtle.size %>% filter(PIT_LFF %in%  PITs) -> PIT_LFF
new.capture.df <- rbind(PIT_RFF, PIT_LFF) %>% group_by(Turtle_ID)

# ggplot() +
#   geom_histogram(data = turtle.size,
#                  aes(x = max.weight))

p.mass <- ggplot() +
  geom_path(data = turtle.size,
            aes(x = Date,
                y = Weight,
                group = Turtle_ID),
            size = 1) 
p.mass
geom_path(data = new.capture.df,
            aes(x = Date, y = Weight,
                color = NMFS_Tag),
            size = 1) +
  geom_point(data = new.capture.df,
            aes(x = Date, y = Weight,
                color = NMFS_Tag),
            size = 1) +

  labs(x = "", y = "Weight (kg)") +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               limits = c(as.Date("1990-01-01"), as.Date("2020-12-31")))
p.mass

p.SCL <- ggplot() +
  geom_point(data = turtle.size.max,
             aes(x = recent.date,
                 y = max.SCL),
             size = 1) +
  geom_path(data = new.capture.df,
            aes(x = Date, y = SCL,
                color = NMFS_Tag),
            size = 1) +
  geom_point(data = new.capture.df,
             aes(x = Date, y = SCL,
                 color = NMFS_Tag),
             size = 1) +
  labs(x = "", y = "SCL (cm)") +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               limits = c(as.Date("1990-01-01"), as.Date("2020-12-31")))


p.SCL

if (save.fig){
  ggsave(plot = p.mass,
         dpi = 600,
         file = paste0("figures/weight_", Sys.Date(), ".png"))
  ggsave(plot = p.SCL,
         dpi = 600,
         file = paste0("figures/SCL_", Sys.Date(), ".png"))

}
