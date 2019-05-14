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

PITs <- c("152307774A", "132136125A")


turtle.haplo.SDB %>% filter(PIT_RFF %in% PITs) -> PIT_RFF
turtle.haplo.SDB %>% filter(PIT_LFF %in%  PITs) -> PIT_LFF

# select one reading per animal:
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
                        format = '%Y-%m-%d')) %>%
  group_by(Turtle_ID) -> turtle.size

turtle.size %>% summarise(max.weight = max(Weight, na.rm = T),
                          max.SCL = max(SCL, na.rm = T),
                          max.CCL = max(CCL, na.rm = T),
                          recent.date = max(Date)) -> turtle.size.max

turtle.size %>% filter(PIT_RFF %in% PITs) -> PIT_RFF
turtle.size %>% filter(PIT_LFF %in%  PITs) -> PIT_LFF
new.capture.df <- rbind(PIT_RFF, PIT_LFF) %>% group_by(Turtle_ID)

# ggplot() +
#   geom_histogram(data = turtle.size,
#                  aes(x = max.weight))

p.mass <- ggplot() +
  geom_point(data = turtle.size.max,
            aes(x = recent.date,
                y = max.weight),
            size = 2) +
  geom_path(data = new.capture.df,
            aes(x = Date, y = Weight,
                color = NMFS_Tag),
            size = 3) +
  geom_point(data = new.capture.df,
            aes(x = Date, y = Weight,
                color = NMFS_Tag),
            size = 3) +

  labs(x = "", y = "Weight (kg)") +
  theme(#legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               limits = c(as.Date("1990-01-01"), as.Date("2020-12-31")))
p.mass

p.SCL <- ggplot() +
  geom_point(data = turtle.size.max,
             aes(x = recent.date,
                 y = max.SCL),
             size = 2) +
  geom_path(data = new.capture.df,
            aes(x = Date, y = SCL,
                color = NMFS_Tag),
            size = 3) +
  geom_point(data = new.capture.df,
             aes(x = Date, y = SCL,
                 color = NMFS_Tag),
             size = 3) +
  labs(x = "", y = "SCL (cm)") +
  theme(#legend.position = "none",
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