#plot_maps


rm(list=ls())

# to create new maps, open Cm_SDB.R and change internet = T
source('Cm_SDB.R')

save.fig <- F

map.west.coast <- ggmap(west.coast)
map.sdbay <- ggmap(sdbay.all)
map.south.sdbay <- ggmap(sdbay.south)

p1 <- map.west.coast +
  xlab('') + ylab('')

p2 <- map.sdbay +
  xlab('') + ylab('')

p3 <- map.south.sdbay +
  xlab('') + ylab('')

if (save.fig){
  ggsave(plot = p1,
         dpi = 600,
         device = 'png',
         filename = 'figures/west_coast.png')

  ggsave(plot = p2,
         dpi = 600,
         device = 'png',
         filename = 'figures/SDbay.png')

  ggsave(plot = p3,
         dpi = 600,
         device = 'png',
         filename = 'figures/SDbay_south.png')

}

