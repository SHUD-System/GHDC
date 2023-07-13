library(rSHUD)
library(tidyverse)

x=readLines('script/filelist.txt')
head(x)

x = gsub('W', '-', x)
x = gsub('E', '+', x)
x = gsub('S', '-', x)
x = gsub('N', '+', x)

ll=cbind(as.numeric(substr(x, 16, 19) ),
         as.numeric(substr(x, 13, 15))) 
ylim = range(ll[, 2])
spp = xy2shp(xy=ll, crs=CRS('+init=epsg:4326'))
png('static/res/Aster_GDEM.png', width = 300, height = 180, units = 'mm', res=120)
plot(spp, axes=T, pch=20, cex=0.2, col='darkblue'); grid()
abline(h = ylim[1], lty=2, col=2)
text(180, ylim[1]+2, paste0(abs(ylim[1]), 'S'), col=2)
abline(h = ylim[2], lty=2, col=2)
text(180, ylim[2]+2, paste0(abs(ylim[2]), 'N'), col=2)
mtext(side=3, text='ASTER Global DEM coverage (30m)')
dev.off()
