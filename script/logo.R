
library(plot3D)
library(grid)

logo <- function(col = jet.col(), resfac=1){
  # par(mfrow = c(1, 1), mar=c(1,1,1,1) * 0)
  AA <- plot3D::Hypsometry$z;  AA[AA<=0] <- NA
  dd = 55
  ZZ = rbind(AA[(dd+1):359, ], AA[1:dd, ])
  ZZ = ZZ ^ 0.75
  plot3D::spheresurf3D(ZZ, NAcol = rgb(0, 94/255, 220/255, .25), 
                       resfac = resfac, # output resolution
                       box = F,bty = "g", 
                       colkey = FALSE,
                       # xlim = lim, ylim = lim, zlim = lim, 
                       theta = 90+dd, phi = 25,
                       col = col, border='gold',
                       lwd=0.01,
                       lighting = TRUE, ltheta = 135+dd, lphi = 35, # Light
                       shade = TRUE)
}

fun.logo <- function(resfac = 0.5, wd=120, fn.fig = 'logo.png'){
  dir.create(dirname(fn.fig), showWarnings = FALSE, recursive = TRUE)
  # fig <- magick::image_graph(res = 96, width=512, height=512, clip = TRUE)
  # png('logo.png', width=512, height=512, unit='px', bg='white')
  png(fn.fig, width=wd, height=wd, unit='px', bg='transparent')
  par(new=TRUE, fig=c(0., 1, 0., 1), mar=rep(0, 4), family='Hei')
  logo(resfac = resfac)
  # abline(h=0)
  # abline(v=0)
  #' ==== 曲线 =====
  # fx <- function(n, npi = 4){
  #   y=0; x = seq(-1 * npi * pi, npi * pi, length.out=100*npi)
  #   for(i in 1:n){ y = y+ sin(x/i)/i }
  #   y
  # }
  # y = fx(6, 4)
  par(new=TRUE, fig=c(0.1, 0.9, c(0.40, 0.60) - 0.13), mar=rep(0, 4))
  # text(0.05, -1., 'www.shud.xyz', cex=1.4, col='darkred', font=2)
  # plot( y, col='gray80', lwd=6*resfac, type='l', xlab='', ylab='', axes=F)
  # lines(y, col=2, lwd=1.5*resfac)
}

fun.logo(resfac = 0.5, wd=120, fn.fig = 'static/res/logo.png')
text(0.05, -1., 'GHDC', cex=2, col='darkred', font=3)
dev.off()

fun.logo(resfac = 0.5, wd=600,  fn.fig = 'static/res/logo_en.png')
text(0.05, -1., 'Global Hydrologic Data Cloud', cex=3, col='darkred', font=3)
dev.off()

fun.logo(resfac = 0.5, wd=600, fn.fig = 'static/res/logo_cn.png')
text(0.0, -.75, '全球水文数据云', cex=5, col='darkred', font=3)
text(0.0, -1.25, 'Global Hydrologic Data Cloud', cex=3.2, col='darkred', font=3)
# text(0.0, -1.55, 'https://shuddata.com', cex=1.2, col='black', font=3)
dev.off()
