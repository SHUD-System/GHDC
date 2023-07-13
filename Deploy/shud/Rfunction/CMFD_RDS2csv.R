# read the RDS above, to save as .csv file.
# source('GetReady.R')
fun.CMFD.RDS2csv <- function(xfg){
  source('AutoSHUD/Rfunction/LDAS_UnitConvert.R')
  years = xfg$years
  fns = file.path(xfg$dir$predata, paste0('CMFD_',years, '.RDS'))
  fns
  
  cns = c("Prec", "Temp", "SHum", "SRad",  "Wind", "Pres")
  forcnames = c( "Prcp_mm.d", "Temp_C", "RH_%", "Wind_m.s", "RN_w.m2" )
  
  nf=length(fns)
  for(i in 1:nf){
    x=readRDS(fns[i])  
    message(i,'/', nf, '\t', basename(fns[i]))
    y=x[,cns,]
    if(i==1){
      dat = y
    }else{
      dat=abind::abind(dat, y, along=3)
    }
  }
  dn = dimnames(dat)
  nd = dim(dat)
  xl = list()
  
  time = as.POSIXct(dimnames(dat)[[3]], tz='UTC')
  for(i in 1:nd[1]){
    message(i,'/', nd[1], '\t', dn[[1]][i] )
    x = t( dat[i,,] )
    y=unitConvert.CMFD(x)
    xl[[i]]=as.xts(y, order.by=time)
  }
  
  nx=length(xl)
  sitename = dn[[1]]
  sitename
  fns=paste0(sitename, '.csv')
  for(i in 1:nx){
    fn=fns[i]
    message(i,'/', nx, '\t', fn)
    write.tsd(xl[[i]], file.path(xfg$dir$forc, fn))
    if(i==1){
      xmean = xl[[i]]
    }else{
      xmean = xmean + xl[[i]]
    }
  }
  png(fn=file.path(xfg$dir$fig, paste0(xfg$prefix, '_Rawdata','_CMFD_TS.png') ), 
      height=6, width=7, res=200, units = 'in')
  plot.zoo(xmean/nx, main='CMFD')
  dev.off()
  
}