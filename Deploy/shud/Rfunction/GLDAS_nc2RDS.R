# read the orginal fldas data and save to .RDS file.

#install.packages('ncdf4')
require(ncdf4)
# library(rSHUD)
# source('GetReady.R')

# =========Forcing Coverage===========================
# sp0 = readOGR(xfg$fsp.forc)
# ========= Get the GRID===========================
# spx = readOGR(xfg$pd.gcs$meteoCov)
# fl=spx@data
fun.GLDAS.nc2RDS <- function(xfg, res=0.25){
  buf.g = readOGR(xfg$pd.gcs$wbd.buf)
  ext = extent(buf.g)
  dir.years = file.path(xfg$dir.ldas, xfg$years)
  
  ndir = length(dir.years)
  xfg$dir.ldas
  
  writelog(msg=paste0('forcing dir:', xfg$dir.ldas), caller = caller)
  fns=list.files(file.path(xfg$dir.ldas, '2002/100'), pattern=glob2rx('*.nc4'), recursive = T, full.names = T)[1]
  # cmd = paste('find',xfg$dir.lddas,  ' -name "*.nc4" > filelist.txt')
  # system(cmd)
  # fns=readLines(file.path(xfg$dir.ldas, 'filelist.txt'))
  
  fid=nc_open(fns[1])  #打开一个NC文件
  nc.all = rSHUD::readnc(fid, varid = 2)
  nc.sub = rSHUD::readnc(fid, varid = 2, ext = ext)
  nc_close(fid)
  r = xyz2Raster(x = nc.all)
  r.sub = xyz2Raster(x = nc.sub, res=res)
  
  # plot(r.sub); 
  # plot(add=T, buf.g)
  
  vns = names(fid$var)
  vns = vns[! grepl('time', tolower(vns))] # don't need the time_bnds
  # =========PLOT===========================
  go.plot <- function(prefix){
    png(file.path(xfg$dir$fig, paste0(prefix, '_LDAS_location.png')), height=8, width=15, units = 'in', res=200)
    plot(r * 0, col='gray', legend=FALSE)
    plot(r.sub * 0, col='red', legend=FALSE, add=TRUE)
    plot(buf.g, add=T)
    grid();
    mtext(side=3, 'GLDAS coverage (0.25 deg)')
    dev.off()
  }; go.plot(xfg$prefix)
  # =========Get the data===========================
  xfg$res = 0.25
  yx = expand.grid(nc.sub$y, nc.sub$x)
  rn = paste0('X', yx[, 2], 'Y',yx[,1])
  ext.fn = c(range(yx[, 2]), range(yx[, 1]) ) + c(-1, 1, -1, 1) *0.5 * xfg$res
  ext.fn
  sp.forc = fishnet(xx=seq(ext.fn[1], ext.fn[2], xfg$res ), 
                    yy=seq(ext.fn[3], ext.fn[4], xfg$res), crs=xfg$crs.gcs)
  go.plot <- function(prefix){
    png(file.path(xfg$dir$fig, paste0(prefix, '_LDAS_Coverage.png')), height=8, width=8, units = 'in', res=200)
    plot(sp.forc, axes=TRUE); 
    plot(buf.g, border='red', add=T); grid()
    text(yx[, 2], yx[, 1]+xfg$res/5, paste0('X', yx[, 2]))
    text(yx[, 2], yx[, 1]-xfg$res/5, paste0('Y', yx[, 1]))
    mtext(side=3, 'GLDAS coverage (0.25 deg)')
    dev.off()
  }; go.plot(xfg$prefix)
  
  sp0.gcs = spTransform(sp.forc, xfg$crs.gcs)
  sp0.pcs = spTransform(sp.forc, xfg$crs.pcs)
  writeshape(sp0.gcs, file = xfg$pd.gcs$meteoCov)
  writeshape(sp0.pcs, file = xfg$pd.pcs$meteoCov)

  ns = length(rn)
  nv=length(vns)
  # library(foreach)
  # library(doMC)
  # library(doParallel)
  # registerDoMC(12)
  # foreach (idd = 1:ndir) %dopar%{
  for (idd in 1:ndir) {
    cdir <- dir.years[idd]
    message(idd, '/', ndir, '\t', basename(cdir))
    fn.rds = file.path(xfg$dir$predata, paste0(xfg$prjname,'-', basename(cdir), '.RDS'))
    if(!file.exists(fn.rds)){
      fns = list.files(cdir, pattern=glob2rx('*.nc4'), recursive = T, full.names = T)
      nf = length(fns)
      x.t= character(nf)
      for(j  in 1:nf){  # files in each year
        fn=fns[j]
        message('\t', j, '/', nf, '\t', basename(fn))
        ncid = nc_open(fn)
        # debug(readnc)
        x.nc = readnc(ncid, varid=vns, ext=ext)
        nc_close(ncid)
        if(j == 1){
          d3 = dim(x.nc$arr)
          x.arr = array(0, dim=c(d3[1] * d3[2], nv, nf) )
        }
        x.t[j] = strftime(x.nc$time, usetz = FALSE, tz='UTC')
        x.arr[ , , j] = matrix(x.nc$arr, ncol=nv)
      }
      dimnames(x.arr) = list(rn, vns,  x.t)
      saveRDS(x.arr, file=fn.rds)
    }else{
    }
  }
}

# xfg = CV$xfg
# xfg$prefix = 'AutoSHUD_S2'
# fun.GLDAS.nc2RDS(xfg)
# fun.GLDAS.RDS2csv(xfg)