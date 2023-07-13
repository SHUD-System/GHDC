
fun.granule <- function(spx, shrink=FALSE){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  
  if(grepl('^SpatialPolygon', class(spx))){
    if(length(spx)>1){
      spx = rgeos::gUnaryUnion(spx)
    }
  }else{  }
  ext0 = extent(spx)
  ext = c(floor(ext0[1]), ceiling(ext0[2]), 
          floor(ext0[3]), ceiling(ext0[4]))
  
  x.granule = fishnet(xx=seq(ext[1], ext[2], by=1), 
                      yy=seq(ext[3], ext[4], by=1), 
                      crs = crs(spx), type='polygon')
  nx = length(x.granule)
  xmin = x.granule@data$xmin
  ymin = x.granule@data$ymin
  xlab = rep('E', nx); xlab[xmin<0] = 'W'
  ylab = rep('N', nx); ylab[ymin < 0] = 'S'
  GID= paste0(ylab, formatC(abs(ymin), digits = 2, width=2, flag='0'), 
              xlab, formatC(abs(xmin), digits = 3, width=3, flag='0') )
  x.granule@data = data.frame(x.granule@data, 'GID' = GID)
  if(shrink){
    # plot(x.granule);plot(add=T, spx, col=2)
    # xx=raster::crop(x.granule, spx)
    # xx
    xid = which(rgeos::gIntersects(spx, x.granule, byid=T))
    rr = x.granule[xid, ]
  }else{
    rr = x.granule
  }
  # plot(rr, axes=T)
  # grid()
  # plot(add=T, col=rgb(1,0,0,0.3), spx, border=2)
  writelog(paste0('Finished.'), caller=caller)
  return(rr)
}
#================================================================
#================================================================
GDEM_files <- function(fn.bnd,  dir.out, dir.rawdem,
                       dir.fig = dir.out,
                       fn.pre = 'ASTGTMV003_', crs.gcs = crs('+init=epsg:4326'), shrink=TRUE){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  
  lapply(list(dir.out, dir.fig), dir.create, recursive = TRUE, showWarnings = FALSE)
  # fn.out = file.path(dir.out, paste0('dl.sh'))
  # =======Load the boundary shapefile========
  sp.bnd = rgdal::readOGR(fn.bnd)
  sp.bnd = sp::spTransform(sp.bnd, crs.gcs)
  # ======Generate the granule ================
  png(filename = file.path(dir.fig, paste0('GDEM_grids.png')), height = 7, width = 7, res=300, unit='in')
  xg=fun.granule(sp.bnd, shrink=shrink)
  dev.off()
  rSHUD::writeshape(xg, file=file.path(dir.out, paste0('GDEM_grids')))
  
  # =======FILE links =============
  fn = paste0(fn.pre, as.matrix(xg@data$GID))
  fn.zip = file.path(dir.rawdem, paste0(fn, '_dem.tif'))
  # fn.zip = list.files(dir.rawdem, pattern=glob2rx('*N36E103*_dem.tif'), full.names = TRUE)
  
  fe = file.exists(fn.zip)
  idx = which(fe)
  id.missing = which(!fe)
  if(length(id.missing)>0){
    writelog(msg='Missing ASTER DEM grid', paste(fn[id.missing], collapse = ','))
  }
  writelog(paste0('Finished.'), caller=caller)
  return(fn.zip[idx])
}

getDEM_ASTER <- function(fn.wbd, 
                         dir.rawdem,
                         dir.out,
                         dir.fig = file.path(dir.out, 'Figure'),
                         copytofile = NULL,
                         crop=TRUE){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  
  fn.dem = file.path(dir.out, 'GDEM.tif')
  fn.demcrop = file.path(dir.out, 'GDEM_crop.tif')
  #======== Generate the links ==============
  fns.dem = GDEM_files(dir.out=file.path(dir.out, 'GDEM'), 
                       dir.fig = dir.fig, 
                       fn.bnd = fn.wbd, 
                       dir.rawdem = dir.rawdem, 
                       shrink = crop)
  writemessage(paste('Merging ', length(fns.dem), ' files'),  caller = caller, CV$task.log)
  writelog(paste('Merging ', length(fns.dem), ' files'), caller=caller)
  if(!file.exists(fn.dem)){
    #========= mosaic Tiff files =============
    cmd = fun.gdalmerge(fin = fns.dem, fout = fn.dem)
    writelog(paste('GDDAL command:', cmd), caller=caller)
    writemessage(paste0('done the gdal_merge command.'), caller=caller, CV$task.log)
  }else{
    writelog(paste('File exist ', fn.dem), caller=caller)
  }
  
  fn.fig = file.path(dir.fig, paste0('dem_buf.png'))
  writelog(paste0('Writing figure: ', fn.fig), caller=caller)
  if(crop){
    writemessage(paste0('Croping DEM ... :', fn.demcrop), caller=caller, CV$task.log)
    writelog(paste0('Croping DEM ... :', fn.demcrop), caller=caller)
    # writelog(paste0('Writing raster: ', fn.demcrop), caller=caller)
    fun.gdalcut(f.in = fn.dem, f.mask = fn.wbd, f.out = fn.demcrop,
                s_srs = crs(raster(fn.dem)), t_srs = crs(raster(fn.dem)))
    # r.crop = raster::crop(r, spx)
    # sp.crs = sp::spTransform(spx, crs(r.crop))
    # r.mask = raster::mask(r.crop, sp.crs)
    # png(filename = fn.fig, height = 7, width = 7, units = 'in', res = 300)
    # raster::plot(r.mask, axes=TRUE); 
    # raster::plot(spx, add=TRUE, border='red');  
    # grid()
    # dev.off()
    # writeRaster(r.mask, filename = fn.demcrop, overwrite=TRUE)
    # # if(!is.null(copytofile)){
    # #   file.copy(from=fn.demcrop, to=copytofile)
    # #   ret = copytofile
    # # }else{
    ret = fn.demcrop
    # }
  }else{
    # if(!is.null(copytofile)){
    #   file.copy(from=fn.dem, to=copytofile)
    #   ret = copytofile
    # }else{
    ret = fn.dem
    # }
  }
  # r = raster(fn.dem)
  spx = rgdal::readOGR(fn.wbd)
  png(filename = fn.fig, height = 7, width = 7, units = 'in', res = 300)
  raster::plot(raster(ret), axes=TRUE); 
  raster::plot(spx, add=TRUE, border='red');  
  grid()
  dev.off()
  
  writelog(paste0('Finished.'), caller=caller)
  return(ret)
}


getDEM_MERITHYDRO <- function(fn.wbd, 
                              dir.rawdem,
                              dir.out,
                              dir.fig = file.path(dir.out, 'Figure'),
                              copytofile = NULL,
                              crop=TRUE){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  
  fn.dem = file.path(dir.out, 'GDEM.tif')
  fn.demcrop = file.path(dir.out, 'GDEM_crop.tif')
  spx = rgdal::readOGR(fn.wbd)
  ext=extent(spx)
  cmd= paste('python3 /home/wwwroot/r/python/dem_merit_hydro90.py ', 
             floor(ext[1]), ceiling(ext[1]), floor(ext[3]), ceiling(ext[4]), 
             paste0(dir.rawdem, '/'), fn.dem )
  
  writelog(paste('GDDAL command:', cmd), caller=caller)
  system(cmd)
  writemessage(paste0('done the gdal_merge command.'), caller=caller, CV$task.log)
  #======== Generate the links ==============
  # fns.dem = GDEM_files(dir.out=file.path(dir.out, 'GDEM'), 
  #                      dir.fig = dir.fig, 
  #                      fn.bnd = fn.wbd, 
  #                      dir.rawdem = dir.rawdem, 
  #                      shrink = crop)
  # writemessage(paste('Merging ', length(fns.dem), ' files'),  caller = caller, CV$task.log)
  # writelog(paste('Merging ', length(fns.dem), ' files'), caller=caller)
  # if(!file.exists(fn.dem)){
  #   #========= mosaic Tiff files =============
  #   cmd = fun.gdalmerge(fin = fns.dem, fout = fn.dem)
  #   writelog(paste('GDDAL command:', cmd), caller=caller)
  #   writemessage(paste0('done the gdal_merge command.'), caller=caller, CV$task.log)
  # }else{
  #   writelog(paste('File exist ', fn.dem), caller=caller)
  # }
  
  fn.fig = file.path(dir.fig, paste0('dem_buf.png'))
  writelog(paste0('Writing figure: ', fn.fig), caller=caller)
  if(crop){
    writemessage(paste0('Croping DEM ... :', fn.demcrop), caller=caller, CV$task.log)
    writelog(paste0('Croping DEM ... :', fn.demcrop), caller=caller)
    # writelog(paste0('Writing raster: ', fn.demcrop), caller=caller)
    fun.gdalcut(f.in = fn.dem, f.mask = fn.wbd, f.out = fn.demcrop,
                s_srs = crs(raster(fn.dem)), t_srs = crs(raster(fn.dem)))
    # r.crop = raster::crop(r, spx)
    # sp.crs = sp::spTransform(spx, crs(r.crop))
    # r.mask = raster::mask(r.crop, sp.crs)
    # png(filename = fn.fig, height = 7, width = 7, units = 'in', res = 300)
    # raster::plot(r.mask, axes=TRUE); 
    # raster::plot(spx, add=TRUE, border='red');  
    # grid()
    # dev.off()
    # writeRaster(r.mask, filename = fn.demcrop, overwrite=TRUE)
    # # if(!is.null(copytofile)){
    # #   file.copy(from=fn.demcrop, to=copytofile)
    # #   ret = copytofile
    # # }else{
    ret = fn.demcrop
    # }
  }else{
    # if(!is.null(copytofile)){
    #   file.copy(from=fn.dem, to=copytofile)
    #   ret = copytofile
    # }else{
    ret = fn.dem
    # }
  }
  # r = raster(fn.dem)
  png(filename = fn.fig, height = 7, width = 7, units = 'in', res = 300)
  raster::plot(raster(ret), axes=TRUE); 
  raster::plot(spx, add=TRUE, border='red');  
  grid()
  dev.off()
  
  writelog(paste0('Finished.'), caller=caller)
  return(ret)
}
# 
# sp.wbd = readOGR(CV$etv$wbd.gcs)
# sp.buf = readOGR(CV$etv$buf.gcs)
# crs.gcs = CV$para$gcs
# # ======= 1. get DEM ==========
# dir.rawdem = file.path(CV$serv$PATH2SD, 'DEM/Merit_Hydro90')
# fn.dem.tmp = getDEM_MERITHYDRO(fn.wbd = CV$etv$buf.gcs,
#                                dir.rawdem = dir.rawdem, dir.fig = CV$dirs$fig,
#                                dir.out = CV$dirs$temp,
#                                copytofile = CV$etv$dem, crop=FALSE)
