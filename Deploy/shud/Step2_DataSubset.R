AutoSHUD_Step2 <- function(CV){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  
  writemessage(paste0('Running the AutoSHUD_Step2(CV) '),  caller = caller, CV$task.log)
  raster.reclass <- function(fn.r, fn.tab, fn.ridx){
    rx = raster(fn.r)
    att = read.df(fn.tab)[[1]]
    rcl = cbind(att[, 1], 1:nrow(att))
    ry = raster::reclassify(rx, rcl)
    writeRaster(ry, filename = fn.ridx, overwrite=TRUE)
    # return(ry)
  }
  plotfun <-function(fn.r, key){
    r = raster(fn.r)
    png(filename = file.path(xfg$dir$fig, paste0(xfg$prefix,'_', key, '.png')), height = 7, width=7, unit='in', res=300)
    par(mar=c(2, 2, 1, 1))
    plot(r)
    plot(wb.p, add=T, border='red', lwd=1)
    plot(stm.p, add=T, col='blue', lwd=1)
    grid();  
    mtext(side=3, line=-1, paste0(key, '(Class)') )
    dev.off()
  }
  xfg = CV$deploy
  xfg$prefix = 'AutoSHUD_S2'
  stm.p = readOGR(xfg$pd.pcs$stm)
  wb.p = readOGR(xfg$pd.pcs$wbd)
  
  if(is.null(xfg)){
    writelog(msg="Error: xfg is missing in CV", caller = caller)
  }
  
  # =======Soil=================================================================
  # =======Soil=================================================================
  source(file.path(CV$dirs$deploy, 'Rfunction/Fun.Soil_Geol.R') )
  writemessage(paste0('Processing SOIL/Geol data... '),  caller = caller, CV$task.log)
  # this script for soil/geol data with SpatialData and Attribute Table.
  message('Processing SOIL/Geol data...')
  message('fn.soil: ', xfg$fn.soil)
  message('fa.soil: ', xfg$fa.soil)
  message('fn.geol: ', xfg$fn.geol)
  message('fa.geol: ', xfg$fa.geol)
  fun.gdalcut(f.in = xfg$fn.soil, f.mask = xfg$pd.pcs$wbd.buf,
              f.out= xfg$pd.pcs$soil.r, t_srs = xfg$crs.pcs, s_srs = xfg$crs.gcs)
  fun.gdalcut(f.in = xfg$fn.geol, f.mask = xfg$pd.pcs$wbd.buf,
              f.out= xfg$pd.pcs$geol.r, t_srs = xfg$crs.pcs, s_srs = xfg$crs.gcs)
  raster.reclass(fn.r = xfg$pd.pcs$soil.r, fn.tab = xfg$fa.soil, fn.ridx = xfg$pd.pcs$soil.idx)
  raster.reclass(fn.r = xfg$pd.pcs$geol.r, fn.tab = xfg$fa.geol, fn.ridx = xfg$pd.pcs$geol.idx)
  dat.soil = fun.Soil_Geol(xfg, TOP = TRUE)
  dat.geol = fun.Soil_Geol(xfg, TOP = FALSE)
  plotfun(fn.r = xfg$pd.pcs$geol.idx, key = 'Geol')
  plotfun(fn.r = xfg$pd.pcs$soil.idx, key = 'Soil')
  #
  # =======Land Cover ====================================================
  # =======Land Cover ====================================================
  writemessage(paste0('Processing landuse data... '),  caller = caller, CV$task.log)
  if( xfg$ilanduse >= 1){
    # local landuse map and attribute table.
    fun.gdalcut(f.in = xfg$fn.landuse, 
                f.mask = xfg$pd.pcs$wbd.buf,
                f.out= xfg$pd.pcs$lu.r, 
                t_srs = xfg$crs.pcs,
                s_srs = xfg$crs.gcs)
    raster.reclass(fn.r = xfg$pd.pcs$lu.r, fn.tab = xfg$tab.landuse, fn.ridx = xfg$pd.pcs$lu.idx)
    plotfun(fn.r = xfg$pd.pcs$lu.idx, key = 'Landuse')
  }
  
  # # =======Forcing =================================================================
  # # =======Forcing =================================================================
  writemessage(paste0('Processing reanalysis coverage data... '),  caller = caller, CV$task.log)
  if(xfg$iforcing >= 1){ 
    # local map
    att=read.df(xfg$fc.att)[[1]]
    sp.forc = xy2shp(xy = cbind(att$LON, att$LAT), crs=xfg$crs.gcs, shape = 'point', df = att)
    # plot(sp.forc)
    writeshape(sp.forc, file = xfg$pd.gcs$meteo)
    sp.forc.pcs = sp::spTransform(sp.forc, xfg$crs.pcs)
    writeshape(sp.forc.pcs, file = xfg$pd.pcs$meteo)
    png(filename = file.path(xfg$dir$fig, paste0(xfg$prefix,'_', 'Metero', '.png')), height = 7, width=7, unit='in', res=300)
    par(mar=c(2, 2, 1, 1))
    plot(sp.forc.pcs, axes=TRUE, col='darkgreen')
    plot(wb.p, add=T, border='red', lwd=1)
    plot(stm.p, add=T, col='blue', lwd=1)
    grid();  
    mtext(side=3, line=-1, paste0('Meterology sites',' (N=', nrow(sp.forc.pcs), ')') )
    dev.off()
  }else{
    stop(paste('WRONG LDAS CODE: ', xfg$iforcing))
  }
  writelog(paste0('Finished'), caller=caller)
  # return(xfg)
}
# AutoSHUD_Step2(CV)
