ETV.Delineation <- function(CV){
  # fn.buf = CV$etv$buf.gcs
  # fn.wbd = CV$etv$wbd.gcs
  
  sp.wbd = readOGR(CV$etv$wbd.gcs)
  sp.buf = readOGR(CV$etv$buf.gcs)
  crs.gcs = CV$para$gcs
  # ======= 1. get DEM ==========
  if(grepl('merit90', tolower(CV$json$dem_source))){
    dir.rawdem = file.path(CV$serv$PATH2SD, 'DEM/Merit_Hydro90')
    writemessage(paste0('Trying to merge the Merit_Hydro90 DEM files... '),  caller = caller, CV$task.log)
    fn.dem.tmp = getDEM_MERITHYDRO(fn.wbd = CV$etv$buf.gcs, 
                              dir.rawdem = dir.rawdem, dir.fig = CV$dirs$fig,
                              dir.out = CV$dirs$temp, 
                              copytofile = CV$etv$dem, crop=FALSE)
    # file.copy(from=fn.dem.tmp, to = CV$etv$dem)
    writemessage(paste0('Trying to mask the DEM files... '),  caller = caller, CV$task.log)
    fun.gdalcut(f.in = fn.dem.tmp, f.mask = CV$etv$buf.gcs, t_srs = crs.gcs, f.out = CV$etv$dem)
    Delineation(CV, fillsteps=1)
  }else if(grepl('aster30', tolower(CV$json$dem_source))){
    dir.rawdem = file.path(CV$serv$PATH2SD, 'DEM/Aster_GDEM')
    writemessage(paste0('Trying to merge the Aster_GDEM files... '),  caller = caller, CV$task.log)
    fn.dem.tmp = getDEM_ASTER(fn.wbd = CV$etv$buf.gcs, 
                              dir.rawdem = dir.rawdem, dir.fig = CV$dirs$fig,
                              dir.out = CV$dirs$temp, 
                              copytofile = CV$etv$dem, crop=FALSE)
    # file.copy(from=fn.dem.tmp, to = CV$etv$dem)
    writemessage(paste0('Trying to mask the DEM files... '),  caller = caller, CV$task.log)
    fun.gdalcut(f.in = fn.dem.tmp, f.mask = CV$etv$buf.gcs, t_srs = crs.gcs, f.out = CV$etv$dem)
    Delineation(CV, fillsteps=3)
  }else{
    
  }
}
#' \code{task.ready}
#' Get Ready for a task.
#' 1. Get the DEM data and merge them together for the wbd.
#' 2. 
#' 
ExtractETV <- function(CV){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  fun.plot <- function(key, fnr, sp.wbd=NULL, sp.riv=NULL){
    r = raster(fnr)
    png(filename = file.path(CV$dirs$fig, paste0('ETV_', key, '.png')), height = 7, width = 7, units = 'in', res = 300)
    par(mar=c(2, 2, 1, 1) )
      raster::plot(r, axes=TRUE); 
    if(!is.null(sp.wbd)){ raster::plot(sp.wbd, add=TRUE, border='red')  }
    if(!is.null(sp.riv)){ raster::plot(sp.riv, add=TRUE, col='blue')  }
    mtext(side=3, line=-1, text=key)
    grid()
    dev.off()
  }
  sp.wbd = readOGR(CV$etv$wbd.gcs)
  sp.buf = readOGR(CV$etv$buf.gcs)
  crs.gcs = CV$para$gcs
  
  if(file.exists(CV$etv$stm_dem)){
    sp.riv=readOGR(CV$etv$stm_dem)
  }else{
    sp.riv=NULL
  }
  fun.plot(key='DEM', fnr=CV$etv$dem, sp.wbd=sp.wbd, sp.riv=sp.riv)
  
  # =================================================
  # ======= 2. get LANDCOVER ==========
  # =================================================
  writemessage(paste0('Mask the landuse data... '),  caller = caller, CV$task.log)
  fun.gdalcut(f.in = CV$files$landuse, f.mask = CV$etv$buf.gcs, t_srs = crs.gcs, f.out = CV$etv$landuse)
  fun.plot(key='Landuse', fnr=CV$etv$landuse, sp.wbd=sp.wbd, sp.riv=sp.riv)
  
  lc.att = read.df(file=CV$files$landuse.att)[[1]]
  write.df(x=lc.att, file = CV$etv$landuse.att)
  lr = LaiRf.GLC(years=CV$json$start_year:CV$json$end_year)
  write.tsd(round(lr$LAI, 2), file = CV$etv$landuse.lai)
  
  # ======= melt factor ( moved to model deployment)==========
  # ts.mf = rSHUD::MeltFactor(years=CV$json$start_year:CV$json$end_year)
  # write.tsd(round(ts.mf, 5), file = CV$etv$meltfactor)
  
  # go.plot <- function(){
  #   r.lu = raster(CV$etv$landuse)
  #   clr = read.table('AutoSHUD/Table/LCType_color.clr', sep='\t')
  #   tab = read.df(CV$etv$landuse.att)[[1]]
  #   tocol = function(x){rgb(x[, 1], x[, 2],x[, 3], min(1, x[, 4]) ) }
  #   col =  tocol(clr[, 2:5]/255)
  #   ulc = cellStats(r.lu, unique, na.rm=TRUE)
  #   brk = tab[, 1]; nbrk = length(brk)
  #   txt = rep('', nbrk); txt[ulc] = '(+)'
  #   labs = paste0(brk, '-', tab$remark, txt)
  #   png(filename = file.path(CV$dir$fig, paste0('ETV_Landuse_GLC.png')), height = 7, width=9, unit='in', res=300)
  #   par(mar=c(2, 2, 3, 15))
  #   plot(r.lu, legend=FALSE, col=col, breaks=brk)
  #   # plot(r.lu, legend.only=TRUE, col=col, breaks=brks, label=labs)
  #   plot(r.lu, legend.only=TRUE, breaks=c(brk, max(brk)+1), col=col, smallplot=c(0.67,0.70, 0.1,0.9),
  #        legend.width=2, legend.shrink=.5, cex=2, horizontal=FALSE,
  #        axis.args=list(at=0:(nbrk-1)+.5, labels=labs, cex.axis=.75),
  #        legend.args=list(text='',side=3, font=2, cex=0.8))
  #   plot(sp.wbd, add=T, border='red', lwd=2)
  #   plot(sp.riv, add=T, col='blue', lwd=1)
  #   grid();       mtext(side=3, line=-1, 'Landuse')
  #   dev.off()
  # };    go.plot()
  
  # =================================================
  # ======= 3. get SOIL/GEOL ==========
  # =================================================
  writemessage(paste0('Mask the soil/geology data... '),  caller = caller, CV$task.log)
  fun.gdalcut(f.in = CV$files$soil, f.mask = CV$etv$buf.gcs, s_srs = crs.gcs, t_srs = crs.gcs, f.out = CV$etv$soil)
  fun.gdalcut(f.in = CV$files$geol, f.mask = CV$etv$buf.gcs, s_srs = crs.gcs, t_srs = crs.gcs, f.out = CV$etv$geol)
  fun.plot(key='Soil', fnr=CV$etv$soil, sp.wbd=sp.wbd, sp.riv=sp.riv)
  fun.plot(key='Geol', fnr=CV$etv$geol, sp.wbd=sp.wbd, sp.riv=sp.riv)
  HWSD.att(fn.r = CV$etv$soil, fn.att=CV$files$soil.att, fn.out = CV$etv$soil.att, toplayer = TRUE)
  HWSD.att(fn.r = CV$etv$geol, fn.att=CV$files$geol.att, fn.out = CV$etv$geol.att, toplayer = FALSE)
  
  
  # =================================================
  # ======= 4. get FORCING GRID ==========
  # =================================================
  writemessage(paste0('Mask the reanalysis data... '),  caller = caller, CV$task.log)
  fun.gdalcut(f.in = CV$files$ldas, f.mask = CV$etv$wbd.gcs, t_srs = crs.gcs, f.out = CV$etv$ldas)
  r = raster::raster(CV$etv$ldas)
  ux = sort(cellStats(r, unique))
  nx=length(ux)
  df=read.table(file=CV$files$ldas.att, sep=',', header = TRUE)
  subdf = df[ux, ]
  write.df(subdf, file = CV$etv$ldas.att)
  plot.ldas <- function(key='LDAS',fnr, sp.wbd, sp.riv=NULL){
    png(filename = file.path(CV$dirs$fig, paste0('ETV_', key, '.png')), height = 7, width = 7, units = 'in', res = 300)
    par(mar=c(2, 2, 1, 1) )
    # cols=sample(colorspace::rainbow_hcl(n=nx*10), nx)
    # raster::plot(r, axes=TRUE, breaks=ux, col=cols); 
    raster::plot(r, axes=TRUE, legend=FALSE); 
    dx=mean(res(r), na.rm=TRUE)
    points(x=subdf[, 'LON'], y=subdf[, 'LAT'], col='darkblue', pch=1, cex=0.25)
    text(x=subdf[, 'LON'], y=subdf[, 'LAT'] + dx/6, subdf[, 'LON'], col='blue', cex=0.25)
    text(x=subdf[, 'LON'], y=subdf[, 'LAT'] - dx/6, subdf[, 'LAT'], col='blue', cex=0.25)
    if(!is.null(sp.wbd)){ raster::plot(sp.wbd, add=TRUE, border='red')  }
    if(!is.null(sp.riv)){ raster::plot(sp.riv, add=TRUE, border='blue')  }
    mtext(side=3, line=-1, text=paste(key, paste0('(N =', nx, ')')) )
    grid()
    dev.off()
  }
  plot.ldas(key='LDAS', fnr=CV$etv$ldas, sp.wbd=sp.wbd, sp.riv=sp.riv)
  
  # =================================================
  # ======= 5. get FORCING TSD  ==========
  # =================================================
  writemessage(paste0('Getting the time-series forcing data... '),  caller = caller, CV$task.log)
  fns = subdf[, 'FILENAME']
  LDAS.funs <- list(
    gldas = GLDAS.rds2csv,
    nldas = NLDAS.rds2csv,
    cmfd = CMFD.rds2csv,
    fldas = FLDAS.rds2csv,
    cldas = CLDAS.rds2csv
  )
  ldasfun <- LDAS.funs[[tolower(CV$json$meteorological_data)]]
  ldasfun(CV, fns = fns)
}
# ExtractETV(CV)
