AutoSHUD_Step1 <- function(CV){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  
  writemessage(paste0('Running the AutoSHUD_Step1(CV) '),  caller = caller, CV$task.log)
  prefix = 'AutoSHUD_S1'
  xfg = CV$deploy
  if(is.null(xfg)){ writelog(msg="Error: xfg is missing in CV", caller = caller) }
  # ================= Boundary =================
  wbd0 = readOGR(xfg$fsp.wbd)  # Read data
  wbd0 = gBuffer(wbd0, width=0) # Remove error from irregular polygon.
  # ---- disolve ----
  wbd.dis = removeholes(gUnaryUnion(wbd0))
  
  # wbd in pcs
  wb.p = spTransform(wbd0, xfg$crs.pcs) 
  writeshape(wb.p, xfg$pd.pcs$wbd)
  
  # buffer of wbd in pcs
  buf.p = gBuffer(wb.p, width = xfg$para$DistBuffer) 
  writeshape(buf.p, xfg$pd.pcs$wbd.buf)
  
  buf.g = spTransform(buf.p, xfg$crs.gcs)
  writeshape(buf.g, xfg$pd.gcs$wbd.buf)
  
  wb.g=spTransform(wb.p, CRSobj = xfg$crs.gcs )
  writeshape(wb.g, xfg$pd.gcs$wbd)
  
  
  # ================= DEM =================
  dem0=raster(xfg$fr.dem)
  # -------CROP DEM -----------------
  # Crop the dem AND conver the dem to PCS.
  writelog(msg=paste0('Cuting DEM ...'), caller = caller)
  fun.gdalwarp(f1=xfg$fr.dem,
               f2=xfg$pd.pcs$dem,
               t_srs = xfg$crs.pcs,
               s_srs = crs(dem0),
               opt = paste0('-cutline ', xfg$pd.pcs$wbd.buf) )
  # # Crop the dem, output is in GCS
  fun.gdalwarp(f1=xfg$fr.dem,
               f2=xfg$pd.gcs$dem,
               t_srs = xfg$crs.gcs,
               s_srs = crs(dem0),
               opt = paste0('-cutline ', xfg$pd.pcs$wbd.buf) )
  
  # =========Stream Network===========================
  writelog(msg=paste0('Stream network ...'), caller = caller)
  stm0 = readOGR(xfg$fsp.stm)  # data 0: raw data
  stm1 = spTransform(stm0, xfg$crs.pcs)  # data 1: PCS
  fun.simplifyRiver <- function(rmDUP=TRUE){
    riv.xy = extractCoords(stm1)
    npoint = nrow(riv.xy)
    mlen = gLength(stm1) / npoint
    r.dem = raster(xfg$pd.pcs$dem)
    dx = mean(res(r.dem))
    if( mlen < dx){
      stm1 = gSimplify(stm1, tol = dx)
    }
    if(rmDUP){
      res = rmDuplicatedLines(stm1)
    }else{
      res = stm1
    }
    res
  }
  # debug(sp.RiverDown)
  if(xfg$para$flowpath){
    stm1 = fun.simplifyRiver(rmDUP = FALSE)
    stm.p= sp.RiverPath(stm1, tol.simplify = 30)$sp  # clean data with flowpath.
    stm.p = stm1
  }else{
    stm.p = stm1
  }
  
  writelog(msg=paste0('writing stream out: ', xfg$pd.pcs$stm), caller = caller)
  writeshape(stm.p, file=xfg$pd.pcs$stm)
  
  #' ==========================================
  if(xfg$LAKEON){
    writemessage(paste0('Lake module is ENABLED  '),  caller = caller, CV$task.log)
    writelog(msg=paste0('Lake is ON.'), caller = caller)
    spl0 = readOGR(xfg$fsp.lake)  # data 0: raw data
    spl1 = removeholes(spl0)
    spl.gcs = spTransform(spl1, CRSobj = xfg$crs.gcs)
    writeshape(spl.gcs, xfg$pd.gcs$lake)
    
    spl.pcs = spTransform(spl.gcs, CRSobj = xfg$crs.pcs)  # data 1: PCS
    writeshape(spl.pcs, xfg$pd.pcs$lake)
  }
  
  #' ==== PLOT FIGURE ================
  writelog(msg=paste0('Ploting'), caller = caller)
  dem.p = raster(xfg$pd.pcs$dem)
  png(file.path(xfg$dir$fig, paste0(prefix, '_Rawdata_Elevation.png')), height=8, width=8, units = 'in', res=200)
  par(mar=c(2, 2, 1, 1) )
  plot(dem.p)
  plot(wb.p, add=T, border=2)
  plot(stm.p, add=T, col=4);
  if(xfg$LAKEON){
    plot(spl.pcs, add=TRUE, border='darkblue', lwd=1.5)
  }
  grid()
  dev.off()
  
  writelog(paste0('Finished'), caller=caller)
}
# AutoSHUD_Step1(CV)


