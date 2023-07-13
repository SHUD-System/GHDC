#' ===============================================================
#' Author: Lele Shu <shulele@lzb.ac.cn>
#' Date: 2022.10.03
#' Function: The autoSHUD function
#' ===============================================================


Delineation <- function(CV,
                        FlowAccCell.min=NULL,
                        fillsteps=3
                        # dir.data = CV$dirs$temp,
                        # dir.fig = CV$dirs$fig,
                        # fn.wbd = CV$etv$wbd.gcs,
                        # fsp.stm = CV$etv$stm_dem,
                        # fsp.wbd = CV$etv$wbd_dem,
                        # fsp.outlets = CV$etv$outlets,
                        # fnr.dem = CV$etv$dem,
                        ){
  # CV
  dir.data = CV$dirs$temp
  dir.fig = CV$dirs$fig
  fn.wbd = CV$etv$wbd.gcs
  fsp.stm = CV$etv$stm_dem
  fsp.wbd = CV$etv$wbd_dem
  fsp.outlets = CV$etv$outlets
  fnr.dem = CV$etv$dem
  crs.gcs = CV$para$gcs
  # FlowAccCell.min=NULL
  # fillsteps=3
  
  # whitebox::install_whitebox()
  # rm(list=ls())
  # library(tidyverse)
  # library(sf)
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  
  wbt_init()
  dir.create(dir.data, showWarnings = FALSE, recursive = TRUE)
  fnr.smoothing = file.path(dir.data, "dem.smoothed.tif")
  fnr.breached = file.path(dir.data, "dem.breached.tif")
  fnr.filled = file.path(dir.data, "dem.filled.tif")
  fnr.d8fa = file.path(dir.data, 'd8fa.tif')
  fnr.d8ptr = file.path(dir.data, 'd8ptr.tif')
  fnr.stm =  file.path(dir.data, 'stream.tif')
  fnr.stmclip =  file.path(dir.data, 'stream_clip.tif')
  # fnr.subs =  file.path(dir.data, 'subbasins.tif')
  # fnr.flood = file.path(dir.data, 'flood.tif')
  fnr.wbd = file.path(dir.data, 'wbd.tif')
  fsp.stm0 = file.path(dir.data, 'stream0.shp')
  plotr <- function(x, ...){raster::plot(raster(x), ...)}
  plotv <- function(x, ...){raster::plot(rgdal::readOGR(x), ...)}

  # 1. Fill Pits
  writelog(paste0('1. Fill Pits'), caller=caller)
  if(fillsteps==1){
    wbt_feature_preserving_smoothing(dem = fnr.dem, output = fnr.filled,filter = 9, num_iter = 3)
  }else if(fillsteps==2){
    wbt_feature_preserving_smoothing(dem = fnr.dem, output = fnr.smoothing, filter = 9, num_iter = 3)
    wbt_breach_depressions_least_cost(dem = fnr.smoothing,  output = fnr.filled,  dist = 5,  fill = TRUE)
  }else{
    wbt_feature_preserving_smoothing(dem = fnr.dem, output = fnr.smoothing, filter = 9, num_iter = 3)
    wbt_breach_depressions_least_cost(dem = fnr.smoothing,  output = fnr.breached,  dist = 5,  fill = TRUE)
    wbt_fill_depressions_wang_and_liu(dem = fnr.breached, output = fnr.filled)
  }
  # plot(raster(fnr.filled))
  

  # # 2. Flow Accumulation and Pointer Grids
  writelog(paste0('2. Flow Accumulation and Pointer Grids'), caller=caller)
  wbt_d8_flow_accumulation(input = fnr.filled, output = fnr.d8fa)
  wbt_d8_pointer(dem = fnr.filled, output = fnr.d8ptr)
  # plot(raster(fnr.d8fa))
  
  # 3. Watershed.
  writelog(paste0('3. Watershed.'), caller=caller)
  do_outlets <- function(){
    r = raster(fnr.d8fa)
    sp.wbd = rgdal::readOGR(fn.wbd)
    spp = sp::spTransform(sp.wbd, CRSobj = crs(r))
    r = raster::mask(r, spp)
    # plot(r)
    maxval = cellStats(r, max, na.rm=TRUE)
    idx = which.max(Which(r >= maxval))
    # idx = which.max(r, na.rm=TRUE)
    ll.outlets = xyFromCell(r,idx)
    sp.outlets = rSHUD::xy2shp(ll.outlets, crs = crs(r))
    rSHUD::writeshape(sp.outlets, file=fsp.outlets)
  }
  if(file.exists(fsp.outlets) ){
    #void
  }else{
    do_outlets()
  }

  
  # wbt_basins(d8_pntr = fnr.d8ptr, output = fnr.wbd)
  # wbt_raster_to_vector_polygons(fnr.wbd, output = fsp.wbd)
  # 
  # WITH outlets
  wbt_watershed(d8_pntr = fnr.d8ptr, pour_pts = fsp.outlets, output = fnr.wbd)
  wbt_raster_to_vector_polygons(fnr.wbd, output = fsp.wbd)
  # plotr(fnr.wbd)
  # plotv(fsp.wbd, add=T)
  # plotv(fn.wbd, border=2, add=T)
  
  writelog(paste0('4. Extract Streams'), caller=caller)
  # 4. Extract Streams
  if(is.null(FlowAccCell.min)){
    r = raster(fnr.d8fa)
    iaa = prod( res(r) * c(111, 111)*1000)
    idea.area = min(CV$para$Area / CV$json$minimum_cell_number, CV$json$maxim_cell_area*1e6)
    FlowAccCell.min = max(round( idea.area / (iaa) ) * 8, 10)
    # FlowAccCell.min = round(length(r)/1000, -2)
  }
  writelog(paste0('FlowAccCell.min = ', FlowAccCell.min), caller=caller)
  wbt_extract_streams(flow_accum = fnr.d8fa, output = fnr.stm, threshold = FlowAccCell.min, command_only=F)
  wbt_raster_streams_to_vector(streams=fnr.stm, d8_pntr = fnr.d8ptr, output = fsp.stm0)
  # prj = sp::proj4string(crs.gcs)
  # fn.prj = fsp.stm0;
  # raster::extension(fn.prj) = '.prj'
  # invisible(rgdal::showWKT(prj, file = fn.prj))
  toStream <- function(){
    riv0 = readOGR(fsp.stm0)
    wbd = readOGR(fsp.wbd)
    # crs(riv0) = crs(wbd)
    wbd.sf = as(wbd, 'sf')
    wbd.sf.val=sf::st_make_valid(wbd.sf)
    wbd.new = gUnaryUnion(as(wbd.sf.val, 'Spatial'))
    crs(riv0) = crs(wbd)
    x=terra::crop(riv0, wbd, snap="near", mask=TRUE)
    idx = which(gIntersects(riv0, wbd.new, byid=TRUE))
    x = riv0[idx, ]
    y = terra::crop(x, wbd.new, snap="near", mask=TRUE)
    y <- gIntersection(y, wbd.new, byid = T)
    plotv(fsp.wbd, axes=TRUE);plot(add=T, y, col=2);
    # dev.off()
    writeshape(y, file = fsp.stm)
    # plot(x);
    # dev.off()
    # plot(sp.wbd, border='red'); plotv(fsp.stm, add=T, col='blue')
  }; toStream()
  go.plot <- function(){
    plotr(fnr.dem)
    plotv(fn.wbd, border='darkblue', add=T)
    plotv(fsp.wbd, border=2, add=T)
    plotv(fsp.stm0, col='lightblue3', add=T)
    plotv(fsp.stm, col='blue', lwd=2, add=T)
  }; 
  # go.plot();
  # dev.off()

  
  writelog(paste0('Plot watershed_delineation.png'), caller=caller)
  go.plot <- function(){
    png(filename = file.path(dir.fig, paste0('ETV', '_watershed_delineation.png')), height = 7, width = 7, res=300, unit='in')
    par(mar=c(2, 2, 1, 1) )
    plotr(fnr.dem)
    plotv(fsp.wbd, add=T, border='red')
    plotv(fsp.stm0, col='lightblue3', add=T)
    plotv(fsp.stm, add=T, col='blue')
    plotv(fsp.outlets, add=T, col='darkred', cex=3)
    grid()
    mtext(side=3, line=-1, text=paste('Watershed Delineation', paste0('(threshold =', FlowAccCell.min, ')')) )
    dev.off()
  }; go.plot()
  writelog(paste0('Finished.'), caller=caller)
  # ret  = list(dem = fnr.filled, 
  #             stm = fsp.stm, 
  #             wbd = fsp.wbd)
  # return(ret)
}

# Delineation(CV, fillsteps=3)
