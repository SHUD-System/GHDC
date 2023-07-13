
#' Build a Albers Equal Area projection based on the spatial data or extent.
#' @param CV
#' @param 
#' @return the parameter list for AutoSHUD.
#' @export
write.autoshud <- function(CV, dir.ldas=CV$etv$ldas){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  
  fn.pcs = normalizePath(CV$etv$wbd.pcs)
  fn.wbd = normalizePath(CV$etv$wbd_dem)
  fn.stm = normalizePath(CV$etv$stm_dem)
  fn.dem = normalizePath(CV$etv$dem)
  AA = CV$para$Area
  
  # ------ Day duration  --------------
  day0 = as.Date(paste0(CV$json$start_year, '-01-01'))
  day1 = as.Date(paste0(CV$json$end_year, '-12-31'))
  # ndays = as.numeric(day1 - day0)+1
  ndays = as.numeric(difftime(day1, day0, units='days'))+1
  # ------ MISC from AREA of the watershed boundary  --------------
  rad = sqrt(AA/pi)
  if(is.null(CV$json$maxim_cell_area) | CV$json$maxim_cell_area < 0){
    CV$json$maxim_cell_area = AA / CV$json$minimum_cell_number /1e6
  }else{
    CV$json$maxim_cell_area  = CV$json$maxim_cell_area 
  }
  if(is.null(CV$json$aquifer_depth) | CV$json$aquifer_depth < 10){
    CV$json$aquifer_depth = 10
  }else{
    CV$json$aquifer_depth = CV$json$aquifer_depth
  }
  irad = sqrt( as.numeric(CV$json$maxim_cell_area) * 1e6 / pi) # meters
  
  # tol.wb = max(round( irad *2), 30)
  # tol.rivlen = max(round(irad /50), 15)
  
  if(is.null(CV$json$tol.wb) ){
    CV$json$tol.wb = max(round( irad / 3), 30)
  }else{
    CV$json$tol.wb = CV$json$tol.wb
  }
  writelog(msg= paste(" tol.wb =", CV$json$tol.wb), caller = caller)
  
  if(is.null(CV$json$tol.rivlen)){
    CV$json$tol.rivlen = max(round(irad), 15)
  }else{
    CV$json$tol.rivlen =  CV$json$tol.rivlen
  }
  writelog(msg= paste(" tol.rivlen =", CV$json$tol.rivlen), caller = caller)
  
  
  rivwidth = CV$json$rivwidth
  rivdepth = CV$json$rivdepth
  # rivwidth = round(max(min(irad/6, 100), 2), 1),
  # rivdepth = round(max(min(tol.rivlen/600, 10), 3), 1)
  
  # ------ the config list --------------
  x = list(
    'prjname' = CV$json$project_name,
    'startyear' = CV$json$start_year,
    'endyear' = CV$json$end_year,
    'dir.out' = CV$dirs$model,
    # ---- basic ---- 
    # 'fsp.crs' = fn.pcs,
    'fsp.wbd' = fn.wbd,
    'fsp.stm' = fn.stm,
    'fr.dem' = fn.dem,
    
    # ---- forcing ------
    'Forcing' = 1,
    'fc.tsd' = CV$dirs$tsd,
    'fc.out' = file.path(CV$dirs$model, 'forcing'),
    'fc.att' = CV$etv$ldas.att,
    # ---- SOIL/GEOL ------
    'Soil' = 1,
    # 'dir.soil' = file.path(CV$serv$PATH2SD, 'Soil/HWSD/HWSD_RASTER'),
    'fn.soil' = CV$etv$soil,
    'fa.soil' = CV$etv$geol.att,
    'fn.geol' = CV$etv$geol,
    'fa.geol' = CV$etv$soil.att,
    # ---- LANDUSE ------
    'Landuse' = 1,
    'fn.landuse' = CV$etv$landuse,
    'tab.landuse' = CV$etv$landuse.att,
    'lai.landuse' = CV$etv$landuse.lai,
    # 'meltfactor' = CV$etv$meltfactor,
    # ---- parameters ------
    'NumCells' = min(CV$json$minimum_cell_number, 10 * 1e4),
    'AqDepth' = round(CV$json$aquifer_depth, 3),
    'MaxArea' = round(CV$json$maxim_cell_area, 4),
    'MinAngle' = 30,
    'tol.wb' =  CV$json$tol.wb,
    'tol.rivlen' =  CV$json$tol.rivlen,
    'RivWidth' =  rivwidth,
    'RivDepth' =  rivdepth,
    'DistBuffer' = CV$para$distBuff,
    'flowpath' = 0,
    'QuickMode' = 0,
    'MAX_SOLVER_STEP' = 4,
    'CRYOSPHERE' = 0,
    'STARTDAY' = 0,
    'ENDDAY'= ndays
  )
  
  writelog(paste0('Writing file: ', CV$files$autoshud), caller=caller)
  xo = cbind(names(x), t(data.frame(x) ))
  write.table(xo, file = CV$deploy$config, col.names = FALSE, row.names = FALSE, quote = FALSE)
  writelog(paste0('Finished.'), caller=caller)
  return(x)
}

# write.autoshud(CV)

