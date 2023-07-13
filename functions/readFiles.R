readJSON <- function(theTask){
  fn.json = list.files(path = theTask, pattern = glob2rx("*.json"), full.names = TRUE)
  checkFile(fn.json, 'JSON')
  js.info = file.info(fn.json)
  x0 = list(
    'email'='admin@shuddata.com',
    'project_name'='shudprj',
    dataonly = 'yes',
    dem_source='aster30',
    start_year = '2020',
    end_year = '2020',
    minimum_cell_number = '1000',
    # tol.wb = '100',
    # tol.rivlen = '5000',
    meteorological_data = 'gldas',
    zip='',
    soil='hwsd',
    geol='hwsd',
    landuse='mglc',
    model='shud', 
    rivwidth='5', 
    rivdepth='4', 
    cryosphere='0', 
    codedir = ''
  )
  x1 <- fromJSON(file=fn.json)
  xj = utils::modifyList(x0, x1)  
  
  writelog(msg = paste('json file: ', fn.json), caller = caller)
  
  xj$start_year = as.numeric(xj$start_year)
  xj$end_year = min(as.numeric(xj$end_year), as.numeric(format(Sys.time(), '%Y'))-1 )
  if(is.null(xj$maxim_cell_area)){
    xj$maxim_cell_area = -999
  }else{
    xj$maxim_cell_area = as.numeric(xj$maxim_cell_area)
  }
  if(is.null(xj$aquifer_depth)){
    xj$aquifer_depth = 10 
  }else{
    xj$aquifer_depth = as.numeric(xj$aquifer_depth)
  }
  if(is.null(xj$minimum_cell_number)){
    xj$minimum_cell_number = 2000
  }else{
    xj$minimum_cell_number = as.numeric(xj$minimum_cell_number)
  }
  
  if(is.null(xj$rivdepth)){
    xj$rivdepth = 4
  }else{
    xj$rivdepth = as.numeric(xj$rivdepth)
  }
  if(is.null(xj$rivwidth)){
    xj$rivwidth = 5
  }else{
    xj$rivwidth = as.numeric(xj$rivwidth)
  }
  
  if(grepl('gldas', tolower(xj$meteorological_data) )){
    if(xj$start_year < 2000){
      writelog(paste0('WARNING:', 'GLDAS start year (', xj$start_yearm, ') must be after ', 2000),
               caller=caller)
      xj$start_year = 2000
    }
    y0 = as.numeric(format(Sys.time(), '%Y'))-1
    if(xj$end_year>y0){
      writelog(paste0('WARNING:', 'GLDAS end year (', xj$end_year, ') must be before ', y0),
               caller=caller)
      xj$end_year = y0
    }
  }
  if(grepl('cmfd', tolower(xj$meteorological_data) )){
    if(xj$start_year < 1979){
      writelog(paste0('WARNING:', 'GLDAS start year (', xj$start_yearm, ') must be after ', 1979),
               caller=caller)
      xj$start_year = 1979
    }
    y0 = 2018
    if(xj$end_year>y0){
      writelog(paste0('WARNING:', 'GLDAS end year (', xj$end_year, ') must be before ', y0),
               caller=caller)
      xj$end_year = y0
    }
  }
  if(grepl('nldas', tolower(xj$meteorological_data) )){
    if(xj$start_year < 1979){
      writelog(paste0('WARNING:', 'GLDAS start year (', xj$start_yearm, ') must be after ', 1979),
               caller=caller)
      xj$start_year = 1979
    }
    y0 = as.numeric(format(Sys.time(), '%Y'))-1
    if(xj$end_year>y0){
      writelog(paste0('WARNING:', 'GLDAS end year (', xj$end_year, ') must be before ', y0),
               caller=caller)
      xj$end_year = y0
    }
  }
  return(xj)
}
# readJSON(theTask)
readin.wbd <- function(CV){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  # ===== readfile  =====================
  CV$files$wbd0 = list.files(path = CV$dirs$userdata, pattern = glob2rx('*.shp'), full.names = TRUE, recursive = TRUE)
  checkFile(CV$files$wbd0, 'CV$files$wbd0')
  sp0 = rgdal::readOGR(dsn = CV$files$wbd0)
  sp.wbd = checkPolygon(sp0)
  if(is.null(sp.wbd)){
    email.error(CV, 'e_input_spPolygon.txt')
    task.fail()
  }else{
    sp.wbd=sp.wbd
  }
  sp.wbd = rgeos::gUnaryUnion(sp.wbd, id=FALSE, checkValidity = 2)
  sp.wbd.gcs = sp::spTransform(sp.wbd, CV$para$gcs)
  CV$para$pcs = crs.Albers(sp.wbd.gcs)
  sp.wbd.pcs = sp::spTransform(sp.wbd, CV$para$pcs)
  writelog(msg = paste('WBD file GCS: ', CV$files$wbd.gcs), caller = caller)
  writelog(msg = paste('WBD file PCS: ', CV$files$wbd.pcs), caller = caller)
  writeshape(sp.wbd.pcs, CV$files$wbd.pcs)
  writeshape(sp.wbd.gcs, CV$files$wbd.gcs)
  
  writelog(paste0('Finished'), caller=caller)
  return(CV)
}
# configure(CV, theTask)
