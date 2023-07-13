#' ===============================================================
#' Author: Lele Shu <shulele@lzb.ac.cn>
#' Date: 2023.02.03
#' Function: The AutoSHUD function
#' 
#' ===============================================================
#' 
wdir = file.path(CV$dirs$deploy)
source(file.path(CV$dirs$deploy, 'Step1_RawDataProcessng.R'))
source(file.path(CV$dirs$deploy, 'Step2_DataSubset.R'))
source(file.path(CV$dirs$deploy, 'Step3_BuidModel.R'))
source(file.path(CV$dirs$deploy, 'Rfunction/ReadProject.R'))

GetReady.AutoSHUD <- function(CV){
  xfg <- read.prj(CV)
  
  if( !is.null(xfg$fsp.lake) ){
    LAKEON = TRUE
  }else{
    LAKEON = FALSE
  }
  
  pre.sp = list(
    dem = 'dem.tif',
    wbd = 'wbd.shp',
    wbd.buf = 'wbd_buf.shp',
    stm = 'stm.shp',
    lake = 'lake.shp',
    
    soil.r = 'soil.tif',
    soil.idx = 'soil_idx.tif',
    
    geol.r = 'geol.tif',
    geol.idx = 'geol_idx.tif',
    
    soil.v = 'soil.shp',
    geol.v = 'geol.shp',
    
    lu.r = 'landuse.tif',
    lu.idx = 'landuse_idx.tif',
    lu.v = 'landuse.shp',
    
    meteo = 'meteo.shp',
    meteoCov = 'meteoCov.shp')
  dir.pd.pcs =  file.path(xfg$dir$predata,'PCS')
  dir.pd.gcs =  file.path(xfg$dir$predata,'GCS')
  
  dir.create(dir.pd.pcs, showWarnings = FALSE, recursive = TRUE)
  dir.create(dir.pd.gcs, showWarnings = FALSE, recursive = TRUE)
  
  pd.pcs = lapply(1:length(pre.sp), function(x){ file.path(dir.pd.pcs, pre.sp[[x]])} )
  names(pd.pcs) = names(pre.sp)
  
  pd.gcs = lapply(1:length(pre.sp), function(x){ file.path(dir.pd.gcs, pre.sp[[x]])} )
  names(pd.gcs) = names(pre.sp)
  
  pd.att <- list(
    geol = file.path(xfg$dir$predata,'GEOL.csv'),
    soil = file.path(xfg$dir$predata,'SOIL.csv'),
    landuse = file.path(xfg$dir$predata,'LANDUSE.csv') 
  )
  
  xfg$LAKEON=LAKEON
  xfg$pd.att=pd.att
  xfg$pd.pcs=pd.pcs
  xfg$pd.gcs=pd.gcs
  
  cv = CV
  cv$deploy = utils::modifyList(CV$deploy, xfg) 
  return(cv)
}

