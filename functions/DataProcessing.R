
# ======= dataSoilGeol ==========
dataSoilGeol <- function(CV, buf){
  buf=readOGR(CV$files$buf.gcs)
  if( grepl('HWSD', toupper(CV$json$soil_data))){
    
    fnr = file.path(CV$serv$PATH2SD, 'Soil/HWSD/HWSD_RASTER/hwsd.bil')
    r=raster(fnr)
    rsub = crop(r, buf)
    soil.att = '/Volumes/SpatialData/World/Soil/HWSD/HWSD_RASTER/hwsd.csv'
  }
}

# ======= dataLanduse ==========
dataLanduse <- function(CV, buf){
  
}
# ======= dataLDSACoverage ==========
dataLDSACoverage <- function(CV, buf){
  
}