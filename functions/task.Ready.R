#' \code{task.ready}
#' Get Ready for a task.
#' 1. generate the CV list.
#' 2. Unzip zip file from a user.
#' 3. read the wbd shapefile, and validate the wbd format.
#' 4. generate the PCS/GCS for the project.
#' 5. calculate the area for the wbd in PCS.
#' 6. calculate the buffer distance, and generate the buffer zone.
#' 
#' @param theTask the Dir where the wbd and json saved.
#' 
task.ready <- function(CV){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  
  # 2. Unzip zip file from a user.
  fn.zip = list.files(path = theTask, pattern = glob2rx("*.zip"), full.names = TRUE)
  checkFile(fn.zip, 'fn.zip')
  utils::unzip(zipfile = fn.zip, exdir = CV$dirs$userdata)
  writelog(msg = paste('zip file: ', fn.zip), caller = caller)
  
  #  3. read the wbd shapefile, and validate the wbd format.
  CV$etv$wbd0 = list.files(path = CV$dirs$userdata, 
                           pattern = glob2rx('*.shp'), 
                           full.names = TRUE, recursive = TRUE)
  writemessage('Checking the boundary file form user... ', caller = caller, CV$task.log)
  tmp = checkFile(CV$etv$wbd0, 'CV$etv$wbd0')
  if(!tmp){
    writemessage('ERROR: Failed to open the boundary shapefile!!!', caller = caller, CV$task.log)
    email.error(CV, 'e_input_openwbdFail.txt')
    task.fail(CV)
  }
  
  sp0 = try(rgdal::readOGR(dsn = CV$etv$wbd0))
  if(grepl('try-error', class(sp0))){
    writemessage('ERROR: Failed to open the boundary shapefile!!!', caller = caller, CV$task.log)
    email.error(CV, 'e_input_openwbdFail.txt')
    task.fail(CV)
  }
  tmp = try(crs(sp0))
  if(grepl('try-error', class(tmp))){
    writemessage('ERROR: projection info is missing !!!', caller = caller, CV$task.log)
    email.error(CV, 'e_input_openwbdFail.txt')
    task.fail(CV)
  }
  
  writemessage('Checking the boundary file whether SpatialPolygon... ', caller = caller, CV$task.log)
  sp1 = checkPolygon(sp0)
  if(is.null(sp1)){
    writemessage('ERROR: The file uploaded is not valid SpatialPolygon!!!', caller = caller, CV$task.log)
    email.error(CV, 'e_input_spPolygon.txt')
    task.fail()
  }else{
    sp1=sp1
  }
  
  #  4. generate the PCS/GCS for the project. =============
  # writelog(msg = paste('Zero buffering boundary. '), caller = caller)
  sp2 = rgeos::gBuffer(sp1, byid = FALSE, width = 0)
  writelog(msg = paste('Union the boundary. '), caller = caller)
  sp3 = rgeos::gUnaryUnion(sp2, id=FALSE, checkValidity = 2)
  writelog(msg = paste('Removing holes in boundary. '), caller = caller)
  sp.wbd = rSHUD::removeholes(sp3)
  
  sp.wbd.gcs = sp::spTransform(sp.wbd, CV$para$gcs)
  writelog(msg = paste('WBD in GCS: ', CV$etv$buf.gcs), caller = caller)
  writeshape(sp.wbd.gcs, CV$etv$wbd.gcs)
  
  CV$para$pcs = crs.Albers(sp.wbd.gcs)
  sp.wbd.pcs = sp::spTransform(sp.wbd, CV$para$pcs)
  writelog(msg = paste('WBD in PCS: ', CV$etv$wbd.pcs), caller = caller)
  writeshape(sp.wbd.pcs, CV$etv$wbd.pcs)
  
  # 5. calculate the area for the wbd in PCS. =============
  CV$para$Area = rgeos::gArea(sp.wbd.pcs, byid=FALSE)
  writemessage(paste0('Area of the target area is ', round(CV$para$Area, 2),' m2.'),  caller = caller, CV$task.log)
  if(CV$para$Area /1e6 > 90E4){
    # Area > 1,000,000 km2.
    writemessage('WARNING: The area of target area is larger than 900,000 km2. ETV ONLY is assigned !!!', 
                 caller = caller, CV$task.log)
    CV$para$dataonly = TRUE
  }
  
  distBuff = round(sqrt(CV$para$Area)/20)
  CV$para$distBuff = min(c(4000, max(c(10, distBuff), na.rm = TRUE))) # 10~3000m
  writemessage(paste0('Buffer distance of the project is ', CV$para$distBuff,' m.'),  caller = caller, CV$task.log)
  
  if(CV$json$maxim_cell_area <= 0){
    # if the maxim_cell_area is not defined.
    CV$json$maxim_cell_area = round( CV$para$Area / CV$json$minimum_cell_number, 0) * 1e-6
  }
  writemessage(paste0('maxim_cell_area of the project is ', CV$json$maxim_cell_area,' km2.'),  caller = caller, CV$task.log)
  
  sp.buf.pcs = rgeos::gBuffer(sp.wbd.pcs, byid=FALSE, width = CV$para$distBuff  )
  writeshape(sp.buf.pcs, CV$etv$buf.pcs)
  
  sp.buf.gcs = sp::spTransform(sp.buf.pcs, CRSobj = CV$para$gcs)
  writeshape(sp.buf.gcs, CV$etv$buf.gcs)
  
  #========= figure =============
  fn.fig = file.path(CV$dirs$fig, paste0('wbd_buf.png'))
  writelog(paste0('Writing figure: ', fn.fig), caller=caller)
  png(filename = fn.fig, height = 7, width = 9, units = 'in', res = 300)
  par(mar=c(1,1,1,1))
  raster::plot(sp.buf.gcs, axes=TRUE, border='red');
  raster::plot(sp.wbd.gcs, add=TRUE, border='blue');  grid()
  mtext(side = 3, paste0('Buffer Distance = ', round(CV$para$distBuff, 4)) )
  dev.off()
  
  #========= end =============
  writelog(paste0('Finished'), caller=caller)
  return(CV)
}
# 
# iflag=try(task.ready(CV),TRUE)
# iflag$para$distBuff
