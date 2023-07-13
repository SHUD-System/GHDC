
#' ===== configure =====================
#' configure function, generate the CV 
#' 
configure <- function(CV, theTask){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  
  x.json = readJSON(theTask)
  # ===== dirs =====================
  keyid = paste(c(
    # basename(dirname(theTask)),
    format(file.info(theTask)$atime, '%Y%m%d'),
    x.json$email,
    x.json$project_name),
    collapse = '_')
  
  dir.out = file.path(CV$serv$DIR.WORKING, 'rProcessing')
  dir.prj = file.path(dir.out, keyid)
  
  ## ===== BACK UP FILES.  =====================
  CV$dirs = list(
    etv = file.path(dir.prj, 'ETV'),
    model = file.path(dir.prj, 'Modeling'),
    copyto = file.path(dir.prj),
    userdata = file.path(dir.prj, 'UserData'),
    
    download = normalizePath( CV$serv$DIR.ZIPOUT ),
    
    deploy = normalizePath(  file.path('./Deploy', x.json$model)),
    
    proc = dir.out,
    prj = dir.prj,
    fig = file.path(dir.prj, 'ETV', 'Figure'),
    pcs = file.path(dir.prj, 'ETV','PCS'),
    tsd = file.path(dir.prj, 'ETV','TSD'),
    
    # datagcs = file.path(dir.prj, 'ETV','GCS'), 
    taskin = theTask,
    temp = file.path(CV$serv$DIR.WORKING, 'rTemp', keyid),
    
    log = file.path(CV$serv$DIR.WORKING, 'rlog'),
    bak = file.path(CV$serv$DIR.WORKING, 'backup', 'data')
  )
  file.copy(from = theTask, to=CV$dirs$bak, recursive = TRUE)
  if(file.exists(CV$dirs$userdata)){  unlink(CV$dirs$userdata, recursive = FALSE, force = TRUE)  }
  
  tmp = lapply(CV$dirs, dir.create, showWarnings = FALSE, recursive = TRUE)
  CV$json = x.json
  CV$keyid = keyid
  CV$task.log = file.path(dir.prj, 'task_log.txt')
  write('', file = CV$task.log)
  # ETV, that for data request only on GHDC
  CV$etv=list(
    # GCS
    wbd.gcs=file.path(CV$dirs$etv, 'wbd.shp'),
    buf.gcs=file.path(CV$dirs$etv, 'buf.shp'),
    stm.gcs=file.path(CV$dirs$etv, 'stm.shp'),
    # PCS
    wbd.pcs=file.path(CV$dirs$pcs, 'wbd.shp'),
    buf.pcs=file.path(CV$dirs$pcs, 'buf.shp'),
    stm.pcs=file.path(CV$dirs$pcs, 'stm.shp'),
    # Water Delineation
    stm_dem=file.path(CV$dirs$etv, 'stm_dem.shp'),
    wbd_dem=file.path(CV$dirs$etv, 'wbd_dem.shp'), 
    outlets = file.path(CV$dirs$etv, 'outlets.shp'),
    
    wbd0='',
    dem=file.path(CV$dirs$etv, 'dem.tif'),
    
    soil = file.path(CV$dirs$etv, 'soil.tif'),
    soil.att = file.path(CV$dirs$etv, 'soil.csv'),
    
    geol = file.path(CV$dirs$etv, 'geol.tif'),
    geol.att = file.path(CV$dirs$etv, 'geol.csv'),
    
    landuse = file.path(CV$dirs$etv, 'landuse.tif'),
    landuse.att = file.path(CV$dirs$etv, 'landuse.csv'),
    landuse.lai = file.path(CV$dirs$etv, 'lai.csv'),
    
    # meltfactor = file.path(CV$dirs$etv, 'meltfactor.csv'),
      
    ldas = file.path(CV$dirs$etv, 'ldas.tif'),
    ldas.att = file.path(CV$dirs$etv, 'ldas.csv'),
    
    geol_depth = file.path(CV$dirs$etv, 'geol_depth.tif'),
    dir.tsd = file.path(CV$dirs$tsd)
  )
  
  ls.soil = list(
    hwsd = file.path(CV$serv$PATH2SD, 'Soil/HWSD/HWSD_RASTER/hwsd.bil'),
    soilgrid = file.path(CV$serv$PATH2SD, '???'),
    att = list(
      hwsd = file.path(CV$serv$PATH2SD, 'Soil/HWSD/HWSD_RASTER/hwsd.dbf'),
      soilgrid = file.path(CV$serv$PATH2SD, '???')
    )
  )
  ls.geol = list(
    hwsd = file.path(CV$serv$PATH2SD, 'Soil/HWSD/HWSD_RASTER/hwsd.bil'),
    soilgrid = file.path(CV$serv$PATH2SD, '???'),
    att = list(
      hwsd = file.path(CV$serv$PATH2SD, 'Soil/HWSD/HWSD_RASTER/hwsd.dbf'),
      soilgrid = file.path(CV$serv$PATH2SD, '???')
    )
  )
  ls.landuse = list(
    'mglc' = file.path(CV$serv$PATH2SD, 'Landuse/USGS_LCI/LCType.tif'),
    att = list(
      mglc = file.path(CV$serv$PATH2SD, 'Landuse/USGS_LCI/attributes.csv')
    )
  )
  ls.dem = list(
    aster=NULL,
    merit=NULL,
    srtm=NULL
  )
  ls.ldas = list(
    gldas = file.path(CV$serv$PATH2FD, 'GESDISC/data/GLDAS/GLDAS_NOAH025_3H.2.1.tif'),
    nldas = file.path(CV$serv$PATH2FD, 'GESDISC/data/NLDAS/NLDAS_FORA0125_H.002.tif'),
    cmfd = file.path(CV$serv$PATH2FD, 'CMFD/CMFD_grid.tif'),
    fldas = file.path(CV$serv$PATH2FD, 'CMFD/Data_forcing_03hr_010deg'), # ????
    cldas = file.path(CV$serv$PATH2FD, 'CMFD/Data_forcing_03hr_010deg'), # ????
    att=list(
      gldas = file.path(CV$serv$PATH2FD, 'GESDISC/data/GLDAS/GLDAS_NOAH025_3H.2.1.csv'),
      nldas = file.path(CV$serv$PATH2FD, 'GESDISC/data/NLDAS/NLDAS_FORA0125_H.002.csv'),
      cmfd = file.path(CV$serv$PATH2FD, 'CMFD/CMFD_grid.csv'),
      fldas = file.path(CV$serv$PATH2FD, 'CMFD/Data_forcing_03hr_010deg'), # ????
      cldas = file.path(CV$serv$PATH2FD, 'CMFD/Data_forcing_03hr_010deg') # ????
    ),
    dir=list(
      gldas = file.path(CV$serv$PATH2FD, 'GLDAS_NOAH025_3H.2.1/RDS'),
      nldas = file.path(CV$serv$PATH2FD, 'NLDAS_FORA0125_H/RDS'),
      cmfd = file.path(CV$serv$PATH2FD, 'CMFD/CMFD_RDS'),
      fldas = file.path(CV$serv$PATH2FD, 'CMFD/Data_forcing_03hr_010deg'), # ????
      cldas = file.path(CV$serv$PATH2FD, 'CMFD/Data_forcing_03hr_010deg') # ????
    )
  )
  
  # ===== files, Original data =====================
  CV$files = list(
    'zipout' = file.path(CV$serv$DIR.ZIPOUT, paste0(keyid, '.zip')),
    # 'link_zip' = paste0(CV$serv$LINK.DL, paste0(keyid, '.zip')),
    'link_zip' = paste0(CV$serv$LINK.DL, paste0(keyid)),
    'email' = file.path(CV$serv$DIR.EMAIL, paste0(keyid, '.md')), 
    
    'dem' = NULL,
    
    'soil' = ls.soil[[CV$json$soil]],
    'soil.att' = ls.soil$att[[CV$json$soil]],
    
    'geol' = ls.geol[[CV$json$geol]],
    'geol.att' = ls.geol$att[[CV$json$geol]],
    
    'landuse' = ls.landuse[[CV$json$landuse]],
    'landuse.att' = ls.landuse$att[[CV$json$landuse]],
    
    'ldas' = ls.ldas[[tolower(x.json$meteorological_data)]],
    'ldas.att' = ls.ldas$att[[tolower(x.json$meteorological_data)]],
    'ldas.dir' = ls.ldas$dir[[tolower(x.json$meteorological_data)]]
  )
  CV$deploy = list(
    'config' = file.path(CV$dirs$model, 'deployConfig.txt'),
    crs.gcs = CV$para$gcs,
    crs.pcs = CV$para$pcs,
    ldas.att = CV$etv$ldas.att
  )
  
  # copy file to 
  writelog(msg=paste('Copy model/citation code ... '), caller = caller)
  file.copy(from = 'StaticFiles/', to= CV$dirs$copyto, recursive = TRUE)
  writemessage('Task is configured. ', caller = caller, CV$task.log)
  writelog(paste0('Finished'), caller=caller)
  return(CV)
}

# CV=configure(CV, theTask)


