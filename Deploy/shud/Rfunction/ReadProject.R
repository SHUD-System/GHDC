read.prj <- function(CV){
  fn.prj = CV$deploy$config 
  crs.gcs = CV$para$gcs
  dir.predata = CV$dirs$model
  
  message('Reading project file ', fn.prj)
  getVAL <- function(x, valname, real=FALSE, defVal = NULL){
    names(x) = toupper(names(x))
    valname = toupper(valname)
    y = x[[valname]]
    if(is.null(y)) {
      # if(is.null(defVal)){
      #   message('Error: value ', valname, ' is missing')
      #   stop('GetVal')
      # }else{
      r = NULL
      # }
    }else{
      r = y
    }
    if(real){
      r = as.numeric(r)
    }
    return(r)
  }
  
  if(file.exists(fn.prj)){
    # tmp=read.table(fn.prj, header = F, row.names = 1)
    t1 = readLines(fn.prj, skipNul = TRUE)
    idx = which( grepl('^#', t1) | grepl('^ *$', t1) )
    if(length(idx)>0){
      t2 = t1[-1 * idx]
    }else{
      t2 = t1
    }
    tmp=read.table(text=t2, header = F, row.names = 1)
    xcfg = data.frame(t(tmp), stringsAsFactors = FALSE)
    # print(xcfg)
  }else{
    stop('File missing: ', fn.prj)
  }
  
  isoil = getVAL(xcfg, 'Soil', TRUE)
  ilanduse = getVAL(xcfg, 'landuse', TRUE)
  iforcing = getVAL(xcfg, 'forcing', TRUE)
  if(isoil >= 1){
    # local soil data.
    fn.soil = getVAL(xcfg, 'fn.soil')
    fn.geol = getVAL(xcfg, 'fn.geol')  
    tab.soil = getVAL(xcfg, 'tab.soil')  
    tab.geol = getVAL(xcfg, 'tab.geol')  
  }else{
    # global soil data.
    dir.soil = getVAL(xcfg, 'dir.soil')  
  }
  
  dir.ldas = getVAL(xcfg, 'dir.ldas')
  dir.out = getVAL(xcfg, 'dir.out')
  dout.forc = getVAL(xcfg, 'dout.forc')
  
  prjname=getVAL(xcfg, 'prjname')
  years=getVAL(xcfg, 'startyear', TRUE): getVAL(xcfg, 'endyear', TRUE)
  
  fsp.wbd = getVAL(xcfg, 'fsp.wbd')
  fsp.stm = getVAL(xcfg, 'fsp.stm')
  fsp.forc = getVAL(xcfg, 'fsp.forc')
  fsp.lake = getVAL(xcfg, 'fsp.lake')
  
  fr.dem = getVAL(xcfg, 'fr.dem')
  fn.landuse = getVAL(xcfg, 'fn.landuse')
  tab.landuse = getVAL(xcfg, 'tab.landuse')
  MaxArea = getVAL(xcfg, 'MaxArea', TRUE, defVal = 1) * 1e6
  NumCells = getVAL(xcfg, 'NumCells', TRUE, 1000)
  AqDepth = getVAL(xcfg, 'AqDepth', TRUE, 10)
  flowpath = getVAL(xcfg, 'flowpath', TRUE, 0)
  MinAngle = getVAL(xcfg, 'MinAngle', TRUE, 31)
  MAX_SOLVER_STEP = getVAL(xcfg, 'MAX_SOLVER_STEP', TRUE, 2)
  CRYOSPHERE = getVAL(xcfg, 'CRYOSPHERE', TRUE, 2)
  STARTDAY = getVAL(xcfg, 'STARTDAY', TRUE, 0)
  ENDDAY = getVAL(xcfg, 'ENDDAY', TRUE, 365)
  DistBuffer = getVAL(xcfg, 'DistBuffer', TRUE, CV$para$distBuff)
  
  if(is.null(CV$para$pcs)){
    crs.fn <- getVAL(xcfg, 'fsp.crs')
    if( !is.null(crs.fn)){
      if(file.exists(crs.fn)){
        message('CRS file:', crs.fn)
        crs.pcs <- raster::crs(rgdal::readOGR(crs.fn))
      }else{
        message('CRS file is missing. So Albers projection is used')
        crs.pcs <-  rSHUD::crs.Albers(rgdal::readOGR(fsp.wbd))
        message(crs.pcs)
      }
    }else{
      message('CRS file is missing. So Albers projection is used')
      crs.pcs <-  rSHUD::crs.Albers(rgdal::readOGR(fsp.wbd))
      message(crs.pcs)
    }
  }else{
    crs.pcs = CV$para$pcs
  }
  
  # ===============================
  tol.wb = getVAL(xcfg, 'tol.wb', TRUE, NULL)
  tol.rivlen = getVAL(xcfg, 'tol.rivlen', TRUE, NULL)
  
  RivWidth = getVAL(xcfg, 'RivWidth', TRUE, NULL)
  RivDepth = getVAL(xcfg, 'RivDepth', TRUE, NULL)
  
  QuickMode = (getVAL(xcfg, 'QuickMode') > 0) 
  
  dir.png =file.path(dir.out, 'Figure')
  dir.modelin <- file.path(dir.out, 'input', prjname)
  dir.modelout <- file.path(dir.out, 'output', paste0(prjname, '.out') )
  
  dir.forc=getVAL(xcfg, 'fc.out',  real=FALSE, file.path(dir.out, 'forcing'))
  dirlist = list(out=dir.out, 
                 fig=dir.png, 
                 predata=dir.predata, 
                 modelin=dir.modelin, 
                 modelout=dir.modelout,
                 forc = dir.forc)
  if(isoil < 1){
    dirlist = c(dirlist, soil=dir.soil)
  }
  tmp=lapply(dirlist, dir.create, showWarnings=F, recursive=T)
  
  # Some Constant values in the working environments.

  
  cfg = list('prjname' = prjname,
             'years' = years,
             'crs.pcs'=crs.pcs,
             'crs.gcs'=crs.gcs,
             'fsp.wbd'=fsp.wbd,
             'fsp.stm'=fsp.stm,
             'fsp.lake' = fsp.lake,
             'fr.dem'=fr.dem,
             
             'dir'=dirlist,
             'iforcing' = iforcing,
             'fsp.forc'=fsp.forc,
             'dout.forctsd' = dout.forc,
             
             
             'ilanduse' = ilanduse,
             'fn.landuse'=fn.landuse,
             'tab.landuse'=tab.landuse,
             
             'isoil' = isoil,
             
             'para' = list(
               'NumCells'=NumCells,
               'AqDepth'=AqDepth,
               'MaxArea'=MaxArea,
               'QuickMode'=QuickMode,
               'tol.wb'=tol.wb,
               'tol.rivlen'=tol.rivlen,
               'RivWidth'=RivWidth,
               'RivDepth'=RivDepth,
               'DistBuffer'=DistBuffer,
               'flowpath' = flowpath,
               'MinAngle'=MinAngle,
               'MAX_SOLVER_STEP' = MAX_SOLVER_STEP,
               'CRYOSPHERE'=CRYOSPHERE,
               'STARTDAY'=STARTDAY,
               'ENDDAY'=ENDDAY
             )
  )
  if(isoil>=1){
    cfg = c(cfg, 
            'isoil' = isoil,
            'fn.soil'=fn.soil,
            'fn.geol'=fn.geol,
            'tab.soil'=tab.soil,
            'tab.geol'=tab.geol)
  }else{
    # void
  }
  if(iforcing < 1){
    # res = getVAL(xcfg, 'res', real = TRUE)
    cfg = c(cfg,   dir.ldas = dir.ldas)
  }else{
    # void
  }
  return(cfg)
}

# 
# xfg <- read.prj(CV)