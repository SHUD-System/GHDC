AutoSHUD_Step3 <- function(CV){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  writemessage(paste0('Running the AutoSHUD_Step3(CV) '),  caller = caller, CV$task.log)
  prefix = 'AutoSHUD_S3'
  xfg = CV$deploy
  if(is.null(xfg)){
    writelog(msg="Error: xfg is missing in CV", caller = caller)
  }
  fin <- shud.filein(xfg$prjname, inpath = xfg$dir$modelin, outpath= xfg$dir$modelout)
  wbd=readOGR(xfg$pd.pcs$wbd)
  dem=raster(xfg$pd.pcs$dem)
  buf.g = readOGR(xfg$pd.pcs$wbd.buf)
  
  # ==============================================
  AA1=gArea(wbd)
  a.max = min(AA1/xfg$para$NumCells, xfg$para$MaxArea)
  NCELL.MAX = round(AA1 / a.max * 1.5)
  q.min = xfg$para$MinAngle
  tol.wb = xfg$para$tol.wb
  tol.rivlen = xfg$para$tol.rivlen
  years = xfg$years
  if(is.null(tol.wb) | is.infinite(tol.wb)){ tol.wb = min(sqrt(a.max), 3000) }
  if(is.null(tol.rivlen) | is.infinite(tol.rivlen) ){ tol.rivlen = min(sqrt(a.max), 5000) }
  # 
  # tol.wb = min(sqrt(a.max)/2, 300)
  # tol.rivlen = min(sqrt(a.max)*3, 5000)
  
  bm.para = data.frame(a.max/1e6,  tol.wb, tol.rivlen)
  names(bm.para)=c('MaxArea_km2', 'tol.wb', 'MaxRivLen')
  print(bm.para)
  
  ny=length(years)
  nday = 365*ny + round(ny/4) - 1
  
  writemessage(paste0('Area = ', AA1),  caller = caller, CV$task.log)
  writemessage(paste0('Max cell area = ', a.max),  caller = caller, CV$task.log)
  writemessage(paste0('NCELL.MAX = ', NCELL.MAX),  caller = caller, CV$task.log)
  writemessage(paste0('Minimum Angle = ', q.min),  caller = caller, CV$task.log)
  writemessage(paste0('Simplity tolerance for boundary = ', tol.wb),  caller = caller, CV$task.log)
  writemessage(paste0('Simplity tolerance for river length = ', tol.rivlen),  caller = caller, CV$task.log)
  writemessage(paste0('Years ', min(years), '~', max(years)),  caller = caller, CV$task.log)
  writemessage(paste0('No of Days = ', nday),  caller = caller, CV$task.log)
  
  #' ==============================================
  #' BUFFER
  wb.dis = rgeos::gUnionCascaded(wbd)
  wb.s1 = rgeos::gSimplify(wb.dis, tol=tol.wb, topologyPreserve = T)
  # wb.s2 = sp.simplifyLen(wb.s1, tol.wb)
  wb.s2 = gSimplify(wb.s1, tol = tol.wb)
  wb.simp = wb.s2
  
  #' ====================================================
  #' 
  if(xfg$LAKEON){
    source(file.path(CV$dirs$deploy, 'SubScript/Sub3_lake.R')  )
  }else{
    sp.lake=NULL
  }
  
  tri = shud.triangle(wb=wb.simp,q=q.min, a=a.max, S=NCELL.MAX)
  
  # =========Mesh generation=====================================
  pm = shud.mesh(tri, dem=dem, AqDepth = xfg$para$AqDepth)
  spm = sp.mesh2Shape(pm, crs = crs(wbd))
  writeshape(spm, crs(wbd), file=file.path(fin['inpath'], 'gis', 'domain'))
  print(nrow(spm@data))
  ia=getArea(pm)
  nCells = length(spm)
  writemessage(paste0('No of tirangular cells = ', nCells),  caller = caller, CV$task.log)
  writemessage(paste0('Mean area of tirangular cells = ', mean(ia)),  caller = caller, CV$task.log)
  
  # ==============================================
  riv0=readOGR(xfg$pd.pcs$stm)
  if(xfg$para$flowpath){
    # debug(sp.RiverPath)
    riv1=sp.RiverPath(riv0)$sp  #Build the River Path --- Dissolve the lines.
    riv1=riv0
    riv2=rmDuplicatedLines(riv1)
  }else{
    riv1 = riv0
    riv2=riv1
  }
  lens=gLength(riv2, byid=TRUE)
  summary(lens)
  spr = sp.CutSptialLines(sl=riv2, tol=tol.rivlen)
  # writeshape(spr, file=file.path(xfg$dir$predata, 'spr'))
  go.png <- function(){
    png(file = file.path(xfg$dir$fig, paste0(prefix, '.1_datain.png')), height=7, width=7, res=300, unit='in')
    par(mar=c(2, 2, 1, 1) )
    plot(dem);  
    plot(wbd, add=T, border='red', lwd=1); plot(riv2, add=T, lwd=1, col='blue');
    mtext(line=-1,side=3, cex=1,'DEM-WBD-Riv');     grid()
    dev.off()
  };   go.png()
  # ======FORCING FILE======================
  sp.meteoSite = rgdal::readOGR(xfg$pd.pcs$meteo)
  sp.meteoSite@data$ID = paste0(CV$json$meteorological_data, '_',  sp.meteoSite@data$FILENAME)
  sp.forc = rSHUD::ForcingCoverage(sp.meteoSite = sp.meteoSite, 
                                   pcs=xfg$crs.pcs, gcs=xfg$crs.gcs, 
                                   dem=dem, wbd=wbd)
  # head(sp.forc@data)
  write.forc(sp.forc@data, path = '../ETV/TSD',
             startdate = paste0(min(years), '0101'), 
             file=fin['md.forc'])
  
  
  go.png <-function(){
    png(file.path(xfg$dir$fig, paste0(prefix, '.2_buf_riv_forc.png')), height=7, width=7, units = 'in', res=300)
    par(mar=c(2, 2, 1, 1) )
    plot(dem);grid()
    plot(buf.g, add=T, axes=T, lwd=1)
    plot(wbd, add=T, border='red', lwd=1)
    plot(riv2, add=T, col='blue', lwd=1)
    plot(sp.meteoSite, add=T, col='darkgreen', cex=0.3)
    plot(sp.forc, add=T, lwd=0.5, lty=2)
    grid();     mtext(line=-1,side=3, cex=1, 'DEM-WBD-Riv-Meteo');     dev.off()
  }; go.png();
  
  # xfg$dir$fig = file.path(xfg$dir$modelin, 'fig')
  gisout = file.path(xfg$dir$modelin, 'gis')
  dir.create(xfg$dir$modelin, showWarnings = F, recursive = T)
  dir.create(xfg$dir$fig, showWarnings = F, recursive = T)
  dir.create(gisout, showWarnings = F, recursive = T)
  
  go.png <-function(){
    png(file.path(xfg$dir$fig, paste0(prefix, '.3_domain.png')), height=7, width=7, units = 'in', res=300)
    par(mar=c(2, 2, 1, 1) )
    plot_sp(spm, 'Zmax', axes=TRUE, lwd=0.3, border='gray')
    plot(spr, add=T, col=rgb(0.8, 0.8, 0.8, alpha = 0.5), lwd=2)
    plot(spr, add=T, col='blue', lwd=1)
    mtext(line=-1,side=3, cex=1, paste0('SHUD triangular mesh (Ncell = ', nCells, ')' ))
    grid();     title('');     dev.off()
  }; go.png()
  
  # ======LANDUSE======================
  rlc.idx = raster(xfg$pd.pcs$lu.idx)
  para.lc = read.df(xfg$tab.landuse)[[1]]
  para.lc = para.lc[, -1*ncol(para.lc)]
  para.lc[, 1] = 1:nrow(para.lc)
  
  # ====== generate  .att ======================
  r.soil.idx = raster(xfg$pd.pcs$soil.idx)
  r.geol.idx = raster(xfg$pd.pcs$geol.idx)
  
  pa=shud.att(tri,  
              r.soil = r.soil.idx, r.geol = r.geol.idx, r.lc = rlc.idx, 
              r.forc = sp.forc, r.BC = 0, sp.lake = sp.lake)
  
  fx <- function(x){ x[is.na(x)] = median(x, na.rm = TRUE); return(x) }
  pa = apply(pa, 2, fx)
  
  spm@data = cbind(spm@data, pa)
  writeshape(spm, crs(wbd), file=file.path(fin['inpath'], 'gis', 'domain'))
  
  # ====== generate  .riv ======================
  pr=shud.river(spr, dem)
  pr@rivertype$Width= pr@rivertype$Width * xfg$para$RivWidth
  pr@rivertype$Depth= xfg$para$RivDepth + (1:nrow(pr@rivertype) - 1 )*0.5
  pr@rivertype$BankSlope = 1
  spr@data = data.frame(pr@river, pr@rivertype[pr@river$Type,])
  writeshape(spr, crs(wbd), file=file.path(gisout, 'river'))
  
  # Cut the rivers with triangles
  sp.seg=sp.RiverSeg(spm, spr)
  writeshape(sp.seg, crs(wbd), file=file.path(gisout, 'seg'))
  
  # Generate the River segments table
  prs = shud.rivseg(sp.seg)
  
  # Generate initial condition
  # debug(shud.ic)
  if(xfg$LAKEON){
    lakestage = 30;
  }else{
    lakestage = NULL
  }
  pic = shud.ic(ncell = nrow(pm@mesh), nriv = nrow(pr@river), lakestage=lakestage,
                AqD = xfg$para$AqDepth)
  
  # Generate shapefile of mesh domain
  cfg.para = shud.para(nday = nday)
  cfg.para['INIT_MODE']=3
  # calibration
  cfg.calib = shud.calib()
  
  #soil/geol/landcover
  if(xfg$para$QuickMode){
    message('\n !!! QUICK MODE in SOIL/GEOL parameters!!!\n')
    para.soil = PTF.soil()
    para.geol = PTF.geol()
  }else{
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # !!!!! the column of soil/geol file must be 5 colume: ID   SILT CLAY     OM    BD
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    asoil = as.matrix(read.df(xfg$pd.att$soil)[[1]])[, -1]
    ageol = as.matrix(read.df(xfg$pd.att$geol)[[1]])[, -1]
    asoil=rbind(asoil)
    ageol=rbind(ageol)
    asoil[asoil[, 3] > 5, 3] = NA  # Organic Matter must less 5.
    ageol[ageol[, 3] > 5, 3] = NA
    fx <- function(x){ x[is.na(x) | is.nan(x)] = mean(x, na.rm=TRUE); return(x) }
    asoil = rbind(apply(asoil, 2, fx))
    ageol = rbind(apply(ageol, 2, fx))
    para.soil = data.frame(PTF.soil(asoil))
    para.geol = data.frame(PTF.geol(ageol))
    # plot(ageol[, 3], para.geol[, 2], xlab='OM', ylab='KsatV(m_d)', log='x')
    fun.plot <- function(dat, ylab=''){
      nd = length(dat)
      x = (1:nd)/nd
      y=sort(dat)
      plot(x, y, xlab='', ylab='', pch=20, col='blue',type='b');
      mtext(side = 1, line=2, 'Exceedance'); 
      mtext(side = 2, line=2, ylab);
      grid()
    }
    go.png <-function(){
      png(file.path(xfg$dir$fig, paste0(prefix, '.4_soilgeol_para.png')), height=7, width=7, units = 'in', res=300)
      par(mar=c(3, 3, 1, 1), mfrow=c(2,2) )
      fun.plot(para.soil$KsatV.m_d., ylab='Soil KsatV (m/day)')
      fun.plot(para.soil$Beta, ylab='Soil Beta (-)')
      fun.plot(para.geol$KsatV.m_d., ylab='Geol KsatV (m/day)')
      fun.plot(para.geol$ThetaS.m3_m3., ylab='Geol Porosity (m3/m3)')
      dev.off()
    }; go.png()
  }
  
  # ====== LAI ======================
  ts.lai = read.tsd(CV$etv$landuse.lai)[[1]]
  write.tsd(ts.lai, file = fin['md.lai'])
  
  #MeltFactor
  # mf =read.tsd(CV$etv$meltfactor)
  mf = MeltFactor(years = years)
  write.tsd(mf, file=fin['md.mf'])
  
  # write SHUD input files.
  write.mesh( pm, file = fin['md.mesh'])
  write.riv(pr, file=fin['md.riv'])
  write.ic(pic, file=fin['md.ic'])
  
  write.df(pa, file=fin['md.att'])
  write.df(prs, file=fin['md.rivseg'])
  write.df(para.lc, file=fin['md.lc'])
  write.df(para.soil, file=fin['md.soil'])
  write.df(para.geol, file=fin['md.geol'])
  
  cfg.para$START=xfg$para$STARTDAY
  cfg.para$END=xfg$para$ENDDAY
  cfg.para$CRYOSPHERE = xfg$para$CRYOSPHERE
  cfg.para$MAX_SOLVER_STEP = xfg$para$MAX_SOLVER_STEP
  
  write.config(cfg.para, fin['md.para'])
  write.config(cfg.calib, fin['md.calib'])
  
  if( any( is.na(pm@mesh) ) |  any( is.na(pm@point) ) ){
    message('NA in .SP.MESH file')
  }
  if(any( is.na(pr@river)) ){
    message('NA in .SP.RIV file')
  }
  if(any( is.na(pa)) ){
    message('NA in .SP.ATT file')
  }
  pp = shud.env(prjname = xfg$prjname, inpath = xfg$dir$modelin, outpath = xfg$dir$modelout)
  ia= getArea()
  ma = MeshAtt()
  go.plot <- function(){
    png(file.path(xfg$dir$fig, paste0(prefix, '.5_HistArea.png')), height=7, width=7, units = 'in', res=300)
    par(mfrow=c(2, 1))
    hist(ia/1e6, xlab='', ylab='', main='Histgram of triangle area', freq=TRUE); 
    mtext(side=1, line=2,'Area (km2)')
    mtext(side=2, line=2,'Count')
    hist(sqrt(ia)/1e3, xlab='', ylab='', main='Histgram of equivalent resolution', freq=TRUE); 
    mtext(side=1, line=2,'Length (km)')
    mtext(side=2, line=2,'Count')
    par(mfrow=c(1, 1))
    dev.off()
  }; go.plot()
  
  # ModelInfo()
  message('Ncell = ', nrow(pm@mesh))
  message('Nriv = ', nrow(pr@river))
  
  writelog(paste0('Finished'), caller=caller)
  return(nCells)
}
# AutoSHUD_Step3(CV)
