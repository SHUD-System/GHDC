#' ===============================================================
#' Author: Lele Shu <shulele@lzb.ac.cn>
#' Date: 2023.05.05
#' Function: The core function of the GHDC
#' ===============================================================

rm(list=ls())
setwd('/home/wwwroot/r')
library(rjson)
library(utils)
library(rgdal)
library(raster)
library(rSHUD)
require(ncdf4)
library(sp)
library(rgeos)
library(lattice)
library(ggplot2)
# library(GGally)
library(hydroTSM)
library(hydroGOF)
library(whitebox)
library(terra)
library(sf)
debug=TRUE
debug=FALSE

# this.dir <- dirname(parent.frame(2)$ofile)
# setwd(this.dir)
go.source <- function(){
  cdir  = 'functions'  
  fns = list.files(cdir, pattern = glob2rx('*.R'), full.names = TRUE, recursive = TRUE)
  nf=length(fns)
  for(i in 1:nf){
    fn = fns[i]
    message(i, '/', nf, '\t', fn)
    source(fn)
  }
}; 
go.source()

CV=list()
# system('cp -r bak/* data/')
CV$serv = load.conf(file = 'script/service.cfg.txt')
CV$para = list(projname = '', Area = -999, distBuff = -999, NumCellMin = -999,
               dataonly = FALSE, # prepare data only. 
               gcs = sp::CRS('+init=epsg:4326'), pcs='')
caller = 'GO.R'

dir.out <- file.path(CV$serv$DIR.WORKING, 'rProcessing')
dir.log <- file.path(CV$serv$DIR.WORKING, 'rlog')

tmp = lapply(list(dir.out, dir.log), dir.create,  showWarnings = FALSE, recursive = TRUE)

# LOGFILE = file.path(dir.log, paste0(format(Sys.time(), '%Y%m%d%H%M%S' ), '.log'))
LOGFILE = file.path(dir.log, paste0(format(Sys.time(), '%Y%m%d' ), '.log'))
assign('LOGFILE',LOGFILE, envir = .GlobalEnv)

if(!file.exists(LOGFILE)){
  write('', file=LOGFILE)
}
writelog(LOGFILE, caller = caller)
writelog("======== RUNING ==========", caller = caller)

task.fns = list.files(path = CV$serv$TARGET.DIR, pattern = glob2rx('*.txt'),  full.names = TRUE)
task.N = length(task.fns)
if(task.N <= 0){
  writelog(paste('Empty dirs. Nothing to do. ( path = ', normalizePath(CV$serv$TARGET.DIR), ')'), caller = caller)
  stop()
}

writelog(paste(task.N, 'task(s) in', CV$serv$TARGET.DIR), caller = caller)

itask = 1
# for(itask in 1:task.N){
for(itask in 1:task.N){
  task.fn = task.fns[itask]
  task.backup(task.fn, CV)
  
  LOGFILE = file.path(dir.log, paste0(format(Sys.time(), '%Y%m%d' ), '.log'))
  assign('LOGFILE',LOGFILE, envir = .GlobalEnv)
  writelog(paste('===================================='),  caller = caller)
  writelog(paste('\t', itask, '/', task.N, 'task \t', task.fn), caller = caller)
  
  theTask = readLines(task.fn)
  theTask = gsub('/home/wwwroot/r', '.', theTask)
  writelog(paste('\t', itask, '/', task.N, 'task \t', theTask), caller = caller)
  writelog(paste('\t++++++START++++++:', theTask), caller = caller)
  
  theTask = normalizePath(theTask)
  CV=configure(CV, theTask)
  # BACKUP the input.zip, input.json to UserData.
  fn.input = list.files(path=theTask, pattern = glob2rx('input*'), full.names = TRUE, recursive = TRUE)
  file.copy(from = fn.input, to = CV$dirs$userdata,  overwrite = TRUE, recursive = TRUE)
  file.copy(from = task.fn, to = file.path(CV$dirs$prj, basename(task.fn)), overwrite = TRUE, recursive = TRUE)
  
  #————— Clean the Task file. —————————————————————————————————————
  writelog(paste('REMOVING task file:', task.fn), caller = caller)
  if(!debug){
    unlink(task.fn, recursive = TRUE, force = TRUE, expand = TRUE)
  }
  #——————————————————————————————————————————
 
  ##### 1. Task.Ready.....
  writemessage(paste0('Trying to run task.ready(CV)'),  caller = caller, CV$task.log)
  iflag=try(task.ready(CV),TRUE)
  if(  class(iflag)=="try-error"  )   { 
    writemessage(paste0('ERROR: something wrong in task.ready(CV). '),  caller = caller, CV$task.log)
    warning('Task stop at task.ready(CV).')
    print(iflag)
    task.clean(task.fn);    next 
  }  else { 
    CV = iflag  
    writemessage(paste0('Finish the task.ready(CV). '),  caller = caller, CV$task.log)
    message('Step of task.ready finished. \n\n')
  } 
  
  ###### 2. Extract ETV
  writemessage(paste0('Trying to run ETV.Delineation(CV)'),  caller = caller, CV$task.log)
  iflag=try(ETV.Delineation(CV),TRUE)
  if(  class(iflag)=="try-error"  )   { 
    writemessage(paste0('ERROR: something wrong in ETV.Delineation(CV). '),  caller = caller, CV$task.log)
    warning('Task stop at ETV.Delineation(CV).')
    print(iflag)
    task.clean(task.fn);    next 
  }  else {
    writemessage(paste0('Finish the ETV.Delineation(CV). '),  caller = caller, CV$task.log)
    message('Step of ETV.Delineation finished. \n\n')} 
  
  writemessage(paste0('Trying to run ExtractETV(CV)'),  caller = caller, CV$task.log)
  iflag=try(ExtractETV(CV),TRUE)
  if(  class(iflag)=="try-error"  )   { 
    writemessage(paste0('ERROR: something wrong in ExtractETV(CV). '),  caller = caller, CV$task.log)
    warning('Task stop at ExtractETV(CV).')
    print(iflag)
    task.clean(task.fn);    next 
  }  else {
    writemessage(paste0('Finish the ExtractETV(CV). '),  caller = caller, CV$task.log)
    message('Step of ExtractETV finished. \n\n')} 
  
  ###### 3. Model Deployment 
  iflag=try(model.Deploy(CV),TRUE)
  writemessage(paste0('Trying to run model.Deploy(CV)'),  caller = caller, CV$task.log)
  if(  class(iflag)=="try-error"  )   { 
    writemessage(paste0('ERROR: something wrong in model.Deploy(CV). '),  caller = caller, CV$task.log)
    warning('Task stop at model.Deploy(CV).')
    print(iflag)
    task.clean(task.fn);    next 
  }  else {
    writemessage(paste0('Finish the model.Deploy(CV). '),  caller = caller, CV$task.log)
    message('Step of model.Deploy finished. \n\n')  } 
  
  ###### 4. Task done. Data packaging. update info.json/citys.js =====
  iflag=try(task.done(CV),TRUE)
  writemessage(paste0('Trying to run task.done(CV)'),  caller = caller, CV$task.log)
  if(  class(iflag)=="try-error"  )   { 
    writemessage(paste0('ERROR: something wrong in task.done(CV). '),  caller = caller, CV$task.log)
    warning('Task stop at task.done(CV)')
    task.clean(task.fn);  
    # next 
  }  else {
    writemessage(paste0('Finish the task.done(CV). '),  caller = caller, CV$task.log)
    message('Step of task.done finished. \n\n')
  } 
  writemessage(paste0('======== END ========'),  caller = caller, CV$task.log)
  writelog(paste('\n'),  caller = caller)
}



