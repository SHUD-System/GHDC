
zipout <- function(indir, fn.zip, outdir){
  caller = as.character( deparse(sys.call()))
  owd = getwd()
  setwd(indir)
  # writelog(paste0('Zipping files, from ', indir, ' to ', fn.zip), caller=caller)
  # utils::zip(zipfile = fn.zip, files = './', flags = "-r9X")
  
  writelog(paste0('Copy files, from ', indir, ' to ', outdir), caller=caller)
  file.copy(from=indir, to=outdir, overwrite = TRUE, recursive = TRUE)
  
  setwd(owd)
  writelog(paste0('Finished'), caller=caller)
}

checkPolygon <- function(spx){
  if(is(spx, 'SpatialPolygons')){
    return(spx)
  # }else if(is(spx, 'SpatialLines')){
  #   spy = rgeos::gPolygonize(spx)
  }else{
    return (NULL)
  }
  return(spy)
}

#' check_folder()
#' @param theDir the directory is searched.
#' @return the directory
#' @export
check_Folder <- function(theDir = 'data'){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  msg = 'check_Folder()'
  x = list.dirs(theDir, recursive = FALSE)
  if(is.null(x)){
    writelog('Target folder is empty.', caller = caller)
  }else{
    writelog(x, caller = caller)
  }
  return(x)
}


# df.prj = write.autoshud(x.json=x.json,
#                fn.wbd = fn.wbd, 
#                fn.stm = fn.wbd,
#                fn.dem = fn.wbd,
#                dir.ldas = NULL)
# xo = cbind(names(df.prj), 
#            t(data.frame(df.prj) ))
# write.table(xo, file=file.path(dir.prj, 'autoshud.prj'), col.names = FALSE, row.names = FALSE, quote = FALSE)

#'
#'
checkFile <- function(fn, str = ''){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  if( length(fn) != 1  ){
    msg = paste('!!!ERROR:', length(fn), str, 'file(s).')
    writelog(msg=msg, caller = caller)
    # stop()
    return(FALSE)
  }else{
    if(file.exists(fn)){
      msg = paste(str, 'files exist: ', fn)
      writelog(msg=msg, caller = caller)
      # return(file.info(fn))
      return(TRUE)
    }else{
      msg = paste0('!!!ERROR:', str, 'Files DOES NOT exist: ', fn)
      writelog(msg=msg, caller = caller)
      # stop()
      return(FALSE)
    }
  }
}

task.backup <- function(task.fn, CV){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  x=task.fn
  fn.bak=file.path(CV$serv$BAK_TXT, paste0( format(Sys.time(), "%Y%m%d%H%M%S"), '.', basename(x)) )
  file.copy(from = x,  to = fn.bak, overwrite = TRUE, recursive = TRUE)
  # file.copy(from = x, to = file.path(CV$dirs$prj, basename(x)), overwrite = TRUE, recursive = TRUE)
  writelog(paste('!!!DONE!!!:', x), caller = caller)
  writelog(paste('===================================='), caller = caller)
}


#' 
#' 
task.clean <- function(x){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  # ===== zip the output  =====================
  zipout(indir = CV$dirs$prj, fn.zip = CV$files$zipout, outdir = CV$dirs$download)
  # ===== Remove taskfile  =====================
  writelog(paste('REMOVING DIR:', x), caller = caller)
  unlink(x, recursive = TRUE, force = TRUE, expand = TRUE)
  
  writelog(paste('!!!DONE!!!:', x), caller = caller)
  writelog(paste('===================================='), caller = caller)
}

task.done <- function(CV){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  # ===== zip the output  =====================
  zipout(indir = CV$dirs$prj, fn.zip = CV$files$zipout, outdir = CV$dirs$download)
  # =====  update the website values.  =====================
  updateTaskHistory(CV)
  # ===== email out  =====================
  emailout(CV)
  writelog(paste0('Done,  do_the_task'), caller=caller)
}

task.fail <- function(CV){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  # ===== zip the output  =====================
  zipout(indir = CV$dirs$prj, fn.zip = CV$files$zipout, outdir = CV$dirs$download)
  # ===== email out  =====================
  for(i in 1:3){ writelog(paste0('!!!!Failed!!!!'), caller=caller) }
  stop()
}

