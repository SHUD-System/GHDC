
#' Update the Task History and save to Update/TaskHistory.RDS
#' The code also update the city.js and info.json file.
#' 
updateTaskHistory <- function(CV){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  
  spx = readOGR(CV$etv$wbd.gcs)
  ext.wbd=extent(spx)
  fn.city.js = 'citys.js'
  fn.rds.uniq = 'Update/TaskHistory_unique.RDS'
  fn.rds.dup = 'Update/TaskHistory_dup.RDS'
  fn.info.template='Update/info.json.txt'
  fn.info.json = 'info.json'
  
  ## udpate *******info.json***********
  update.info <- function(ntimes){
    txt=readLines(fn.info.template) 
    txt=gsub('xxxx', ntimes + 1062, txt)
    write.table(txt, file = fn.info.json, 
                row.names=FALSE, col.names = FALSE, 
                append = FALSE, quote = FALSE )
    
  }
  
  ## update Update/TaskHistory.RDS
  x = data.frame('Lon'=round(mean(ext.wbd[1:2]), 3),
                 'Lat'=round(mean(ext.wbd[3:4]), 3), 
                 'Area' = CV$para$Area,
                 data.frame(CV$json) )
  if(file.exists(fn.rds.uniq)){
    x.old_uniq=readRDS(fn.rds.uniq)
    x.old_uniq$Lon = round(x.old_uniq$Lon, 3)
    x.old_uniq$Lat = round(x.old_uniq$Lat, 3)
    x.new_uniq = plyr::rbind.fill(x.old_uniq, x)
    
    x.old_dup=readRDS(fn.rds.dup)
    x.new_dup = plyr::rbind.fill(x.old_dup, x)
  }else{
    x.new_uniq = x
  }
  x.dup = x.new_dup
  x.uniq = unique(x.new_uniq)
  saveRDS(x.uniq, file = fn.rds.uniq)
  saveRDS(x.dup, file = fn.rds.dup)
  
  ## update the *******info.json***********
  update.info(ntimes = nrow(x.dup))
  
  ## *********update city.js********************
  ## Only the UNIQUE values are added into the city.js.
  loc.xy = x.uniq[, c('Lon', 'Lat', 'project_name')]
  tab=cbind(loc.xy, 'style'=round(log10(x.uniq[, 'Area'] / 1e6 ), 0)+1)
  uq = unique(tab)
  txt =     paste(
    ' { "lnglat": [', uq$Lon, ',', uq$Lat, '] ,"name": "', 
    uq$project_name, '" ,"style": ', uq$style, '}')
  ntxt = length(txt)
  txt[ -1 * ntxt] = paste0(txt[ -1 * ntxt], ',')
  
  txt.out = c(
      'var citys = [   ',
    txt,
    ']'
  )
  write.table(txt.out, file = fn.city.js, 
              row.names=FALSE, col.names = FALSE, append = FALSE, quote = FALSE )
  writelog(paste0('Done,  do_the_task'), caller=caller)
}
# updateTaskHistory(CV)

# xx=read.table('city.txt')
# colnames(xx) = c('Lon', 'Lat', 'project_name')
# yy=data.frame(matrix(NA, 18, 19))
# colnames(yy) = colnames(x.new)
# yy[1:17,]$Lon = xx$Lon
# yy[1:17,]$Lat = xx$Lat
# yy[1:17,]$project_name = xx$project_name
# yy[18, ]=x.new[1, ]
# yy
# saveRDS(yy, file = fn.rds.uniq)
