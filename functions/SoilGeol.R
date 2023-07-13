#' Extract HWSD data.
HWSD.att <- function(fn.r = CV$etv$soil, fn.att=CV$files$soil.att, fn.out = CV$etv$soil.att, toplayer = TRUE){
  r = raster::raster(fn.r)
  ur=sort(raster::unique(r))
  x.tab = foreign::read.dbf(fn.att)
  
  cn =  c('SILT', 'CLAY', 'OC', 'BULK_DEN')
  if(toplayer){
    cnx = paste0('T_', cn )
  }else{
    cnx=paste0('S_', cn )
  }
  print(cnx)
  idx = x.tab[, 1] %in% ur
  subtab = x.tab[idx,   c('ID', cnx)]
  write.df(subtab, file=fn.out)
}
