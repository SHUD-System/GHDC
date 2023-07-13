load.conf <- function(file = 'service.cfg.txt'){
  cfg = rSHUD::readconfig(file=file)
  
  tmp = lapply(list(cfg$DIR.WORKING, cfg$DIR.EMAIL, cfg$DIR.ZIPOUT ), 
               dir.create,  showWarnings = FALSE, recursive = TRUE)
  
  cfg$DIR.WORKING = normalizePath(cfg$DIR.WORKING)
  cfg$TARGET.DIR = normalizePath(cfg$TARGET.DIR )
  cfg$PATH2SD = normalizePath(cfg$PATH2SD)
  cfg$PATH2FD = normalizePath(cfg$PATH2FD)
  cfg$DIR.EMAIL = normalizePath(cfg$DIR.EMAIL)
  cfg$DIR.ZIPOUT = normalizePath(cfg$DIR.ZIPOUT)
  cfg$LINK.DL = cfg$LINK.DL
  if(is.null(cfg$BACKUP) ){
    cfg$BACKUP =  file.path(cfg$DIR.WORKING, 'backup')
  }else{
    #void
  }
  dir.create(cfg$BACKUP, showWarnings = FALSE, recursive = TRUE, mode='0777')
  cfg$BACKUP =  normalizePath(cfg$BACKUP)
  cfg$BAK_TXT =  file.path(cfg$BACKUP, 'confirmData')
  cfg$BAK_DATA =  file.path(cfg$BACKUP, 'data')
  
  dir.create(cfg$BAK_TXT, showWarnings = FALSE, recursive = TRUE, mode='0777')
  dir.create(cfg$BAK_DATA, showWarnings = FALSE, recursive = TRUE, mode='0777')
  
  return(cfg)
}
# CV$serv = load.conf(file = 'script/service.cfg.txt')