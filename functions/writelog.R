#' 
#' 
#' @param msg The msging writing to fn.log.
#' @param n number of tab.
#' @param caller Name of the function calling this.
#' @return  NOTHING
#' @export
#' 
writelog <- function(msg='',  caller=''){
  fn.log = get('LOGFILE',envir = .GlobalEnv)
  if(exists('ntab', envir = .GlobalEnv)){
    n=get('ntab', envir = .GlobalEnv)
  }else{
    n=0
  }
  pretab = ''
  if(n > 0){
    for(i in 1:n){
      pretab = paste0(pretab, '\t')
    }
  }
  
  msg = paste0(Sys.time(), '\t', pretab,
              paste0('[', caller,']'), 
              "\t", 
              msg)
  
  write(x = msg, file=fn.log, append = TRUE)
  message(msg)
  # if(length(msg) <=1){
  #   write(x = msg, file=fn.log, append = TRUE)
  #   message(msg)
  # }else{
  #   # msg = paste0(Sys.time(), paste0('[', caller,']'),  " \t ")
  #   # write(x = msg, file=fn.log, append = TRUE)
  #   # message(msg)
  #   # 
  #   # msg = paste0(msg, pretab)
  #   write(x = msg, file=fn.log, append = TRUE)
  #   message(msg)
  # }
}

#' 
#' 
#' @param msg The msging writing to fn.log.
#' @param caller Name of the function calling this.
#' @param fn.log log file
#' @return  NOTHING
#' @export
#' 
writemessage <- function(msg='',  caller='', fn.log){
  msg = paste0(Sys.time(), '\t', 
               paste0('[', caller,']'), 
               "\t", 
               msg)
  write(x = msg, file=fn.log, append = TRUE)
  message(msg)
}


# task.done(CV)