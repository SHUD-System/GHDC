
#' 
#'
emailout <- function(CV){
  caller = as.character( deparse(sys.call())   )
  if(is.null(CV$json$offline)){
    # continue
  }else{
    if(CV$json$offline > 0){
      return(0);
    }else{
      #continue
    }
  }
  fn = CV$files$email
  email = CV$json$email
  url = CV$files$link_zip
  
  writelog(paste0('Writing email to ', email, '; ', url), caller=caller)
  xl = readLines('Markdown/emailout.md')
  yl = gsub('dataurl', url, xl)
  yl = gsub('emailaddress', email, yl)
  
  write(yl, file = fn)
  file.copy(from=fn, to=file.path(CV$dirs$bak, paste('email_', basename(fn))) )
  writelog(paste0('Finished'), caller=caller)
}


email.error <- function(CV, msgfile){
  caller = as.character( deparse(sys.call())   )
  fn = CV$files$email
  email = CV$json$email
  url = CV$files$link_zip
  
  writelog(paste0('Writing email to ', email, '; ', url), caller=caller)
  xl = readLines('Markdown/emailerror.md')
  # ## yl = gsub('dataurl', url, xl)
  yl = gsub('emailaddress', email, xl)
  
  msg = readLines(file.path('Markdown/errorMSG', msgfile))
  yl = c(yl, msg)
  
  write(yl, file = fn)
  file.copy(from=fn, to=file.path(CV$dirs$bak, paste('email_', basename(fn))) )
  writelog(paste0('Finished'), caller=caller)
}
