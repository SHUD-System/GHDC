
#' copyBib()
#' @param toDIR the directory .
#' @param bibfile the citation file.
#' @return the directory
#' @export
copyBib <- function(toDIR, bibfile = 'functions/citation.bib'){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  
  file.copy(from = bibfile, toDIR)
  writelog(msg='Finished', caller = caller)
}

