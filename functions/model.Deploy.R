
#' Core for SHUD model deployment.
#' 
#' 
mod.shud <- function(CV){
  fn.getready = file.path(CV$dirs$deploy, 'GetReady.R')
  source(fn.getready)
  # == 1. write AutoSHUD file. ====
  x = write.autoshud(CV)
  writemessage(paste0('Write the autoSHUD configure file out. '),  caller = caller, CV$task.log)
  CV$deploy = utils::modifyList(CV$deploy, x)
  # == 2. Read the AutoSHUD file  =====
  writemessage(paste0('Configure the autoSHUD environment... '),  caller = caller, CV$task.log)
  CV = GetReady.AutoSHUD(CV)
  
  # == 3 Run the deployment codes =====
  AutoSHUD_Step1(CV)
  AutoSHUD_Step2(CV)
  AutoSHUD_Step3(CV)
}
# mod.shud(CV)

#' Plan of VIC model deployment.
mod.vic <-function(){}

#' Plan of SWAT model deployment.
mod.swat <-function(){}

#' Plan of topmodel  deployment.
mod.topmodel <-function(){}

#' the entry of the model deploy function
#' 
model.Deploy <- function(CV){
  caller = as.character( deparse(sys.call())   )
  writelog(msg=caller, caller = caller)
  ls.model <- list(
    shud = mod.shud,
    topmodel = mod.topmodel,
    vic = mod.vic,
    swat = mod.swat
  )
  
  if(CV$para$dataonly){
    writemessage(paste0('Data only reqrest. SKIP the model deployment. '),  caller = caller, CV$task.log)
    writelog(msg='Data only reqrest. SKIP the model deployment.', caller = caller)
    return(0)
  }else{
    writemessage(paste0('Start the model deployment ... '),  caller = caller, CV$task.log)
    model.func <- ls.model[[tolower(CV$json$model)]]
  }
  ret= model.func(CV)
  return(ret)
}

