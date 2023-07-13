libs=c('terra', 'raster', 'sp', 'rgdal', 'rgeos',  'rgl',
       'gstat', 'zoo', 'xts', 'hydroGOF', 'hydroTSM',
       'ggplot2', 'lattice', 'reshape2',
       'abind', 'spam', 'fields',
       'RTriangle', 'geometry',
       'Rcpp', 'sf',
       'deldir', 'proj4', 'lubridate', 'interp', 
       'GGally',
       'mapproj',
       'coord_map',
       'akima',
       'graticule',
       'doParallel','rjson'
       
)
# libs=c('lavaan', 'multcompView', 'pander', 'psych', 'pwr', 'rmdpartials', 'ufs', 'kableExtra') # ROSETTA

fun.inst <- function(libs){
  nx=length(libs)
  i=1
  for(i in 1:nx){
    x=libs[i]
    message(i, '/', nx, '\t', x)
    if(!require(x, character.only = TRUE, )){
      message('Installing ', x)
      install.packages(x)
    }else{
      message('Existing ', x)
    }
  }
}
fun.inst(libs)


if(!require(devtools)){
  install.packages("devtools")
}
devtools::install_github("shulele/RTriangle", subdir="pkg")


