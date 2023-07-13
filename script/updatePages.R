source('script/logo.R')
fns = list.files('Makrdown', pattern = glob2rx('*.Rmd'))

fns = c('Markdown/HydroCloud_cn.Rmd',
       'Markdown/HydroCloud_en.Rmd')
go.knit <- function(fns){
  nx=length(fns)
  for(i in 1:nx){
    fn = normalizePath(fns[i])
    dir.out = normalizePath('static/res/')
    message(i, '/', nx, '\t', fn, ' -> ', dir.out)
    # print(getwd())
    # owd = getwd()
    # setwd('static/pages/')
    rmarkdown::render(input = fn, output_dir = dir.out)
    # setwd(owd)
  }
}

go.knit(fns)


