library(rSHUD)
library(xts)
dir.in = '/home/leleshu/aFusion/CMFD_RDS'
dir.out = '/data/ForcingData/CMFD_csv'

yrs = 1980:2018
ny=length(yrs)

for(i in 1:ny){
  yr = yrs[i]
  cdir = file.path(dir.in, yr)
  fns = list.files(cdir, pattern = glob2rx('X*Y*.RDS'), recursive = TRUE,
                   full.names = TRUE)
  nfn = length(fns)
  message(i, '/', ny, '\t', yr, '\t', nfn)
  if(nfn > 0){
    dir.create(file.path(dir.out, yr), showWarnings = FALSE, recursive = TRUE)
  for(j in 1:nfn){
    fn.in = fns[i]
    sn = basename(fn.in)
    fn.out = file.path(dir.out, yr, paste0(substr(sn, 1, nchar(sn)-4), '.csv') )
    if(file.exists(fn.out)){
      message('\t', j , '/', nfn,  '\t', fn.out, ' !!exist!!')
    }else{
      message('\t', j , '/', nfn,  '\t', fn.out)
      x=readRDS(fn.in)
      write.xts(round(x, 4), file=fn.out)
    }
  }
  }
}


