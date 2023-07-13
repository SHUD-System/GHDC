library(rjson)

txt=readLines('/Users/leleshu/CloudDrive/SHUD/Qgis/Boundary/ll.csv')
tt = read.table(text=txt[-1], header = 1)
tt
nx = nrow(tt)
ndigits = 5
sline = paste('{\n"lnglat":', paste0('[', round(tt[, 2], ndigits), ',', round(tt[, 3], ndigits),  ']') ,
      '\n,"name":', paste0('"', tt[, 1],  '"') ,
      '\n,"style":', '1', '\n}') 
str = paste0('var citys = [',
  paste(sline, collapse = ',\n'),
  ']')
str
write(str, file = 'citys.js')
