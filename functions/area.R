goArea <- function(fn, spx = rgdal::readOGR(fn)){
  crs.pcs = crs.Albers(spx=spx)
  wbd.pcs = spTransform(spx, crs.pcs)
  AA = rgeos::gArea(wbd.pcs)
  return(AA)
}