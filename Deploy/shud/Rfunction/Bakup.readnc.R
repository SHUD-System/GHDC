readnc.time <- function(ncid) {
  # modified after https://stackoverflow.com/questions/46001573/convert-a-netcdf-time-variable-to-an-r-date-object
  require(lubridate)
  ncdims <- names(ncid$dim) #get netcdf dimensions
  timevar <- ncdims[which(ncdims %in% c("time", "Time", "datetime", "Datetime", "date", "Date"))[1]] #find time variable
  times <- ncvar_get(ncid, timevar)
  if (length(timevar)==0) stop("ERROR! Could not identify the correct time variable")
  timeatt <- ncatt_get(ncid, timevar) #get attributes
  timedef <- strsplit(timeatt$units, " ")[[1]]
  timeunit <- timedef[1]
  if(length(timedef) < 5){
    # cat("Warning:", tz, "not a valid timezone. Assuming UTC\n")
    tz <- "UTC"
  }else{
    tz <- timedef[5]
  }
  timestart <- strsplit(timedef[4], ":")[[1]]
  if (length(timestart) != 3 || timestart[1] > 24 || timestart[2] > 60 || timestart[3] > 60 || any(timestart < 0)) {
    cat("Warning:", timestart, "not a valid start time. Assuming 00:00:00\n")
    warning(paste("Warning:", timestart, "not a valid start time. Assuming 00:00:00\n"))
    timedef[4] <- "00:00:00"
  }
  timestart <- ymd_hms(paste(timedef[3], timedef[4]), tz=tz)
  f <- switch(tolower(timeunit), #Find the correct lubridate time function based on the unit
              seconds=seconds, second=seconds, sec=seconds,
              minutes=minutes, minute=minutes, min=minutes,
              hours=hours,     hour=hours,     h=hours,
              days=days,       day=days,       d=days,
              months=months,   month=months,   m=months,
              years=years,     year=years,     yr=years,
              NA
  )
  suppressWarnings(if (is.na(f)) stop("Could not understand the time unit format"))
  timestart + f(times)
}


readnc<-function(ncid= nc_open(fn), 
                 xyid=c(1,1), vns=NULL){

  if(is.null(vns)){
    vns = names(ncid$var)
    vns = vns[!(vns %in% 'time_bnds')] # don't need the time_bnds
  }
  
  nv = length(vns)
  x.mat = matrix(0, ncol=nv, nrow=ns)
  
  for(i in 1:nv){  #reading file
    vn=vns[i]
    mat=ncvar_get(ncid, vn)
    x.v = mat[xyid]
    x.mat[,i] = x.v
  }
  colnames(x.mat) = vns
  x.mat
}
nc.sub <-function(fn, ncid=ncdf4::nc_open(fn),
                  ext){
  ncid=nc_open(fns[1])
  #nc_close(ncid)
  lon=round(ncvar_get(ncid, 'longitude'), 3)
  lat=round(ncvar_get(ncid, 'latitude'), 3)
  nlon=length(lon)
  nlat = length(lat)
  #==================
  loc.confirm <- function(){
    ext = c(-20, 50, -38, 40)
    xid=which(lon >= ext[1] & lon <= ext[2])
    yid=which(lat >= ext[3] & lat <= ext[4])
    xy.grid = expand.grid(lon[xid], lat[yid])
    dat = ncvar_get(ncid, 'precip', 
                    count = c(length(xid), length(yid), 1),
                    start = c( which(lon == min(lon[xid]) ), which(lat==min(lat[yid]) ), 1) )
    xyz=cbind(xy.grid, as.numeric(dat) )
    r=rasterFromXYZ(xyz)
    png(filename = file.path(odir, paste0('LocationConfirm.png')), height = 11, width = 8, units = 'in', res=100)
    
    rplot(r); rplot(wbd, add=T, border=2)
    grid()
    dev.off()
  }
  nc_close(ncid)
}
read.nc2Raster <- function(fn, ncid=ncdf4::nc_open(fn),
                           plot=TRUE, 
                           varid=2, xname=NULL, yname=NULL){
  nv = ncid$nvars
  if(is.character(varid)){
    varid = varid
  }else{
    vns = names(ncid$var)
    if(nv>0){
      varid = vns[varid]
    }else{
      varid = vns[1]
    }
  }
  if(is.null(xname) | is.null(yname)){
    dn = names(ncid$dim)
    yname = dn[grepl('^lat|^x', tolower(dn) )]
    xname = dn[grepl('^lon|^y', tolower(dn) )]
  }
  x = ncdf4::ncvar_get(ncid, xname)
  y = ncdf4::ncvar_get(ncid, yname)
  nx=length(x); ny=length(y);
  dx=mean(diff(x)); dy=mean(diff(y))
  r = raster::raster(ncols=nx, nrows=ny)
  raster::extent(r) = c(min(x), max(x), min(y), max(y)) + c(-dx, dx, -dy, dy)/2
  val = ncdf4::ncvar_get(ncid, varid)
  nc_close(ncid)
  # dim(val)
  r = raster::setValues(r, t(val[, ny:1 ]) )
  if(plot){
    raster::plot(r, main=varid)
  }
  r
}

# vn=vns[4]
# ncvar_get(ncid, varid = vn, start=)
