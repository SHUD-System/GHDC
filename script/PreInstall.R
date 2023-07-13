#' ===============================================================
#' Lele Shu <shulele@lzb.ac.cn>
#' Date: 2022.10.03
#' Function:  the required packages installation before the autoshud code.
#' ===============================================================

#' Install rSHUD package with source Code.
#' R CMD INSTALL rSHUD.

#' Install GDAL before rGDAL
install.packages('rgdal')

#' Whitetoolbox support.
install.packages('whitebox')
whitebox::install_whitebox()


#' Install NETCDF out of R before the R package installation.
install.packages('ncdf4')

