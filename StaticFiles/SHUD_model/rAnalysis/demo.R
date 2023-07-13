#' ===================================================
#' Analysis the output results from SHUD model
#' Author: Lele Shu <shulele@lzb.ac.cn>
#' Website: www.shud.xyz
#' Date: 2023-05-27
#' Copyright: MIT-Licence
#' ===================================================
#' Function:
#'
#'

rm(list=ls())
library(rSHUD)
library(xts)
library(raster)
library(rgdal)
library(sp)
library(ggplot2)

prjname = 'lcg'
shud.env(prjname = 'lcg',
         inpath = file.path('../../../Modeling/input/', prjname),
         outpath = file.path('../../../Modeling/output/', paste0(prjname, '.out')))
AA = getArea()
oid = getOutlets()
spr=sp.riv2shp()

qdown = readout('rivqdown')
qs = ts2Daily(qdown[, oid[1]])
ts = time(qs)
autoplot(qs[,])

gw=readout('eleygw')
mgw = colMeans(gw)
sp.mesh = sp.mesh2Shape(dbf = data.frame('gw'=mgw))
plot_sp(sp.mesh, zcol = 'gw')
plot(add=T, spr, col='red')


