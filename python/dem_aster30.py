#!/usr/bin/python

# Requirement:
#       1. DEM files download from ASTER GDEM. Total files is more than 300 GB.
#          More information may be found in http://shud.ncdc.ac.cn/res/HydroCloud_cn.html
#       2. gdal and gdal-mergy.py files.
#
# The path of ASTER Global DEM files: '/Volumes/Spatial/World/DEM/Aster_GDEM/'
# Example files: ASTGTMV003_N00E011_dem.tif, ASTGTMV003_S83W143_dem.tif
#
# The OUTPUT:
#       Option 1: gdal_merge.py command. You can copy, modify and execute the command by your own.
#       Option 2: Execute the command via calling system command in python.

import sys
import math
import os
    
def funmerge(xmin, xmax, ymin, ymax, path, fout):
    flist = []
    xx=range(math.floor(float(xmin)), math.ceil(float(xmax)))
    yy=range(math.floor(float(ymin)), math.ceil(float(ymax)))
    #path = '/Volumes/SpatialData/World/DEM/Aster_GDEM/'
    prefix = ''
    sufix = '_elv.tif'
    #fout = 'gdem'+'_'.join([str(xmin), str(xmax), str(ymin), str(ymax)])+'.tif'
    cmd = 'gdal_merge.py -o ' + fout + ' '
    for i in xx:
        for j in yy:
            sx = 'w' + str(abs(i)).zfill(3) if i < 0 else 'e' + str(abs(i)).zfill(3)
            sy = 'w' + str(abs(j)).zfill(2) if i < 0 else 'n' + str(abs(j)).zfill(2) 
            fs = ''.join([str(path), prefix, sy, sx, sufix])
            if os.path.isfile(fs):
                print('Exist: ', fs)
                flist.append(fs)
            else:
                print('Missing: ', fs)
            
    
    cmd = cmd + ' '.join(flist)
    print('\n')
    print(cmd)
    print('\n')
    #os.system(cmd)  # uncomment this line, if you wanna execute the command.

#funmerge(101, 103, 34, 37)

print ('Number of arguments:', len(sys.argv), 'arguments.')
print ('Argument List:', str(sys.argv) )

nv = len(sys.argv)
if nv != 8 :
    print('Usage: ')
    print('\tpython3 merge.py lonmin lonmax latmin latmax Path_to_DEM_file')
    print('\n')
else:
    print(' '.join(sys.argv))
    print('\n')
    funmerge(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5], sys.argv[6])

print('Done.\n\n')

