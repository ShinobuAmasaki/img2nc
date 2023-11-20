#!/bin/bash
#This script depends on GMT6

# For Gentoo Linux environment
type emerge 2>&1 >/dev/null && shopt -s expand_aliases && alias gmt='gmt6'

nc='out.nc'
range='338/341/24/27'
grad='grad.nc'
dpi=250

gmt set PROJ_ELLIPSOID = Moon
gmt begin lambart png
	gmt basemap -JM8i -R$range -Bafg -BWeSn
	gmt makecpt -Cviridis -T-5000/1000/1000 -Z
	gmt grdgradient out.nc -G$grad -A310 -Ne0.6
	gmt grdimage out.nc -JM8i -C -E$dpi -I$grad

	gmt text -F+a0+f20p,white+cRB -Dj0.1/0.1 << EOF
SLDEM2013/ISAS,JAXA
EOF
gmt end