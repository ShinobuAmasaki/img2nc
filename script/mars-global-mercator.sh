#!/bin/bash

# For Gentoo Linux environment
type emerge 2>&1 >/dev/null && shopt -s expand_aliases && alias gmt='gmt6'

nc='mola.nc'
range='-180/180/-70/70'
grad="grad.nc"
dpi=100

gmt set PROJ_ELLIPSOID = Mars
gmt begin mars-global png
   gmt basemap -JM16i -R$range -Bafg -BWeSn
   gmt makecpt -Ccopper -T-24000/32000/1000 -Z

   gmt grdgradient $nc -G$grad -A320 -Ne0.5
   gmt grdimage $nc -JM16i -R$range -C -E$dpi -I$grad

   gmt text  -F+a0+f20p,white+cRB -Dj0.1/0.1 << EOF
MGS-MOLA/NASA
EOF
gmt end