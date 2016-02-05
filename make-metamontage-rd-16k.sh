#!/bin/bash
d=`date +"%Y%m%d_%H%M%S"`
fname="meta-montage_rd-twotiles-random-hilbert-16k_"$d".png"
echo montage: $fname
#montage -geometry 16384x4096+0+0 -tile 1x4 montage_hex_two_tiles_wide-2_00*.png $fname
#montage -geometry 8192x2048+0+0 -tile 1x10 montage_two-tiles_2.3_*.png $fname
montage -geometry 16384x1024+0+0 -tile 1x16 montage_rd-twotiles-random-hilbert-16k-*.png $fname
