#!/bin/bash
outname="montage_rd-twotiles-random-hilbert-16k"
montage -geometry 64x64+0+0 -tile 128x8 output/00000000/0*.png $outname-00000000".png"
montage -geometry 64x64+0+0 -tile 128x8 output/00001024/0*.png $outname-00001024".png"
montage -geometry 64x64+0+0 -tile 128x8 output/00002048/0*.png $outname-00002048".png"
montage -geometry 64x64+0+0 -tile 128x8 output/00003072/0*.png $outname-00003072".png"

montage -geometry 64x64+0+0 -tile 128x8 output/00004096/0*.png $outname-00004096".png"
montage -geometry 64x64+0+0 -tile 128x8 output/00005120/0*.png $outname-00005120".png"
montage -geometry 64x64+0+0 -tile 128x8 output/00006144/0*.png $outname-00006144".png"
montage -geometry 64x64+0+0 -tile 128x8 output/00007168/0*.png $outname-00007168".png"

montage -geometry 64x64+0+0 -tile 128x8 output/00008192/0*.png $outname-00008192".png"
montage -geometry 64x64+0+0 -tile 128x8 output/00009216/0*.png $outname-00009216".png"
montage -geometry 64x64+0+0 -tile 128x8 output/00010240/0*.png $outname-00010240".png"
montage -geometry 64x64+0+0 -tile 128x8 output/00011264/0*.png $outname-00011264".png"

montage -geometry 64x64+0+0 -tile 128x8 output/00012288/0*.png $outname-00012288".png"
montage -geometry 64x64+0+0 -tile 128x8 output/00013312/0*.png $outname-00013312".png"
montage -geometry 64x64+0+0 -tile 128x8 output/00014336/0*.png $outname-00014336".png"
montage -geometry 64x64+0+0 -tile 128x8 output/00015360/0*.png $outname-00015360".png"
