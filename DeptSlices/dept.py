from gmtpy import GMT

#!/bin/csh

# OUTPUT
gmt.save(dept-slices)

# MAP PARAMETERS
gmt.save(dx = 0.703125)						# spatial sampling
gmt.save(PROJ = -JKf160/7.0)						# projection
gmt.save(REG = -Rg)							# plot area
gmt.save(R = 0.3515625/359.6484375/-89.6484375/89.6484375)	
gmt.save(REG1 = -R)	# inversion area (for binary file)

# IN-LOOP VARIABLES
gmt.save(dept = ( 250, 350, 410, 660, 800, 900, 1000, 1200, 1400, 1600, 1800, 2000, 2200, 2400, 2600, 2800 ))
gmt.save(layr =  (   7,   9,  10,  15,  17,  18,   19,   21,   23,   25,   27,   29,   31,   33,   35,   37 ))
gmt.save(lett =  (   a,   b,   c,   d,   e,   f,    g,    h,    i,    j,    k,    l,    m,    n,    o,    p ))
gmt.save(tx =  ( -0.35, 0.0,  0.0,  0.0,  7.5,  0.0,  0.0,  0.0,  7.5,  0.0,  0.0,  0.0,  7.5,  0.0,  0.0, 0.0 ))
gmt.save(ty =  ( 18.9, -3.8, -3.8, -3.8, 11.4, -3.8, -3.8, -3.8, 11.4, -3.8, -3.8, -3.8, 11.4, -3.8, -3.8, -3.8 ))

# COLOR PALETTE
gmt.save(COL = dvp.cpt)
gmt.psscale(D='14.5/7./8/0.3h',
	X='0.5',
	Y='-1.5', 
	B='0.35:"@%12%\144@%%v@-P@-/v@-P@- [\045]": -C$COL -E -S -K >  $OUTFILE.ps)')

# @%12%: use special characters
# @%%: go back to using normal characters
# \144: code for 'delta'
# @-: subscript

i = 1
while (i <= len(layr)):

  #echo i layr[i] '('dept[i]' km)'

  # create tomographic image
  gmt.xyz2grd( Pmod/layer$layr[$i] -Ddegree -Gtmp.grd -I$dx $REG1 -:)

  # plot tomographic image
  gmt.grdimage( tmp.grd -C$COL $REG $PROJ -X$tx[$i] -Y$ty[$i]	-O -K >> $OUTFILE.ps)

  # coast lines
  gmt.pscoast( $REG $PROJ -Dc -A10000 -W0.4p,dimgrey			-O -K >> $OUTFILE.ps)

  # plate boundaries
  gmt.psxy( $REG $PROJ boundaries.xy -Bg60/g30wsen+ -W0.25p,grey	-O -K >> $OUTFILE.ps)

  # annotations
  echo "1.5 0.5 $dept[$i] km" |pstext -R0/7.5/0/3.75 -JX7.5/3.75 -F+f10,Helvetica+jMC -Gwhite -W1p -N -O -K >> $OUTFILE.ps
  echo "0.1 3.5 ($lett[$i])" |pstext -R0/7.5/0/3.75 -JX7.5/3.75 -F+f12,Helvetica-Bold+jTL -N -O -K >> $OUTFILE.ps

  @ i++
  end

  rm $OUTFILE.eps
  gmt.ps2eps( -R=+ $OUTFILE.ps)
  rm $OUTFILE.ps tmp.*
