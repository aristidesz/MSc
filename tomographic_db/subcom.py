'''
			set file = {2};
			echo $file;
			set OUTFILEmy=$PWD/MScFirst/media/GMT_Images/$file; 
			set dx = 0.703125;
			# set PROJ = -JKs160/7.0;						
			# set REG = -Rg;							
			set REG1 = -R0.3515625/359.6484375/-89.6484375/89.6484375;	
			set dept =  ( 250 350 410 660 800 900 1000 1200 1400 1600 1800 2000 2200 2400 2600 2800 );
			set layr =  (   7   9  10  15  17  18   19   21   23   25   27   29   31   33   35   37 );
			set lett =  (   a   b   c   d   e   f    g    h    i    j    k    l    m    n    o    p );
			set tx =  ( -0.35 0.0  0.0  0.0  7.5  0.0  0.0  0.0  7.5  0.0  0.0  0.0  7.5  0.0  0.0  0.0 );
			set ty =  ( 18.9 -3.8 -3.8 -3.8 11.4 -3.8 -3.8 -3.8 11.4 -3.8 -3.8 -3.8 11.4 -3.8 -3.8 -3.8 );
			set COL=$PWD/media/dvp.cpt; 
			# scale bottom right
			psscale -D14.5/7./8/0.3h -X0.5 -Y-1.5 -B0.35:"@%12%\144@%%v@-P@-/v@-P@- [\045]": -C$COL -E -S -K > $OUTFILEmy.ps;
			set i = 1;
			xyz2grd Pmod/layer$layr[$i] -Ddegree -Gtmp.grd -I$dx $REG1 -:;
			grdimage tmp.grd -C$COL {0} {1} -X$tx[$i] -Y$ty[$i] -O -K >> $OUTFILEmy.ps;
			pscoast {0} {1} -Dc -A10000 -W0.4p,dimgrey -O -K >> $OUTFILEmy.ps;
			psxy {0} {1} boundaries.xy -Bg60/g30wsen+ -W0.25p,grey	-O -K >> $OUTFILEmy.ps;
			echo "1.5 0.5 $dept[$i] km" |pstext -R0/7.5/0/3.75 -JX7.5/3.75 -F+f10,Helvetica+jMC -Gwhite -W1p -N -O -K >> $OUTFILEmy.ps;
			echo "0.1 3.5 ($lett[$i])" |pstext -R0/7.5/0/3.75 -JX7.5/3.75 -F+f12,Helvetica-Bold+jTL -N -O -K >> $OUTFILEmy.ps;
			rm $OUTFILEmy.eps;
			ps2eps -R=+ $OUTFILEmy.ps;
			convert $OUTFILEmy.eps $OUTFILEmy.jpg
			rm $OUTFILEmy.eps;
			rm $OUTFILEmy.ps tmp.*;
			'''