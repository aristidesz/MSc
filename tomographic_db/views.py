from django.http import HttpResponse
from django.shortcuts import render
from django.http import Http404
# Create your views here.
from django.shortcuts import render_to_response
from django.template import RequestContext
from django.template.loader import render_to_string
from django.http import HttpResponse
from django.http import HttpResponseRedirect
from .models import tomoImages
from tomographic_db.models import tomoImages, GMTImages
from tomographic_db.models import globeImages
from django.http import HttpResponse
from django.template import RequestContext, loader
from django.shortcuts import HttpResponse,get_object_or_404
from django.conf import settings
from django.template import RequestContext, Template, Context
from django.shortcuts import render, redirect
from django.http import HttpResponseRedirect
from .forms import FormTomoImages,FormGlobeImages, FormGMTImage
from django.core.urlresolvers import reverse
import os
import subprocess
import csv
from django.core.files.storage import default_storage
from django.core.files.base import ContentFile
from .tasks import *
from celery import task
from celery.task import task
from celery.task.base import task
from .tasks import subpro as subpro

def detail(request, imageName):
    return HttpResponse("You're looking at images with name %s." % imageName)

def index(request):
	print "1"
	images_list = tomoImages.objects.all()
	images_list_globe = globeImages.objects.all()
	print images_list
	template = loader.get_template('tomographic_db/home.html')
	#newimages_list = new_TomoImages.objects.all()	
	context = RequestContext(request,{'images_list': images_list,'images_list_globe':images_list_globe,})
	print add(2,2)

	return HttpResponse(template.render(context))


def image(request):
	form = FormTomoImages(request.GET or None) 
	form_globe = FormGlobeImages(request.GET or None) 
	formGMT = FormGMTImage(request.GET or None)
	template = loader.get_template('tomographic_db/image.html')
	if form_globe.is_valid():
		template = loader.get_template('tomographic_db/image1.html')
		globeName= form_globe.cleaned_data['form_globeName']
		new_GlobeImages=globeImages.objects.get(globeName=globeName)
		print "3"
		print new_GlobeImages
		print new_GlobeImages.globeImage.url
		text = open(os.path.join(settings.MEDIA_ROOT, 'dvp.cpt')).read()
		

		context = RequestContext(request,{'new_GlobeImages':new_GlobeImages,})
		return HttpResponse(template.render(context))
	elif form.is_valid():
		imageName= form.cleaned_data['form_imageName']
		new_TomoImages= tomoImages.objects.get(imageName=imageName)
		print new_TomoImages
		print new_TomoImages.image.url
		print "1"
		context = RequestContext(request,{'new_TomoImages': new_TomoImages,})
		return HttpResponse(template.render(context))
	elif formGMT.is_valid():
		GMTlatMAX = formGMT.cleaned_data['form_latMAX']
		GMTlatMIN=formGMT.cleaned_data['form_latMIN']
		GMTlonMAX=formGMT.cleaned_data['form_lonMAX']
		GMTlonMIN=formGMT.cleaned_data['form_lonMIN']	
		GMTDepth= formGMT.cleaned_data['form_Depth']
		GMTImageName = GMTDepth+GMTlatMAX+GMTlatMIN+GMTlonMAX+GMTlonMIN
		print GMTlatMAX, GMTlatMIN,GMTlonMAX,GMTlonMIN, GMTDepth,GMTImageName	
		subpro('''
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
		''',GMTImageName)
		pic = GMTImages()
		pic.GMTImageName = GMTImageName
		pic.GMTImage = "GMT_Images/"+GMTImageName+".jpg"
		pic.save()
		return HttpResponse(template.render())
	else:
		form = FormTomoImages()	
		new_TomoImages = form
		print "2"
		images_list = tomoImages.objects.all()
		images_list_globe = globeImages.objects.all()
		template = loader.get_template('tomographic_db/home.html')
		context = RequestContext(request,{'images_list': images_list,'images_list_globe':images_list_globe,})
		return HttpResponse(template.render(context))
