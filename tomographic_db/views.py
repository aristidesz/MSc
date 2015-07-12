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
from tomographic_db.models import tomoImages
from django.http import HttpResponse
from django.template import RequestContext, loader
from django.shortcuts import HttpResponse,get_object_or_404
from django.conf import settings
from django.template import RequestContext, Template, Context
from django.shortcuts import render, redirect
from django.http import HttpResponseRedirect
from .forms import FormTomoImages
from django.core.urlresolvers import reverse


def detail(request, imageName):
    return HttpResponse("You're looking at images with name %s." % imageName)

def index(request):
	print "1"
	images_list = tomoImages.objects.all()
	print images_list
	template = loader.get_template('tomographic_db/home.html')
	#newimages_list = new_TomoImages.objects.all()	
	context = RequestContext(request,{'images_list': images_list,})
	return HttpResponse(template.render(context))


def image(request):
	form = FormTomoImages(request.GET or None) 
	template = loader.get_template('tomographic_db/image.html')
	if form.is_valid():
		imageName= form.cleaned_data['form_imageName']
		new_TomoImages= tomoImages.objects.get(imageName=imageName)
		print new_TomoImages
		print new_TomoImages.image.url
		print "1"
		context = RequestContext(request,{'new_TomoImages': new_TomoImages})
		return HttpResponse(template.render(context))
	else:
		form = FormTomoImages()	
		new_TomoImages = form
		print "2"
		images_list = tomoImages.objects.all()
		template = loader.get_template('tomographic_db/home.html')
		context = RequestContext(request,{'images_list': images_list,'new_TomoImages': new_TomoImages,})
		return HttpResponse(template.render(context))

def vote(request, imageName_id):
    p = get_object_or_404(tomoImages, pk=imageName_id)
    try:
        selected_choice = p.choice_set.get(pk=request.POST['choice'])
    except (KeyError, Choice.DoesNotExist):
        # Redisplay the question voting form.
        return render(request, 'polls/detail.html', {
            'question': p,
            'error_message': "You didn't select a choice.",
        })
    else:
        selected_choice.votes += 1
        selected_choice.save()
        # Always return an HttpResponseRedirect after successfully dealing
        # with POST data. This prevents data from being posted twice if a
        # user hits the Back button.
        return HttpResponseRedirect(reverse('polls:results', args=(p.id,)))