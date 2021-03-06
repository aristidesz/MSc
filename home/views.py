from django.shortcuts import render

# Create your views here.
from django.shortcuts import render_to_response
from django.template import RequestContext
from django.template.loader import render_to_string
from django.http import HttpResponse
from django.http import HttpResponseRedirect
import os
from .forms import images
 
def index(request):
    return render_to_response('home/index.html', context_instance=RequestContext(request))
def get_name(request):
    # if this is a POST request we need to process the form data
    if request.method == 'POST':
        # create a form instance and populate it with data from the request:
        form = images(request.POST)
        # check whether it's valid:
        if form.is_valid():
            # process the data in form.cleaned_data as required
            # ...
            # redirect to a new URL:
            return HttpResponseRedirect('/thanks/')

    # if a GET (or any other method) we'll create a blank form
    else:
        form = images()

    return render(request, 'name.html', {'form': form})
def error500(request):
    return render_to_response('home/500.html', context_instance=RequestContext(request))
def error404(request):
    return render_to_response('home/404.html', context_instance=RequestContext(request))
def download(request):
    return render_to_response('home/download.html', context_instance=RequestContext(request))
def downloadfile(request):
    path_to_file = os.path.realpath("download/download.tar.gz")
    myfile = open(path_to_file, 'r')
    response = HttpResponse(myfile, content_type='application/vnd.ms-excel')
    response['Content-Disposition'] = 'attachment; filename=' + 'download.tar.gz'
    return response
    return render_to_response('home/download.html', context_instance=RequestContext(request))

