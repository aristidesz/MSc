from django.conf.urls import url
from django.conf.urls.static import static
from .import views
from tomographic_db import views
from django.conf import settings
from django.conf import settings
from django.conf.urls import patterns
from tomographic_db.views import *

urlpatterns = [
	url(r'^$', views.index, name='index'),
	url(r'^image/$', views.image, name='image'),
	url(r'^datamethod/$', views.datamethod, name='datamethod'),
	url(r'^about/$', views.about, name='about'),
	url(r'^image/(?P<id>[0-9]+)/$', views.imageWithID, name='imageWithID'),
	url(r'^download/(?P<file_id>\d+)/?', 'views.download_file', 'myapp_download_file'),
	url(r'^(?P<id_imageName>[0-9]+)/image/$', views.image, name='image'),
]+ static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)

urlpatterns += patterns('',
    url(r'^media/(?P<path>.*)$', 'django.views.static.serve', {'document_root': settings.MEDIA_ROOT, 'show_indexes': False}),
)
