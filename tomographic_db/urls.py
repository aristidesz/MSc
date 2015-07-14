from django.conf.urls import url
from django.conf.urls.static import static
from .import views
from django.conf import settings
from django.conf import settings
from django.conf.urls import patterns

urlpatterns = [
	url(r'^$', views.index, name='index'),
	url(r'^image/$', views.image, name='image'),
	url(r'^download/(?P<file_id>\d+)/?', 'views.download_file', 'myapp_download_file'),
	url(r'^(?P<id_imageName>[0-9]+)/image/$', views.image, name='image'),
]+ static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)

urlpatterns += patterns('',
    url(r'^media/(?P<path>.*)$', 'django.views.static.serve', {'document_root': settings.MEDIA_ROOT, 'show_indexes': False}),
)
