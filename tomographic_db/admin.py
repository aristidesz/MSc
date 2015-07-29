from django.contrib import admin
from .models import tomoImages,globeImages, GMTImages

# Register your models here.

admin.site.register(tomoImages)
admin.site.register(globeImages)
admin.site.register(GMTImages)