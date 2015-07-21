from django.db import models

# Create your models here.

class tomoImages(models.Model):
    imageName = models.CharField(max_length=200)
    image = models.ImageField(upload_to='JGR_figures')
    def __unicode__(self):              # __unicode__ on Python 2
        return self.imageName
#    def __unicode__(self):              # __unicode__ on Python 2
#        return self.imageUrl
	def image(self):    
	    return '<img src="%s" height="150"/>' % (self.image)
class globeImages(models.Model):
    globeName = models.CharField(max_length=200)
    globeImage = models.ImageField(upload_to='JGR_figures')
    def __unicode__(self):              # __unicode__ on Python 2
        return self.globeName
	def globeImage(self):    
		return '<img src="%s" height="150"/>' % (self.globeImage)

 
