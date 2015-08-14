from django.test import TestCase
from tomographic_db.models import tomoImages
from tomographic_db.models import globeImages
from tomographic_db.models import gmtImages

class ImagesTestCase(TestCase):
    def setUp(self):
        t = tomoImages.objects.create(imageName="lion", image="roar")
        t = tomoImages.objects.create(imageName="cat", image="meow")
        t.save()
        g=gmtImages.objects.create(gmtImageName="Hello", gmtImage="roar")
        g.save()
        i=globeImages.objects.create(globeName="Hello", globeImage="roar")
        i.save()
    def test_tomoImages(self):
        """Animals that can speak are correctly identified"""
        t = tomoImages.objects.get(imageName="lion")
        
    def test_Images(self):
        """Animals that can speak are correctly identified"""
        i = globeImages.objects.get(globeName="Hello")

    def test_GMT(self):
        """Animals that can speak are correctly identified"""
        g = gmtImages.objects.get(gmtImageName="Hello")