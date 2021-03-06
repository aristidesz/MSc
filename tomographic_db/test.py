from django.test import TestCase
from tomographic_db.models import tomoImages
from tomographic_db.models import globeImages
from tomographic_db.models import gmtImages
from tomographic_db.forms import FormTomoImages
from tomographic_db.forms import FormGlobeImages
from tomographic_db.forms import FormGMTImage
from tomographic_db.tasks import subpro
from django.test import Client
import unittest
from django.conf import settings
from django.test import LiveServerTestCase
from selenium.webdriver.firefox.webdriver import WebDriver
from django.contrib.staticfiles.testing import StaticLiveServerTestCase
from django.contrib.staticfiles.storage import staticfiles_storage #The file storage engine to use when collecting static files with the collectstatic management command.
from django.contrib.staticfiles import finders
from selenium import webdriver
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
import unittest, time, re

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

class MyTests(TestCase):
    def test_forms(self):
        form_data = {'form_imageName': 'something'}
        form = FormTomoImages(data=form_data)
        self.assertTrue(form.is_valid())
    def test_forms1(self):
        form_data = {'form_globeName': 'something'}
        form = FormGlobeImages(data=form_data)
        self.assertTrue(form.is_valid())
    def test_forms2(self):
        form_data = {'form_latMAX': 'something','form_latMIN': 'something','form_lonMAX': 'something','form_lonMIN': 'something','form_Depth': 'something'}
        form = FormGMTImage(data=form_data)
        self.assertTrue(form.is_valid())


class MyTestsSubpro(TestCase):
    def test_sub(self):
        gmtImageName='111'
        prc1=subpro.delay('''
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
            psxy {0} {1} boundaries.xy -Bg60/g30wsen+ -W0.25p,grey  -O -K >> $OUTFILEmy.ps;
            echo "1.5 0.5 $dept[$i] km" |pstext -R0/7.5/0/3.75 -JX7.5/3.75 -F+f10,Helvetica+jMC -Gwhite -W1p -N -O -K >> $OUTFILEmy.ps;
            echo "0.1 3.5 ($lett[$i])" |pstext -R0/7.5/0/3.75 -JX7.5/3.75 -F+f12,Helvetica-Bold+jTL -N -O -K >> $OUTFILEmy.ps;
            rm $OUTFILEmy.eps;
            ps2eps -R=+ $OUTFILEmy.ps;
            convert $OUTFILEmy.eps $OUTFILEmy.jpg
            rm $OUTFILEmy.eps;
            rm $OUTFILEmy.ps tmp.*;
            ''',gmtImageName)

class SimpleTest(unittest.TestCase):
    def test_details1(self):
        client = Client()
        response = client.get('/')
        self.assertEqual(response.status_code, 200)

    def test_details2(self):
        client = Client()
        response = client.get('/download/')
        self.assertEqual(response.status_code, 200)

    def test_details3(self):
        client = Client()
        response = client.get('/tomographic_db/image/')
        self.assertEqual(response.status_code, 200)

    def test_details4(self):
        client = Client()
        response = client.get('')
        self.assertEqual(response.status_code, 200)

    def test_details5(self):
        client = Client()
        response = client.get('/tomographic_db/datamethod/')
        self.assertEqual(response.status_code, 200)

    def test_details6(self):
        client = Client()
        response = client.get('/download/downloadfile/')
        self.assertEqual(response.status_code, 200)


class Seleniumpython(unittest.TestCase):
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://localhost:8000/"
        self.verificationErrors = []
        self.accept_next_alert = True
    
    def test_seleniumpython(self):
        driver = self.driver
        driver.get(self.base_url + "/")
        driver.find_element_by_css_selector("button.button").click()
        driver.find_element_by_css_selector("button.button").click()
        Select(driver.find_element_by_id("lonMAX")).select_by_visible_text("19")
        Select(driver.find_element_by_id("lonMAX")).select_by_visible_text("19")
        Select(driver.find_element_by_id("lonMIN")).select_by_visible_text("19")
        Select(driver.find_element_by_id("lonMIN")).select_by_visible_text("19")
        Select(driver.find_element_by_id("latMAX")).select_by_visible_text("11")
        Select(driver.find_element_by_id("latMAX")).select_by_visible_text("11")
        Select(driver.find_element_by_id("latMIN")).select_by_visible_text("2")
        Select(driver.find_element_by_id("latMIN")).select_by_visible_text("2")
        driver.find_element_by_css_selector("#formwrapper > form > input.btn").click()
        driver.find_element_by_css_selector("#formwrapper > form > input.btn").click()
        driver.find_element_by_css_selector("button.button").click()
        driver.find_element_by_css_selector("button.button").click()
        driver.find_element_by_css_selector("input.btn").click()
        driver.find_element_by_css_selector("input.btn").click()
        driver.find_element_by_css_selector("button.button").click()
        driver.find_element_by_css_selector("button.button").click()
        Select(driver.find_element_by_name("form_imageName")).select_by_visible_text("200KmRani")
        Select(driver.find_element_by_name("form_imageName")).select_by_visible_text("200KmRani")
        driver.find_element_by_xpath("(//input[@value='Submit'])[2]").click()
        driver.find_element_by_xpath("(//input[@value='Submit'])[2]").click()
        driver.find_element_by_css_selector("button.button").click()
        driver.find_element_by_css_selector("button.button").click()
    
    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException, e: return False
        return True
    
    def is_alert_present(self):
        try: self.driver.switch_to_alert()
        except NoAlertPresentException, e: return False
        return True
    
    def close_alert_and_get_its_text(self):
        try:
            alert = self.driver.switch_to_alert()
            alert_text = alert.text
            if self.accept_next_alert:
                alert.accept()
            else:
                alert.dismiss()
            return alert_text
        finally: self.accept_next_alert = True
    
    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

if __name__ == "__main__":
    unittest.main()
