from django import forms

class FormTomoImages(forms.Form):
    form_imageName = forms.CharField(max_length=100)

class FormGlobeImages(forms.Form):
    form_globeName = forms.CharField(max_length=100)

class FormGMTImage(forms.Form):
    form_latMAX = forms.CharField(max_length=100)
    form_latMIN = forms.CharField(max_length=100)
    form_lonMAX = forms.CharField(max_length=100)
    form_lonMIN = forms.CharField(max_length=100)
    form_Depth = forms.CharField(max_length=100)