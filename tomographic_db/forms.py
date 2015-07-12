from django import forms

class FormTomoImages(forms.Form):
    form_imageName = forms.CharField(max_length=100)

