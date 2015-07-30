from django import forms

class images(forms.Form):
    imageNameForm = forms.CharField(label='ImageNameForm', max_length=100)
