# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('tomographic_db', '0007_tomoimages_image'),
    ]

    operations = [
        migrations.CreateModel(
            name='globeImages',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('globeName', models.CharField(max_length=200)),
                ('globeImage', models.ImageField(upload_to=b'JGR_figures')),
            ],
        ),
    ]
