# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('tomographic_db', '0008_globeimages'),
    ]

    operations = [
        migrations.CreateModel(
            name='GMTImages',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('GMTImageName', models.CharField(max_length=200)),
                ('GMTlatMAX', models.CharField(max_length=200)),
                ('GMTlatMIN', models.CharField(max_length=200)),
                ('GMTlonMAX', models.CharField(max_length=200)),
                ('GMTlonMIN', models.CharField(max_length=200)),
            ],
        ),
    ]
