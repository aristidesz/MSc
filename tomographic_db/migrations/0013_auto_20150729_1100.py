# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('tomographic_db', '0012_gmtimages_gmtdepth'),
    ]

    operations = [
        migrations.AlterField(
            model_name='gmtimages',
            name='GMTDepth',
            field=models.CharField(max_length=200),
        ),
    ]
