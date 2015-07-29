# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('tomographic_db', '0009_gmtimages'),
    ]

    operations = [
        migrations.AddField(
            model_name='gmtimages',
            name='GMTDepth',
            field=models.CharField(default=100, max_length=200),
            preserve_default=False,
        ),
    ]
