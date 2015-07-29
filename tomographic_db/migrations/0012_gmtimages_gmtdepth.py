# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('tomographic_db', '0011_remove_gmtimages_gmtdepth'),
    ]

    operations = [
        migrations.AddField(
            model_name='gmtimages',
            name='GMTDepth',
            field=models.ImageField(default=100, upload_to=b''),
            preserve_default=False,
        ),
    ]
