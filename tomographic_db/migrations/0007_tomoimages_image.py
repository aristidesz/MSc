# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
import datetime
from django.utils.timezone import utc


class Migration(migrations.Migration):

    dependencies = [
        ('tomographic_db', '0006_remove_tomoimages_image'),
    ]

    operations = [
        migrations.AddField(
            model_name='tomoimages',
            name='image',
            field=models.ImageField(default=datetime.datetime(2015, 6, 29, 10, 15, 30, 325648, tzinfo=utc), upload_to=b'JGR_figures'),
            preserve_default=False,
        ),
    ]
