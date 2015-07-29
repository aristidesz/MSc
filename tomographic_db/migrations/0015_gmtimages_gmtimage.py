# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('tomographic_db', '0014_auto_20150729_1342'),
    ]

    operations = [
        migrations.AddField(
            model_name='gmtimages',
            name='GMTImage',
            field=models.ImageField(default='url', upload_to=b'GMT_Images'),
            preserve_default=False,
        ),
    ]
