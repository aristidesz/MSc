# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('tomographic_db', '0013_auto_20150729_1100'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='gmtimages',
            name='GMTDepth',
        ),
        migrations.RemoveField(
            model_name='gmtimages',
            name='GMTlatMAX',
        ),
        migrations.RemoveField(
            model_name='gmtimages',
            name='GMTlatMIN',
        ),
        migrations.RemoveField(
            model_name='gmtimages',
            name='GMTlonMAX',
        ),
        migrations.RemoveField(
            model_name='gmtimages',
            name='GMTlonMIN',
        ),
    ]
