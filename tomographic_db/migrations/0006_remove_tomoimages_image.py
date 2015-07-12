# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('tomographic_db', '0005_auto_20150628_1657'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='tomoimages',
            name='image',
        ),
    ]
