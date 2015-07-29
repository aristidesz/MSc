# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('tomographic_db', '0010_gmtimages_gmtdepth'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='gmtimages',
            name='GMTDepth',
        ),
    ]
