# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('tomographic_db', '0004_auto_20150628_1648'),
    ]

    operations = [
        migrations.RenameField(
            model_name='tomoimages',
            old_name='imageUrl',
            new_name='image',
        ),
    ]
