# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('tomographic_db', '0002_remove_tomoimages_idimage'),
    ]

    operations = [
        migrations.AddField(
            model_name='tomoimages',
            name='imageName',
            field=models.CharField(default=1, max_length=200),
            preserve_default=False,
        ),
    ]
