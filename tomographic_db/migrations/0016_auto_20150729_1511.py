# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('tomographic_db', '0015_gmtimages_gmtimage'),
    ]

    operations = [
        migrations.RenameField(
            model_name='gmtimages',
            old_name='GMTImageName',
            new_name='gmtImageName',
        ),
        migrations.RemoveField(
            model_name='gmtimages',
            name='GMTImage',
        ),
        migrations.AddField(
            model_name='gmtimages',
            name='gmtImage',
            field=models.ImageField(default='1001111', upload_to=b'JGR_figures'),
            preserve_default=False,
        ),
    ]
