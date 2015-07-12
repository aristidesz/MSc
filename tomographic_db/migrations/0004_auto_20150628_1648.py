# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('tomographic_db', '0003_tomoimages_imagename'),
    ]

    operations = [
        migrations.AlterField(
            model_name='tomoimages',
            name='imageUrl',
            field=models.ImageField(upload_to=b'JGR_figures'),
        ),
    ]
