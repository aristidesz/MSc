from __future__ import absolute_import

import os

from celery import Celery

# set the default Django settings module for the 'celery' program.
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'MScFirst.settings')

from django.conf import settings

app = Celery('MScFirst')

# Using a string here means the worker will not have to
# pickle the object when using Windows.
app.config_from_object('django.conf:settings')
app.autodiscover_tasks(lambda: settings.INSTALLED_APPS)

app.conf.update(
	CELERY_RESULT_BACKEND="amqp",
)
app.conf.update(
    CELERY_TASK_RESULT_EXPIRES=3600,
)
app.conf.update(
	CELERY_ROUTES = {
		'MScFirst.tasks.subpro': {'queue': 'hipri'},
	},
)
@app.task(bind=True)
def debug_task(self):
	print('Request: {0!r}'.format(self.request))
