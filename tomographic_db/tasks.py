from __future__ import absolute_import

from celery import shared_task
from celery.task import task
from celery.task.sets import subtask
import subprocess
from tomographic_db.models import gmtImages

@shared_task
def add(a, b):
	return a + b

@shared_task
def mul(x, y):
    return x * y


@shared_task
def xsum(numbers):
    return sum(numbers)

@shared_task
def subpro(command, name, depth,GMTlonMAX,GMTlonMIN,GMTlatMAX,GMTlatMIN):
	MyImageName = name
	depth1 = int(float(depth))
	print depth1
	REG='-Rg'
	PROJ = '-JKs160/7.0'
	print "kosti" +MyImageName
	print GMTlonMAX,GMTlonMIN,GMTlatMAX,GMTlatMIN
	print REG

	process = subprocess.Popen(command.format(REG,PROJ,MyImageName,depth1,GMTlonMAX,GMTlonMIN,GMTlatMAX,GMTlatMIN),executable="/bin/csh",stdout=subprocess.PIPE, shell=True)
	proc_stdout = process.communicate()[0].strip()
	print proc_stdout

