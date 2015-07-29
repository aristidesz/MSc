from __future__ import absolute_import

from celery import shared_task
import subprocess

@shared_task
def add(x, y):
    return x + y


@shared_task
def mul(x, y):
    return x * y


@shared_task
def xsum(numbers):
    return sum(numbers)

@shared_task
def subpro(command, name):
	MyImageName = name
	REG='-Rg'
	PROJ = '-JKs160/7.0'
	print "Hello" +MyImageName
	print REG
	process = subprocess.Popen(command.format(REG,PROJ,MyImageName),executable="/bin/csh",stdout=subprocess.PIPE, shell=True)
	proc_stdout = process.communicate()[0].strip()
	print proc_stdout
	