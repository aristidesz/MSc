bashCommand ="./dept-slices.gmt"
import subprocess
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
output = process.communicate()[0]