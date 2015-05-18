import statistics
import math
import sys
import numpy
sys.path.append('/Users/huangruochen/src/python/jitviewer/_jitviewer')
from jitcounter import counting_file

theme = 'unroll'

def norm(lst):
	m = statistics.mean(lst)
	return [e/m for e in lst]

def read_time_data(filename):
	f = open(filename, "rb")
	return [float(e) for e in filter(lambda x: not x.startswith("#"), f.readlines())]

def norm_fib_time(lst, _from, _to):
	res = []
	if _to - _from == len(lst):
		for i in range(_from, _to):
			res.append(lst[i-_from]/pow(1.6, i) * 100000000)
		return norm(res)
	else:
		raise Exception("[error]invalid range: except range which length is %d"%(len(lst)))

def standard_deviation(lst):
	return math.sqrt(statistics.variance(lst))

def print_raw(name, data):
	print ", ".join([str(e) for e in data]) + ";"

def gen_matlab(datas):
	f = open("%s_log/%s.m"%(theme,theme), "wb")
	time = datas[0]
	f.write("%s=[%s];\n"%(time[0], ";".join([str(e) for e in time[1]])))
	datas = datas[1:]
	for name, data in datas:
		f.write("%s=[%s];\n"%(name, ",".join([str(e) for e in data])))
	f.write("X = [bridge; trace; all; opt; byte_code; guard; allocation; allocation_and_guard];\n")
	f.write("stepwisefit(X', time)\n")
	f.close()

def find_jump(filename):
	f = open(filename, "rb")
	trace_jump_number = 0
	bridge_jump_number = 0
	recording = False
	for line in f.readlines():
		#print "line:", line
		if line.find("{jit-backend-counts") != -1:
			recording = True
		elif line.find("jit-backend-counts}") != -1:
			recording = False
		if recording:
			if line.startswith("TargetToken"):
				trace_jump_number += int(line.split(":")[-1])
			elif line.startswith("bridge"):
				bridge_jump_number += int(line.split(":")[-1])
	f.close()
	return trace_jump_number, bridge_jump_number

from_scale = 40
to_scale = 57
step = 1
#from_scale = 10000
#to_scale = 30000
#step = 1000

if __name__ == '__main__':
	t_lst = []
	b_lst = []
	a_lst = []
	opt_lst = []

	# executing time in "three switching", from 33 to 52.
	#time_lst = [0.105861028035,
			#0.147120634715, 0.234294017156, 0.36473997434, 0.476224422455, 0.823757727941, 1.32534996668, 2.1247130235, 3.4408967495, 6.34565138817, 8.9287463824, 15.886149327, 20.5545713902,
	#37.3077030182,
	#60.3575621446,
	#97.4623932044,
	#176.649143378,
	#228.683529377,
	#418.632503351,
	#665.890812953]

	# executing time_lst in "two switching", from 33 to 52.
	#time_lst = [0.0892406304677,
			#0.137968301773,
			#0.196089744568,
			#0.353085915248,
			#0.486348708471,
			#0.750188589096,
			#1.18845065435,
			#1.91063825289,
			#3.07317503293,
			#5.17512027423,
			#8.00197863579,
			#15.5324643453,
			#21.1049114068,
			#35.1070570151,
			#54.324908336,
			#87.7742576599,
			#173.996048292,
			#233.635377089,
			#375.362902959,
			#605.6314044]

	o_n_lst = [53798080,
			98950096,
			181997601,
			334745777,
			615693474,
			1132436852,
			2082876103,
			3831006429,
			7046319384,
			12960201916,
			23837527729,
			43844049029,
			80641778674,
			148323355432]
	#o_n_lst = [300100006,
				#363110006,
				#432120006,
				#507130006,
				#588140006,
				#675150006,
				#768160006,
				#867170006,
				#972180006,
				#1083190006,
				#1200200006,
				#1323210006,
				#1452220006,
				#1587230006,
				#1728240006,
				#1875250006,
				#2028260006,
				#2187270006,
				#2352280006,
				#2523290006,
				#2700300006]
	time_lst = read_time_data("%s_log/time.data"%(theme,))
	time_lst = time_lst[7:]
	byte_lst = []
	guard_lst = []
	allocate_lst = []
	allocate_and_guard_lst = []
	for i in range(from_scale, to_scale + step, step):
		file_name = "%s_log/%s_counts_%d.log"%(theme, theme, i)
		o_n = pow(1.6, i)
		index = (i - from_scale) / step
		#o_n = float(o_n_lst[index])
		time_lst[index] = time_lst[index]/o_n * 100000000
		t,b = find_jump(file_name)
		count = counting_file(file_name, 'debug_merge_point', False)
		t_lst.append(t / o_n)
		b_lst.append(b / o_n)
		a_lst.append((t + b) / o_n)
		opt_lst.append(count / o_n)
		count = counting_file(file_name, 'debug_merge_point', True, True)
		byte_lst.append(count / o_n)
		count = counting_file(file_name, '')
		guard_lst.append(count / o_n)
		count = counting_file(file_name, 'new_with_vtable', True, True)
		allocate_lst.append(count / o_n)
		count = counting_file(file_name, 'new_with_vtable', True)
		allocate_and_guard_lst.append(count / o_n)
	nts = norm(t_lst)
	nbs = norm(b_lst)
	nas = norm(a_lst)
	nps = norm(opt_lst)
	n_bytes = norm(byte_lst)
	n_guards = norm(guard_lst)
	n_allocations = norm(allocate_lst)
	n_aags = norm(allocate_and_guard_lst)
	ntimes = norm(time_lst)
	print "scale\ttime\tbridge\ttrace\tall\topts\tbyte code\tguard\tallocation\tallocation and guard"
	for i in range(len(t_lst)):
		print "%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f"%(from_scale+i, ntimes[i], nbs[i], nts[i], nas[i], nps[i], n_bytes[i], n_guards[i], n_allocations[i], n_aags[i])
	datas = [('time', ntimes), ('bridge', nbs), ('trace', nts), ('all', nas), ('opt', nps), ('byte_code', n_bytes), ('guard', n_guards), ('allocation', n_allocations), ('allocation_and_guard', n_aags)]
	gen_matlab(datas)
	print numpy.corrcoef(ntimes, nbs)[0, 1]
	print numpy.corrcoef(ntimes, nts)[0, 1]
	print numpy.corrcoef(ntimes, nas)[0, 1]
	print numpy.corrcoef(ntimes, nps)[0, 1]
	print numpy.corrcoef(ntimes, n_bytes)[0, 1]
	print numpy.corrcoef(ntimes, n_guards)[0, 1]
	print numpy.corrcoef(ntimes, n_allocations)[0, 1]
	print numpy.corrcoef(ntimes, n_aags)[0, 1]
