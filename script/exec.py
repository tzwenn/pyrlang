import os
import sys
import time
import statistics
import math
#import graph
sys.path.append("../")

#bin = './pyrlang-5-6-3 -s'
bin = './targettest-c -s'
#bin = './pyrlang-normal -s'
#file = 'test_beam/append.beam'

#file = 'test_beam/counting.beam'
#file = 'test_beam/unroll.beam'
#file = 'test_beam/genpath.beam'
file = 'test_beam/fib.beam'
#file = 'test_beam/reverse.beam'
#func = 'start'
func = 'test'
step = 1
#step = 5000
args = [str(n) for n in range(40,50, step)]
#args = [str(n) for n in range(17,20, step)]
#args = [str(n) for n in range(20000, 50000, step)]
#target_dir = 'gen_pa_lock_log/'
target_dir = 'fib_lock_log/'

#file = 'test_beam/ack.beam'
#target_dir = 'ack_log/'
#step = 1000
#args = [str(n) for n in range(10000, 31000, step)]

bin_args = '-s'

exec_counfib = 3

#def begin_reduction_benchmark():
	#res_dict = {}
	#for r in range(0, 40 + 2, 2):
		#t1 = time.time()
		#cmd_lst = [bin, file, bin_args, str(r), func] + args
		#cmd = ' '.join(cmd_lst)
		#os.system(cmd)
		#t2 = time.time()
		#res_dict[r] = t2 - t1
	#print res_dict
	#print "**********************************"
	#for reduction, etime in res_dict.iteritems():
		#print str(reduction) + " " + str(etime)
	#print "**********************************"
	#graph.paint(res_dict)

def variance_benchmark(arg):
	scale = 20
	res_lst = []
	for i in range(scale + 1):
		t1 = time.time()
		cmd_lst = [bin, bin_args, file, func, arg]
		cmd = ' '.join(cmd_lst)
		os.system(cmd)
		t2 = time.time()
		res_lst.append(t2 - t1)
		if i > 2:
			print "times: %d variance/mean: %f"%(i, math.sqrt(statistics.variance(res_lst)) * 100.0 / statistics.mean(res_lst)) + "%"

def begin_scale_benchmark():
	res_dict = {}
	for s in args:
		res_lst = []
		for i in range(exec_counfib):
			t1 = time.time()
			cmd_lst = [bin, bin_args, file, func, s]
			cmd = ' '.join(cmd_lst)
			os.system(cmd)
			t2 = time.time()
			res_lst.append(t2 - t1)
		res_dict[int(s)] = sum(res_lst) / float(exec_counfib)
	sorted_keys = res_dict.keys()
	sorted_keys.sort()
	for scale in sorted_keys:
		print str(scale) + " " + str(res_dict[scale])
	f = open(target_dir + 'time.data', 'a')
	f.write("# %s to %s, step: %d\n"%(args[0], args[-1], step))
	f.write("\n".join([str(res_dict[e]) for e in sorted_keys]))
	f.close()
	return res_dict

if __name__ == '__main__':
	#begin_reduction_benchmark()
	begin_scale_benchmark()
	#variance_benchmark(sys.argv[1])
