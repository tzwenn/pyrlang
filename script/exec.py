import os
import sys
import time
import graph
sys.path.append("../")

bin = './pyrlang-fair-thread'
#file = 'test_beam/fact.beam'
#func = 'fact'
#args = ['20000000']
file = 'test_beam/reverse.beam'
func = 'start'
args = ['20000']
bin_args = '-r'

def begin_reduction_benchmark():
	res_dict = {}
	for r in range(0, 40 + 2, 2):
		t1 = time.time()
		cmd_lst = [bin, file, bin_args, str(r), func] + args
		cmd = ' '.join(cmd_lst)
		os.system(cmd)
		t2 = time.time()
		res_dict[r] = t2 - t1
	print res_dict
	print "**********************************"
	for reduction, etime in res_dict.iteritems():
		print str(reduction) + " " + str(etime)
	print "**********************************"
	graph.paint(res_dict)

def begin_scale_benchmark(bin_as_arg):
	res_dict = {}
	scale_num = int(args[0])
	for s in range(scale_num, scale_num * 11, scale_num):
		t1 = time.time()
		cmd_lst = [bin_as_arg, file, func, str(s)]
		cmd = ' '.join(cmd_lst)
		os.system(cmd)
		t2 = time.time()
		res_dict[s] = t2 - t1
	for scale, etime in res_dict.iteritems():
		print str(scale) + " " + str(etime)
	return res_dict

if __name__ == '__main__':
	begin_reduction_benchmark()
	#res1 = begin_scale_benchmark('./targettest-c')
	#res2 = begin_scale_benchmark('./pyrlang-fair-thread')
	#graph.paint(res1)
	#graph.paint(res2)
