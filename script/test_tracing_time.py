import os
import sys
import time
import statistics
import graph
import subprocess
sys.path.append("../")

bin = './targettest-c'
#file = 'test_beam/fact.beam'
#func = 'fact'
#args = ['20000000']
file = 'test_beam/recursive.beam'
func = 'run_test'
#args = (3, 37)
bin_args = '-s'

exec_counts = 5

def variance_benchmark(args):
	cmd_lst = [bin, bin_args, file, func] + args[1:]
	print "executing command: %s, please wait..."%(" ".join(cmd_lst))
	res_lst_lst = []
	for j in range(3):
		print "running process at %d time:"%(j)
		p = subprocess.Popen(cmd_lst, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
		out, err = p.communicate()
		lines = out.split("\n")

		res_lst = []
		for i in range(len(lines)/2 - 1):
			res_lst.append(int(lines[i*2+1]))
		res_lst_lst.append(res_lst)

	one_time = res_lst_lst[0]
	res = []
	for j in range(len(one_time)):
		tmp = []
		for data in res_lst_lst:
			tmp.append(data[j])
		res.append(statistics.mean(tmp))
	print res

	graph.plot(range(1,int(args[1])+1), res)

if __name__ == '__main__':
	variance_benchmark(sys.argv)
