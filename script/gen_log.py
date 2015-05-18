import os
import sys
sys.path.append("../")

pre='PYPYLOG=jit-summary:unroll_lock_log/unroll_sum_'
#pre='PYPYLOG=jit-log-opt,jit-backend-counts:counting_log/counting_counts_'
#ext='log'
ext='sum'
#bin = './pyrlang-normal -s'
bin = './pyrlang-5-6-3 -s'
#file = 'test_beam/recursive.beam'
#file = 'test_beam/hanoi.beam'
#file = 'test_beam/counting.beam'
file = 'test_beam/fib.beam'
#file = 'test_beam/ts.beam'
#file = 'test_beam/ack.beam'
func = 'test'
args = [str(n) for n in range(30,44)]
step = 1
#args = [str(n) for n in range(1000000, 11000000, 1000000)]
#step = 1000
#args = [str(n) for n in range(10000, 31000, step)]

def begin_gen_log():
	for arg in args:
		cmd_lst = [pre+arg+"."+ext, bin, file, func, arg]
		cmd = ' '.join(cmd_lst)
		os.system(cmd)
	print "ok"

if __name__ == '__main__':
	begin_gen_log()
