import re
import subprocess
import time

repeat = 1

benchmarks = {
			"ack" :     110,
			"cpstak" :  500,
			"deriv" :   30000000,
			"diviter" : 8000000,
			"fib" :     50,
			"fibfp" :   50,
			"fpsum" :   2500,
			"mazefun" : 10000,
			"nqueens" : 5000,
			"pi" :      50,
			"primes" :  1000000,
			"string" :  3,
			"sumLoop" : 50,
			"tak" :     20000,
			"takl" :    5000
			}

benchmark_path = 'benchmarks/larceny'
benchmark_func_name = 'run_benchmark'
benchmark_func_full_name = benchmark_func_name + '/1'

def insert_export(lines):
	for i,l in enumerate(lines):
		reg_res = re.search('-export\(\[(.*)\]\).', l)
		if reg_res:
			export_funs = [x.strip() for x in reg_res.group(1).split(",")]
			#print export_funs, benchmark_func_full_name, benchmark_func_full_name in export_funs
			if benchmark_func_full_name in export_funs:
				return
			else:
				export_funs.append(benchmark_func_full_name)
				lines[i] = "-export([%s])."%(", ".join(export_funs))
				return

def append_bench_func(lines):
	for i,l in enumerate(lines):
		reg_res = re.search(benchmark_func_name+"\(N\)", l)
		if reg_res:
			return
	lines.append(benchmark_func_name+"([Arg]) -> "+ benchmark_func_name + "(list_to_integer(Arg));")
	lines.append(benchmark_func_name+"(0) -> true;")
	lines.append(benchmark_func_name+"(N) -> test()," + benchmark_func_name + "(N-1).\n")

def rewrite_and_compile():
	print "transform and compiling..."
	err_count = 0
	for b in benchmarks.keys():
		f_name = benchmark_path + "/" + b + ".erl"
		f = open(f_name,"r")
		lines = f.read().split("\n")
		insert_export(lines)
		append_bench_func(lines)
		f.close()
		f = open(f_name, "w+")
		f.write("\n".join(lines))
		f.close()
		cmd_lst = ["erlc", "-o", benchmark_path, f_name]
		p = subprocess.Popen(cmd_lst, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
		out, err = p.communicate()
		if err:
			print err
			err_count += 1
		cmd_lst = ["ERL_AFLAGS=\'-smp disable\'", "erlc", "-o", benchmark_path + "_hipe", "+native", "+\"{hipe, [o3]}\"", f_name]
		p = subprocess.Popen(cmd_lst, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
		out, err = p.communicate()
		if err:
			print err
			err_count += 1
	print "compile completed with %d error"%(err_count)

def run_bench(bin):
	print "begin running benchmark with repeat value %d..."%repeat
	sorted_keys = benchmarks.keys()
	sorted_keys.sort()
	res = {}
	for b in sorted_keys:
		if bin == 'erl':
			cmd_lst = [bin,
					"-run",
					b,
					benchmark_func_name,
					str(benchmarks[b]),
					"-smp",
					"disable",
					"-run",
					"init",
					"stop",
					"-noshell",
					"-pa",
					benchmark_path]
		elif bin == "hipe":
			cmd_lst = [bin,
					"-run",
					b,
					benchmark_func_name,
					str(benchmarks[b]),
					"-smp",
					"disable",
					"-run",
					"init",
					"stop",
					"-noshell",
					"-pa",
					benchmark_path+"_hipe"]
		else:
			cmd_lst = ["./"+bin, "-s", benchmark_path + "/" + b + ".beam", benchmark_func_name, str(benchmarks[b])]
		t_res = []
		for i in range(repeat):
			t1 = time.time()
			p = subprocess.Popen(cmd_lst, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
			out, err = p.communicate()
			if err:
				print err
				return
			t2 = time.time()
			t_res.append(t2-t1)
		print "benchmark %s: executing time %f"%(b, float(sum(t_res)) / repeat)
		res[b] = float(sum(t_res) / repeat)
	sorted_keys = res.keys()
	sorted_keys.sort()
	f = open("benchmark_result/" + bin + ".txt", "w+")
	for k in sorted_keys:
		f.write(k + " " + str(res[k]) + "\n")
	f.close()
	print "done"

if __name__ == '__main__':
	import sys
	if len(sys.argv) > 1:
		bin = sys.argv[1]
	else:
		bin = 'targettest-c'

	rewrite_and_compile()
	run_bench(bin)
