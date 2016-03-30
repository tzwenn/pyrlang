import re
import subprocess
import time
import os

repeat = 3

benchmarks = {
                        "barnes" : 5,
                        "length" : 2,
                        "length_c" : 3,
                        "length_u" : 3,
                        "mean" : 9,
                        "mean_nnc" :9,
                        "nrev" : 1,
                        "nucleic" : 96,
			"pseudoknot" : 1,
			"qsort" : 2,
			"ring" : 2,
			"smith" : 2,
			"stable" : 2,
			"sum" : 3,
			"zip" : 1,
			"zip3" : 1,
			"zip_nnc" : 1,
			}

benchmark_path = 'benchmarks/erllvm'
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
		my_env = os.environ.copy()
		my_env["ERL_AFLAGS"] = "-smp disable"
		cmd_lst = ["erlc", "-o", benchmark_path + "_hipe/", "+native", "+\"{hipe, [o3]}\"", f_name]
		#print " ".join(cmd_lst)
		p = subprocess.Popen(cmd_lst, env=my_env, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
		out, err = p.communicate()
		#print out
		if err:
			print err
			err_count += 1
	print "compile completed with %d error"%(err_count)

def run_bench(bin):
	print "begin running benchmark with binary %s, repeat value %d..."%(bin, repeat)
	sorted_keys = benchmarks.keys()
	sorted_keys.sort()
	res = {}
	for b in sorted_keys:
		if bin == 'erl':
			cmd_lst = ["/usr/bin/erl",
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
			cmd_lst = ["/usr/bin/erl",
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
					benchmark_path+"_hipe/"]
		else:
			cmd_lst = ["./"+bin, benchmark_path + "/" + b + ".beam", benchmark_func_name, str(benchmarks[b])]
		t_res = []
		my_env = os.environ.copy()
		for i in range(repeat):
			t1 = time.time()
			print " ".join(cmd_lst)
			#my_env["PYPYLOG"] = "jit-summary:erllvm_sum/"+bin+"_"+b+".sum"
			p = subprocess.Popen(cmd_lst, env=my_env, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
			out, err = p.communicate()
			#print out
			if err:
				print err
				return
			t2 = time.time()
			t_res.append(t2-t1)
		print "benchmark %s: executing time %f"%(b, float(sum(t_res)) / repeat)
		res[b] = float(sum(t_res) / repeat)
        sorted_keys = res.keys()
        sorted_keys.sort()
        f = open("benchmark_erllvm_result/" + bin + ".txt", "w+")
        for k in sorted_keys:
                f.write(k + " " + str(res[k]) + "\n")
        f.close()
	print "done"

if __name__ == '__main__':
	import sys
	if len(sys.argv) > 1:
		bin = sys.argv[1]
	else:
		bin = [
                        'pyrlang',
			#'pyrlang-less',
                        'pyrlang-naive',
                        'erl',
                        'hipe',
			]

	#rewrite_and_compile()
	if isinstance(bin, list):
		for b in bin:
			run_bench(b)
	else:
		run_bench(bin)
