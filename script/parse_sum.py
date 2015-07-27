from __future__ import print_function
import os

def num(s):
	try:
		return int(s)
	except ValueError:
		return float(s)

def file_to_dict(file_name):
	f = open(file_name, "rb")
	res = {}
	if f:
		lines = f.readlines()
		lines = lines[1:-1]
		for l in lines:
			arr = [s.strip().strip(":") for s in l.split("\t") if s]
			res[arr[0]] = tuple([num(s) for s in arr[1:]]) if len(arr) > 2 else num(arr[1])
	return res

def analyze_warming_time(d):
	tracing_time = d['Tracing'][1]
	backend_time = d['Backend'][1]
	total_time = d['TOTAL'] 
	native_time = total_time - tracing_time - backend_time
	return (tracing_time/total_time, backend_time/total_time, native_time/total_time)

def walker(path, bin):
	sum_list = [f for f in os.listdir(path) if f.startswith(bin)]
	res = {}
	for s in sum_list:
		k = s.split("_")[1].split(".")[0]
		res[k] = analyze_warming_time(file_to_dict(path + "/" + s))
	return res

def sort_iterator(d, f):
	sorted_keys = d.keys()
	sorted_keys.sort()
	for k in sorted_keys:
		f(k, d[k])

if __name__ == '__main__':
	f = lambda k, v: print(k + "\t" + "\t".join([str(v) for v in v]))
	res1 = walker("lancery_sum", "pyrlang-match")
	sort_iterator(res1, f)
