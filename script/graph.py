import matplotlib.pyplot as plt
import statistics

def paint(res_dict):
	sorted_keys = res_dict.keys()
	sorted_keys.sort()
	sorted_values = []
	for key in sorted_keys:
		sorted_values.append(res_dict[key])
	
	plt.bar(range(len(res_dict)), sorted_values, align='center')
	plt.xticks(range(len(res_dict)), sorted_keys)
	plt.show()

def plot(x_lst, y_lst):
	plt.bar(range(len(y_lst)), y_lst, align='center')
	plt.xticks(range(len(x_lst)), x_lst)
	plt.show()

if __name__ == '__main__':
	x_lst = range(1, 21)
	y_lst = [3433004, 3403950, 3409848, 3408138, 3406272, 3405926, 3417053, 3406093, 3405864, 3400331, 3409654, 3406062, 3404962, 3411290, 3400919, 3407434, 3412618, 3405966, 3402336, 3412689]
	m = statistics.mean(y_lst)
	plot(x_lst, [y * 100.0 / m - 100.0 for y in y_lst])
