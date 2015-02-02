def classify_args(arg_lst):
	normal_args = []
	pair_args = {}
	i = 0
	while not _is_out_of_index(i, arg_lst):
		s = arg_lst[i]
		i += 1
		if s.startswith('-'):
			s = s[1:]
			if option_arities[s]:
				arg = arg_lst[i]
				i += 1
				pair_args[s] = arg
			else:
				pair_args[s] = "YES"
		else:
			normal_args.append(s)
	(default_dict, entry_args) = parse_default(normal_args)
	return (_merge_dict(default_dict, pair_args), entry_args)

def _merge_dict(dict1, dict2):
	for key, value in dict2.iteritems():
		dict1[key] = value
	return dict1

# for arguments that without name
def parse_default(arg_lst):
	res = default_args_dict
	entry_args = []
	dummy = arg_lst[0]
	if _is_out_of_index(1, arg_lst):
		print_usage()
		return (res, entry_args)
	else:
		file = arg_lst[1]
		res["file"] = file
	if _is_out_of_index(2, arg_lst):
		res["entry"] = "start"
	else:
		res["entry"] = arg_lst[2]
	if not _is_out_of_index(3, arg_lst):
		entry_args = arg_lst[3:]
	return (res, entry_args)

def _is_out_of_index(i, arg_lst):
	return i >= len(arg_lst)

option_names = {
		"s" : "single scheduler",
		"r" : "reduction time",
		}

option_arities = {
		"s" : False,
		"r" : True,
		}

default_args_dict = {
		"r" : "2000",
		"s" : "",
		}

def print_usage():
	print "[usage] [-s] [-r : number] file_name [funtion name] [arguments ...]"

if __name__ == '__main__':
	import sys
	print classify_args(sys.argv)
