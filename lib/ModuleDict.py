from pyrlang.lib import ErlangModule
from pyrlang.lib import ListsModule

module_dict = {
		"erlang" : ErlangModule.ModuleEntity,
		"lists" : ListsModule.ModuleEntity,
		}

def is_bif_from_tuple(name_entry):
	return is_bif(name_entry[0], name_entry[1], name_entry[2])

def is_bif(module_name, func_name, arity):
	if module_name in module_dict:
		return get_bif_name(func_name, arity) in module_dict[module_name]._func_dict
	else:
		return False

def get_bif_name(func_name, arity):
	return "%s_%d"%(func_name, arity)
