from pyrlang.utils.arg_parser import *
from pyrlang.utils import eterm_operators
from pyrlang.rpybeam import pretty_print
from pyrlang.rpybeam.beam_file import BeamRoot
from pyrlang.rpybeam.beam_code import CodeParser
from pyrlang.interpreter.interp import Process
from pyrlang.interpreter.datatypes.number import *
from pyrlang.interpreter.datatypes.list import W_StrListObject
from pyrlang.interpreter.pid_provider import PidProvider
from pyrlang.interpreter import constant
from pyrlang.interpreter.scheduler import Scheduler

class App:
	def __init__(self, args):
		(self.arg_pairs, self.entry_args) = classify_args(args)

	def launch(self):
		is_single_run = False
		parse_as_string = False
		default_reduction_counter = 2000

		for key, value in self.arg_pairs.iteritems():
			if key == 's' and value == "YES":
				is_single_run = True
			elif key == 'r':
				default_reduction_counter = int(value)
			elif key == 'run' and value == "YES":
				parse_as_string = True

		file_name = self.arg_pairs["file"]
		f = open(file_name, "rb")
		b = BeamRoot(f)
		pid_provider = PidProvider()
		scheduler = Scheduler(pid_provider, is_single_run, default_reduction_counter)
		main_process = Process(pid_provider.create_pid(), scheduler, constant.PRIORITY_NORMAL)
		scheduler.process_pool[main_process.pid] = main_process
		cp = CodeParser(b, file_name)
		#from pyrlang.interpreter.atom_table import global_atom_table
		#print global_atom_table._str_table
		func_addr = cp.get_func_addr_by_name(self.arg_pairs["entry"], len(self.entry_args))
		if parse_as_string:
			init_args = [eterm_operators.build_list_object([eterm_operators.build_strlist_object_from_string(arg) for arg in self.entry_args])]
		else:
			init_args = [W_IntObject(int(arg)) for arg in self.entry_args]
		main_process.init_entry_arguments(init_args)

		#try:

		if is_single_run:
			main_process.execute(cp, func_addr, is_single_run, default_reduction_counter)
			print "================ Result ================="
			pretty_print.print_value(main_process.x_reg.get(0))
			#print main_process.counter_n
		else:
			scheduler.push_to_priority_queue((main_process, cp, func_addr),
					main_process.priority)
			scheduler.schedule()
			print "================ Result ================="
			pretty_print.print_value(main_process.x_reg.get(0))

		f.close()
		return 0 

		#except:
			#pass
		#finally:
			#f.close()
			#return 0
