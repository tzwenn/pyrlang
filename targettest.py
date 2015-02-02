import sys
# used for my MacBook
sys.path.append('/Users/kiwakachen/src/python/pypy-zh/pypy')
# used for lab's MacPro
sys.path.append('/Users/huangruochen/src/python/pypy')
sys.path.append('../')
#print sys.path
from pyrlang.utils.app import App

#from pyrlang.rpybeam.beam_file import BeamRoot
#from pyrlang.rpybeam.pretty_print import *
#from pyrlang.rpybeam.beam_code import CodeParser
#from pyrlang.interpreter.interp import Process
#from pyrlang.interpreter.datatypes.number import *
#from pyrlang.interpreter.pid_provider import PidProvider
#from pyrlang.interpreter import constant
#from pyrlang.interpreter.scheduler import Scheduler

#def parse_arg(argv):
	#entry_func = "start"
	#single_run = False
	#if argv[0] == '-s':
		#single_run = True
		#argv = argv[1:]
	#file_name = argv[0]
	#argv = argv[1:]
	#arg_len = len(argv)
	#args = []
	#if arg_len > 0:
		#entry_func = argv[0]
		#if arg_len > 1:
			#for i in range(0, arg_len - 1):
				#args.append(W_IntObject(int(argv[1+i])))
	#return (single_run, file_name, entry_func, args)

#def main(argv):
	#(single_run, file_name, entry_func, args) = parse_arg(argv[1:])
	#f = open(file_name, "rb")
	#b = BeamRoot(f)
	#pid_provider = PidProvider()
	#scheduler = Scheduler(pid_provider)
	#brt = Process(pid_provider.create_pid(), scheduler, constant.PRIORITY_NORMAL)
	#scheduler.process_pool[brt.pid] = brt
	#cp = CodeParser(b, file_name)
	#func_addr = cp.get_func_addr_by_name(entry_func, len(args))
	#brt.init_entry_arguments(args)
	#try:
		#if single_run:
			#try:
				#pc = brt.execute(cp, func_addr, True)
				#res = brt.x_reg.get(0)
				#print_value(res)
			#except:
				#return 0
		#else:
			##try:
			#scheduler.push_to_priority_queue((brt, cp, func_addr), brt.priority)
			#scheduler.schedule()
			#res = brt.x_reg.get(0)
			#print_value(res)
			##except:
				##return 0
	#finally:
		#f.close()
	#return 0

def main(argv):
	app = App(argv)
	return app.launch()
	return 0

def target(driver, args):
	return main, None

if __name__ == '__main__':
	main(sys.argv)
