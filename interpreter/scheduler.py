from pyrlang.utils.deque import Deque
from pyrlang.interpreter import constant
from pyrlang.rpybeam import pretty_print
from rpython.rlib import jit
class Scheduler:
	_immutable_fields_ = ['pid_provider', 'is_single_run', 'reduction', 'const0']
	def __init__(self, pid_provider, is_single_run, reduction):
		self.pid_provider = pid_provider
		self.is_single_run = is_single_run
		self.reduction = reduction
		# the run able queue
		self.normal_queue = Deque()
		self.high_queue = Deque()
		self.max_queue = Deque()
		
		# a pid => process dictionary
		self.process_pool = {}

		# pid => (process, cp, pc)
		self.inactive_processes = {}

		self.internal_message_queue = Deque()

	@jit.unroll_safe
	def schedule(self):
		low_skip_times = 0
		while True:
			#print "active:"+str([pretty_print.value_str(e[0].pid) for e in self.normal_queue.dump()]) 
			#print "inactive:"+str([pretty_print.value_str(e) for e in self.inactive_processes.keys()])
			while not self.max_queue.empty():
				self._handle_one_process_from_queue(self.max_queue)
			while not self.high_queue.empty():
				self._handle_one_process_from_queue(self.high_queue)
			if self.normal_queue.empty():
				# we now still only have one scheduler so here
				# we just simply terminate the whole system
				break
			else:
				pcp = self.normal_queue.pop()
				# for low priority process: skipping a low priority process
				# for a number of times before executing it.
				if pcp[0].priority == constant.PRIORITY_LOW:
					if low_skip_times >= constant.LOW_PRIORITY_PROCESS_SKIP_TIMES:
						self._handle_one_process(self.normal_queue, pcp)
						low_skip_times = 0
					else:
						low_skip_times += 1
				# for normal priority process
				else:
					self._handle_one_process(self.normal_queue, pcp)

	def create_pid(self):
		return self.pid_provider.create_pid()

	def push_to_priority_queue(self, pcp, priority):
		if priority == constant.PRIORITY_MAXIMUM:
			self.max_queue.append(pcp)
		elif priority == constant.PRIORITY_HIGH:
			self.high_queue.append(pcp)
		else:
			self.normal_queue.append(pcp)

	def send_by_pid(self, pid, msg):
		if pid in self.process_pool:
			process = self.process_pool[pid]
			process.append_message(msg)
			#print "send msg %s to process %s"%(pretty_print.value_str(msg), pretty_print.value_str(pid)) + " message queue:" + str([pretty_print.value_str(e) for e in process.mail_box.dump()])
			if pid in self.inactive_processes:
				self.push_to_priority_queue(self.inactive_processes[pid],
						process.priority)
				#print "found process %s in inactive_processes"%(pretty_print.value_str(pid))
				del self.inactive_processes[pid]

	def _handle_one_process_from_queue(self, queue):
		pcp = queue.pop()
		self._handle_one_process(queue, pcp)

	def _handle_one_process(self, queue, pcp):
		(process, cp, pc) = pcp
		(state, pc, cp) = process.execute(cp, pc, self.is_single_run, self.reduction)
		#print "process " + pretty_print.value_str(process.pid) + " terminate by state %d"%(state)
		if state == constant.STATE_SWITH:
			queue.append((process, cp, pc))
		elif state == constant.STATE_TERMINATE:
			del self.process_pool[process.pid]
		elif state == constant.STATE_HANG_UP:
			if process.mail_box.is_empty():
				self.inactive_processes[process.pid] = (process, cp, pc)
			else:
				#print [pretty_print.value_str(e) for e in process.mail_box.dump()]
				self.push_to_priority_queue(pcp, process.priority)
