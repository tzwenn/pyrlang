from pyrlang.utils.deque import Deque
from pyrlang.interpreter import constant
from pyrlang.rpybeam import pretty_print
from rpython.rlib import jit
from sets import Set
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
		
		#self.process_pool = Set()

		self.internal_message_queue = Deque()

	@jit.unroll_safe
	def schedule(self):
		low_skip_times = 0
		while True:
			#print "active:"+str([pretty_print.value_str(e.pid) for e in self.normal_queue.dump()]) 
			while not self.max_queue.empty():
				self._handle_one_process_from_queue(self.max_queue)
			while not self.high_queue.empty():
				self._handle_one_process_from_queue(self.high_queue)
			if self.normal_queue.empty():
				# we now still only have one scheduler so here
				# we just simply terminate the whole system
				break
			else:
				# NOTE: only for checking, comment it out before compilation!!!
				#for p in self.normal_queue.dump():
					#if not p.is_active:
						#raise Exception("inactive process %s in queue!"%(str(p)))
				# end
				process = self.normal_queue.pop()
				# for low priority process: skipping a low priority process
				# for a number of times before executing it.
				if process.priority == constant.PRIORITY_LOW:
					if low_skip_times >= constant.LOW_PRIORITY_PROCESS_SKIP_TIMES:
						self._handle_one_process(self.normal_queue, process)
						low_skip_times = 0
					else:
						low_skip_times += 1
				# for normal priority process
				else:
					self._handle_one_process(self.normal_queue, process)

	def create_pid(self):
		return self.pid_provider.create_pid()

	def push_to_priority_queue(self, process, priority):
		self.get_queue_by_priority(priority).append(process)

	def get_queue_by_priority(self, priority):
		if priority == constant.PRIORITY_MAXIMUM:
			return self.max_queue
		elif priority == constant.PRIORITY_HIGH:
			return self.high_queue
		elif priority == constant.PRIORITY_NORMAL:
			return self.normal_queue
		else:
			assert False

	def send_by_pid(self, pid, msg):
		process = pid.get_process()
		#print "active:"+str([pretty_print.value_str(e.pid) for e in self.normal_queue.dump()]) 
		if process:
			if not process.is_active:
				#print "found process %s in inactive_processes"%(pretty_print.value_str(pid))
				process.is_active = True
				if process.mail_box.is_empty():
					# we shouldn't push target target process to the queue in this situation, because we execute it immediately (just like pop it from the queue)
					#print "send meg %s DIRECTLY to process %s"%(pretty_print.value_str(msg), pretty_print.value_str(pid))
					self._handle_one_process(self.get_queue_by_priority(process.priority), process, msg)
					return
				else:
					self.push_to_priority_queue(process, process.priority)
			#print "send msg %s to process %s"%(pretty_print.value_str(msg), pretty_print.value_str(pid)) + " message queue:" + str([pretty_print.value_str(e) for e in process.mail_box.dump()])
			process.append_message(msg)

	def _handle_one_process_from_queue(self, queue):
		process = queue.pop()
		self._handle_one_process(queue, process)

	def _handle_one_process(self, queue, process, msg=None):
		state = process.execute(self.is_single_run, self.reduction, msg)
		#print "process " + pretty_print.value_str(process.pid) + " terminate by state %d"%(state)
		if state == constant.STATE_SWITH:
			queue.append(process)
		elif state == constant.STATE_TERMINATE:
			pass
			#self.process_pool.remove(process.pid)
			#del self.process_pool[process.pid]
		elif state == constant.STATE_HANG_UP:
			#if queue._exist(process):
				#raise Exception("why %s is in queue? it shouldn't actually"%(pretty_print.value_str(process.pid)))
			if process.mail_box.is_empty():
				process.is_active = False
			else:
				self.push_to_priority_queue(process, process.priority)
