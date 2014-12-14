import sys

from pyrlang.rpybeam import opcodes
from pyrlang.lib import ModuleDict

lib_module = ["erlang"]

class BeamRunTime:
	def __init__(self, cp, atoms, impTs):
		self.cp = cp # code parser
		self.atoms = atoms
		self.func_list = []
		self.importFuncs(impTs)

	def importFuncs(self, impTs):
		for i in range(0, len(impTs)):
			entry = impTs[i];
			moduleName = self.atoms[entry[0] - 1]
			# TODO: add else branch to hold custom module
			if moduleName in lib_module:
				# we use function_arity to emulate function overload
				moduleEntity = ModuleDict.module_dict[moduleName]()
				function_name = "%s_%d"%(self.atoms[entry[1] - 1], entry[2])
				self.func_list.append(moduleEntity.searchFunc(function_name)())

	#def execute(self):
		#instr = cp.parseInstr()
		#if instr == opcodes.GC_BIF2:
			#bif_index = cp.parse_literal()
			#fail = cp.parseInt()
			#alive = cp.parseInt()
			#regval1 = cp.parseBaseReg()
			#regval2 = cp.parseBaseReg()
			#dst_reg = cp.parseBaseReg()
			#self.gc_bif2(bif_index, fail, alive, regval1, regval2, dst_reg)
