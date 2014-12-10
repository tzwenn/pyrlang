from beam_file import *

def print_ImpT(impt, atomTable):
	impt_len = len(impt.entries)
	print "# Imports:"
	for i in range(0, impt_len):
		e = impt.entries[i]
		print "#      %s:%s/%d"%(atomTable[e.module-1], atomTable[e.function - 1], e.arity)
	print "#"

def print_ExpT(impt, atomTable):
	expt_len = len(impt.entries)
	print "# Exports:"
	for i in range(0, expt_len):
		e = impt.entries[i]
		print "#      %s/%d"%(atomTable[e.function-1], e.arity)
	print "#"

def print_Root(root):
	atoms = root.atomChunk.asList()
	print "# Module: %s"%(atoms[0])
	print "#"
	print_ExpT(root.expTChunk, atoms)
	print_ImpT(root.impTChunk, atoms)
