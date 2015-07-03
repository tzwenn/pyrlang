import sys

if __name__ == '__main__':
	f = open(sys.argv[1], "rb")
	times = ""
	for l in f.readlines():
		(_,time) = l.split(" ")
		times += time
	print times
	f.close()
