A = [10, 11, 14, 13, -6, -14, 14, 13, -8, -15, 10, -11, -13, -4]
B = [1,  9,  12, 6,   9,  15, 7,  12,  15, 3,  6,   2,   10, 12]
pops = [0,0,0,0,1,1,0,0,2,2,0,1,1,1]


def finished(z):
	return all([val == 0 for val in z])

def run(inputs):
	z = [0, 0, 0, 0]
	i = 0
	for a,b,w,pop in zip(A,B,inputs,pops):
		x = z[-1] + a
		if pop:
			z = z[:-1]
		if x != w:
			z.append(w+b)
	return all([val == 0 for val in z])


def solve1():
	for a in range(9,0,-1):
		for b in range(9,0,-1):
			for c in range(9,0,-1):
				for d in range(9,0,-1):
					e = d
					f = c-2
					if(e<1 or e>9 or f<1 or f>9):
						continue
					for g in range(9,0,-1):
						for h in range(9,0,-1):
							i = h+4
							j = g-8
							if(i<1 or i>9 or j<1 or j>9):
								continue
							for k in range(9,0,-1):
								l = k-5
								m = b-4
								n = a-3
								if(l<1 or l>9 or m<1 or m>9 or n<1 or n>9):
									continue
								# return [a,b,c,d,e,f,g,h,i,j,k,l,m,n]
								if run([a,b,c,d,e,f,g,h,i,j,k,l,m,n]):
									return [a,b,c,d,e,f,g,h,i,j,k,l,m,n]

def solve2():
	for a in range(1,10):
		for b in range(1,10):
			for c in range(1,10):
				for d in range(1,10):
					e = d
					f = c-2
					if(e<1 or e>9 or f<1 or f>9):
						continue
					for g in range(1,10):
						for h in range(1,10):
							i = h+4
							j = g-8
							if(i<1 or i>9 or j<1 or j>9):
								continue
							for k in range(1,10):
								l = k-5
								m = b-4
								n = a-3
								if(l<1 or l>9 or m<1 or m>9 or n<1 or n>9):
									continue
								return [a,b,c,d,e,f,g,h,i,j,k,l,m,n]
								if run([a,b,c,d,e,f,g,h,i,j,k,l,m,n]):
									return [a,b,c,d,e,f,g,h,i,j,k,l,m,n]


print(solve2())