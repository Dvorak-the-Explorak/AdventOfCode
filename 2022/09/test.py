
# I don't have a haskell equivalent
def iterative(f, vals):
	result = vals[:1]
	for i in range(1, len(vals)):
		result.append(f(result[i-1], vals[i]))
	return result

	# # or:
	# vals = vals[:]
	# for i in range(1, len(vals)):
	# 	vals[i] = f(vals[i-1], vals[i])
	# return vals


# cascade :: (a -> a -> a) -> [a] -> [a]
# cascade _ [] = []
# cascade _ [x] = [x]
# cascade f (p:x:xs) = p:cascade f ((f p x):xs)
def recursive(f, vals):
	# Keeps the previous result at the front of the list,
	#	so apply f to the first two elems, using the result
	#	as the new second element.
	#	The first element doesn't need to be passed along in
	#	the recursive call.
	if len(vals) < 2:
		return vals[:]
	prev,current,*rest = vals
	result = f(prev, current)
	return [prev] + recursive(f, [result] + rest)

# cascade :: (a -> a -> a) -> [a] -> [a]
# cascade f [] = []
# cascade f (x:xs) = x : zipWith f (cascade f (x:xs)) xs
def lazyRecursive(f, vals):
	# The idea is just take the answer and a copy of the input shifted forwards (first element removed),
	#	then apply the function to corresponding elements.  
	# The elements in the shifted copy will be the second argument, 
	#	and the first argument will be the result at the previous element

	if not vals:
		return []

	# This solution requires a lazily evaluated generator,
	#	so this is here so I can wrap it in a list at the end
	def gen():
		# return the head of the list unmodified
		yield vals[0]

		# zip together the full generator with the tail of the values
		yield from (f(*pair) for pair in zip(gen(), vals[1:]))

	return list(gen())



def add(x, y):
	return x + y


N = 400
print(iterative(add, [1]*N))
print(recursive(add, [1]*N))
print(lazyRecursive(add, [1]*N))

