# please refer to the counterpart C# implementation
#     which exactly describes and solves the same problems

class IdentityMonad:
	@staticmethod
	def returnM(val):
		return IdentityMonad(val)

	def __init__(self, val):
		self.val = val	

	# f :: a -> m b
	def bind(self, f):
		newM = f( self.val )
		return IdentityMonad.returnM( newM.val )

class MaybeMonad:

	@staticmethod
	def returnM(val):
		return MaybeMonad(val)
	
	def __init__(self, val):
		self.val = val

	def bindM(self, f):
		if self.val is None:
			return MaybeMonad.returnM( None )
		return f( self.val )

class Container:

	def __init__(self, capaLimit):
		self.capaLimit = capaLimit
		self.capa = MaybeMonad.returnM( 0 )

	def change(self, diff):
		def doChange(x):
			if x + diff < 0 or x + diff > self.capaLimit:
				return MaybeMonad.returnM( None )
			else:
				return MaybeMonad.returnM( x + diff )

		self.capa = self.capa.bindM( doChange )
	
	def toString(self):
		if self.capa.val is None:
			return "Container: error"
		else:
			return "Container: %s/%s" % (self.capa.val, self.capaLimit)

if __name__ == "__main__":
	print "Task #4: implement monad in a nonfunctional language"
	print "Identity monad: "
	idMonad = IdentityMonad.returnM(1).bind( lambda x:
		IdentityMonad.returnM( x*2 ).bind( lambda y:
			IdentityMonad.returnM( y+30 )))

	print "Perform: 1 * 2 + 30"
	print idMonad.val

	def containerModify( container, diff ):
		container.change( diff )
		print container.toString()
		return container

	reduce( containerModify, [10, -8, -3, 3], Container(10) )
	reduce( containerModify, [10, -8, 3, -3], Container(10) )
