from dlist import Dlist

class HashEntry :
	def __init__(self, c, d):
		self.clave = c
		self.dato = d

	def __str__(self):
		return ' clave: '+str(self.clave)+' tipo: '+str(self.dato) + ' '

	def __repr__(self):
		return self.__str__()

class HashTable :
	#Metodo constructor
	def __init__(self, n):
		self.size = n
		self.array = [Dlist() for i in range(n)]

	#Agrega un elemento e de tipo HashEntry a la tabla de hash
	def agregarElemento(self,e):
		key = e.clave%self.size
		i = self.array[key].header.next
		while not(i.element is None) :
			if i.element.clave == e.clave :
				#i.element.dato = e.dato
				return True
			i = i.next
		self.array[key].insertBetween(e, self.array[key].header, self.array[key].header.next)

	#Se agrega a a la tabla de hash una clave c, que tiene asociada un dato de tipo String d
	def Agregar(self, c, d) :
		self.agregarElemento(HashEntry(c,d))

	#Dada una referencia de un elemento e de tipo HashEntry de la tabla de hash, 
	#entonces se elimina a e de la tabla.
	def eliminarElemento(self,e):
		key = e.clave%self.size
		i = self.array[key].header.next
		while not(i.element  is None) :
			if i.element.clave == e.clave and i.element.dato == e.dato :
				self.array[key].deleteNode(i)
				return
			i = i.next

	# Dada un clave c, si algún elemento en la tabla de hash tiene una
	# clave igual a c, entonces el elemento se elimina de la tabla y retorna el String
	# asociado a esa clave.
	def Eliminar(self,c):
		key = c%self.size
		i = self.array[key].header.next
		while not(i.element  is None) :
			if i.element.clave == c :
				a = i.element.dato
				self.array[key].deleteNode(i)
				return a
			i = i.next

	#Dada un clave c, se busca el elemento en la tabla de hash que posea la clave igual a c. 
	#Si el elemento se encuentra en la tabla, entonces se retorna el String asociado a esa clave.
	def Buscar(self, c) :
		key = c%self.size
		i = self.array[key].header.next
		while not(i.element  is None) :
			if i.element.clave == c :
				return i.element.dato
			i = i.next

	def __len__(self) :
		return self.size

	#Se muestra por la salida estándar todos los elementos de la tabla de hash, en forma de pares clave y valor asociado.
	def __str__(self):
		s = ''
		for i in range(self.size):
			s += str(self.array[i].header) + '\n'
		return s
