
class Dlist:
	#Clase para el apuntador
	class Nodo:
		def __init__(self, e, n, m):
			self.element = e
			self.prev = n
			self.next = m

		def __str__(self):
			string = "["
			aux = self
			while not aux == None:
				string += "{0},".format(aux.element)
				aux = aux.next
			string = string[:-1] + "]" 
			return string

		def __repr__(self):
			return self.__str__()

	#Metodo constructor.
	def __init__(self):
		self.header = self.Nodo(None,None,None)
		self.trailer = self.Nodo(None,None,None)
		self.header.next = self.trailer
		self.trailer.prev = self.header
		self.size = 0

	#Metodo que devuelve el numero de elementos de la lista.
	def __len__(self) :
		return self.size

	#Metodo que devuelve True si la lista esta vacıa, si no retorna False.
	def is_empty(self) :
		return self.size == 0

	#Metodo que inserta un elemento en lista entre 2 elementos.
	def insertBetween(self, e, predecessor, successor) :
		newest = self.Nodo(e, predecessor, successor)
		predecessor.next = newest
		successor.prev = newest
		self.size += 1
		return newest

	#Metodo que elimina a un elemento de la lista y lo retorna.
	def deleteNode(self, node):
		predecessor = node.prev
		successor = node.next
		predecessor.next = successor
		successor.prev = predecessor
		self.size -= 1
		element = node.element
		node.prev = node.next = node.element = None
		return element

	def __str__(self) :
		return str(self.header)
		
	def __repr__(self):
		return self.__str__()
