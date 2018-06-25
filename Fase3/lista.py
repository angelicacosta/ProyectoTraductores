import copy

class NodeList:
	def __init__(self, table=None, nextNode=None, level=0):
		self.table = {}
		self.nextNode = nextNode
		self.level = level
	
	def add(self, name, idType, value):
		self.table[name] = [idType,value]
	
	def search(self, name):
		if name in self.table.keys():
			return True
		else:
			return False

	def getTable(self):
		return self.table

	def getType(self, name):
		return self.table[name][0]

	def getValue(self, name):
		return self.table[name][1]

	def getNext(self):
		return self.nextNode

	def getLevel(self):
		return self.level

	def setNext(self, nextNode):
		self.nextNode = nextNode

	def setLevel(self, level):
		self.level = level

class List:
	def __init__(self, inicial=NodeList(), final=None, count=0):
		self.inicial = inicial
		self.final = inicial
		self.count = count
	
	def add(self, nextList):
		self.final.setNext(nextList)
		self.final = nextList
		self.count += 1

	def setInicial(self, inicial):
		self.inicial = inicial
		self.count += 1

	def search(self, name, level):
		actual=self.inicial
		for i in range(self.count):
			if actual.getLevel() < level:
				if actual.search(name):
					return [actual.getType(name),actual.getValue(name)]
			actual = actual.getNext()
		return None

	def printList(self):
		actual = self.inicial
		while actual != None:
			for key in sorted(actual.getTable().keys()):
				print('\t'*actual.level+'| variable: '+key +' | tipo: '+actual.getType(key)+' | valor: ' + str(actual.level))
			actual = actual.getNext()

'''
nodo1 = NodeList()
nodo1.add('a','int',1)
nodo1.add('b','bool',True)
nodo1.add('c','char','a')

d1 = copy.deepcopy(nodo1)

nodo1 = NodeList()

print(d1.getTable())
print(nodo1.getTable())
contador = nodo1.getLevel()+1
nodo2 = NodeList(level=contador)

nodo2.add('e','int',2)
nodo2.add('f','bool',False)

lista = List()
lista.setInicial(nodo2)
lista.add(nodo1)

lista.printList()'''