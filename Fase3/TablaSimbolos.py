
class VariablesDeclaradas():
	def __init__(self,id,type):
		self.id = id
		self.type = type
		self.linea = -1
		self.columna = -1
	  
	def setType(self,type):
		self.type = type

	def getType(self):
		return self.type 

	def setLinea(self,line):
		self.linea = line
	  
	def setColumna(self,col):
		self.columna = col

	def __eq__(self,otro):
		return self.id == otro.id
	  
	def __str__(self):
		return " Variable: " + str(self.id) + " | tipo: " + str(self.type)
  
# Clase para la tabla de símbolos. Almacena una lista de variables, y una indentación
# que se usa al imprimirse
class TablaSimbolos():
	def __init__(self):
		self.lista = []
		
	def insert(self,var):
		self.lista.append(var)
		
	def delete(self,var):
		if (self.pertenece(var.id)):
			self.lista.remove(var)
		
	# El parametro verbose indica si debe imprimirse errores en pantalla
	def pertenece(self,var):
		try:
			self.lista.index(VariablesDeclaradas(var,''))
		except ValueError:
			return 0
		return 1
		
	def busqueda(self,id):
		if self.pertenece(id):
			return self.lista[self.lista.index(VariablesDeclaradas(id,''))]
		else:
			return None
		
	def __str__(self):
		string = '    Tabla de simbolos:\n'
		
		for i in self.lista:
			string += ' ' + str(i) + '\n'
		return string

