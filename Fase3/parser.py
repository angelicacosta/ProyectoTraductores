# Autores:
# Giulianne Tavano. 13-11389
# Angelica Acosta. 14-10005

import ply.yacc as yacc
import ply.lex as lex
import sys
import re
from Entrega1 import tokens, obtenerColumna, lexerErrorFound
from lista import *

parserErrorFound=False
semanticErrorFound=False

lista=List()
actualLevel=0
nodoNuevo = NodeList()

# Indica cuales son el orden de precedencia de los tokens y hacia donde asocian.
precedence = (
	('left', 'TkSuma', 'TkResta'),
	('left', 'TkMult', 'TkDiv','TkMod'),
	('left', 'TkDisyuncion'),
	('left', 'TkConjuncion'),
	('right', 'TkNegacion'),
	('left','TkSiguienteCar'),
	('left', 'TkAnteriorCar'), 
	('right', 'TkValorAscii'),
	('left','TkConca'),
	('right','TkShift'),
	('left', 'TkCorcheteAbre', 'TkCorcheteCierra'),
	('nonassoc', 'TkMenor', 'TkMenorIgual','TkMayor','TkMayorIgual','TkIgual','TkDesigual'),
	('left', 'TkParAbre','TkParCierra'),
	('left', 'TkHacer')
)

# Clase node que permite la creacion de los nodel del arbol sintactico.
class Node:
	def __init__(self,type,children=None,leaf=None, tipo = None, valor=None):
		self.type = type
		if children:
			self.children = children
		else:
			self.children = [ ]
		self.leaf = leaf
		self.tipo=tipo
		self.valor=valor
	def __str__(self):
		return str(self.type)

	#Cambia el type del nodo.
	def changeType(self,newType):
		self.type = newType

	def setTipo(self, tipo):
		self.tipo=tipo

	def getTipo(self):
		return self.tipo

	#Obtiene el type del nodo.
	def getType(self):
		return self.type

	def getLeaf(self):
		return self.leaf

	def setLeaf(self,leaf):
		self.leaf = leaf

	#Agrega hijos a un nodo.
	def addChildren(self,newChildren):
		self.children = [newChildren] + self.children

	def addTipo(self,newTipo):
		self.tipo = self.tipo + newTipo

# Regla inicial de la gramatica
def p_S(p):
	''' S : TkWith Lista_Declaraciones Bloque_Inst 
	| Bloque_Inst
	'''
	global lista, nodoNuevo, actualLevel
	if (p[1]=='with'):
		temp = copy.deepcopy(nodoNuevo)
		lista.add(temp)
		nodoNuevo = NodeList(level=actualLevel)
		p[0] = Node("",[p[3]])
		p[0].setLeaf('with')
		actualLevel-=1	
	else: 
		p[0] = p[1]
	

# Regla de la gramatica que reconoce todas las declaracios de variables.
def p_Lista_Declaraciones(p):
	'''Lista_Declaraciones : TkVar Lista_Variables Lista_Declaraciones 
	| TkVar Lista_Variables '''
	global lista, nodoNuevo, actualLevel
	if(len(p)>3):
		p[0]= str(p[2]) +'\n' + str(p[3])  
	else:
		temp = copy.deepcopy(nodoNuevo)
		lista.add(temp)
		actualLevel+=1
		nodoNuevo = NodeList(level=actualLevel+1)
		p[0] = p[2]

# Regla de la gramatica que reconoce toda la lista de variables.
def p_Lista_Variables(p):
	''' Lista_Variables : TkId TkComa Lista_Variables 
	| TkId TkAsignacion Operacion TkComa Lista_Variables
	| TkId TkAsignacion Operacion TkDosPuntos Tipo
	| TkId TkDosPuntos Tipo '''
	global nodoNuevo
	if (len(p)==6):
		if p[4]==r':':
			if not nodoNuevo.search(p[1]):
				nodoNuevo.add(p[1],p[5].getTipo(),p[3].valor)
			else:
				print("No puede redeclarar la variable "+p[1])
				exit()
			p[0] = Node(p[2], [p[1], p[3]], p[1])
		else:
			if not nodoNuevo.search(p[1]):
				nodoNuevo.add(p[1],nodoNuevo.getType(p[5].getLeaf()),p[3].valor)
			else:
				print("No puede redeclarar la variable "+p[1])
				exit()
			p[0] = Node(p[2], [p[1], p[3]], p[1])
		
		#p[0]= str(Node(p[2], [p[1], p[3]], None, None)) + ' '+ str(p[5])
	else:
		if p[2]==r':':
			if not nodoNuevo.search(p[1]):
				nodoNuevo.add(p[1],p[3].getTipo(),None)
			else:
				print("No puede redeclarar la variable "+p[1])
				exit()
			p[0] = Node(p[2], [p[1]], p[1])
		else:
			if not nodoNuevo.search(p[1]):
				nodoNuevo.add(p[1],nodoNuevo.getType(p[3].getLeaf()),None)
			else:
				print("No puede redeclarar la variable "+p[1])
				exit()
			p[0] = Node(p[2], [p[1]], p[1])
		#p[0]= str(p[1]) +'\n' + str(p[3])   

# Regla de la gramatica que reconoce todo los tipos de variables.
def p_Tipo(p):
	''' Tipo : TkInt 
	| TkBool 
	| TkChar
	| TkArray TkCorcheteAbre Operacion TkCorcheteCierra TkOf Tipo
	| TkArray TkCorcheteAbre Operacion TkCorcheteCierra TkOf TkId
	'''
	global lista, actualLevel
	if (p[1]=='array'):
		p[0] = Node(p[1], [p[3]], None, 'array de ')
		if isinstance(p[6],Node):
			p[0].addTipo(p[6].getTipo())
		else:
			p[0].setLeaf(p[6])
			p[0].addTipo('variable')
	
	else: 
		if(p[1]=='int'):
			p[0]=Node(p[1], None,None, 'int')
		elif (p[1]=='bool'):
			p[0]=Node(p[1], None,None, 'bool')
		else:
			p[0]=Node(p[1], None,None, 'char')

# Regla de la gramatica que reconoce los bloques de instrucciones.
def p_Bloque_Inst(p):	
	'''Bloque_Inst : TkBegin Lista_Instrucciones TkEnd 
	| TkBegin TkEnd 
	'''
	if(p[2]!='end'):	
		p[0]= p[2]
	else:
		p[0] = p[1] + p[2]

# Regla de la gramatica que reconoce tiene todas las opciones de instrucciones.
def p_Inst(p):
	'''Inst : Inst_Asignacion 
	| Inst_If 
	| Inst_Bool 
	| Inst_For 
	| Inst_Entrada  
	| Inst_Salida
	| Inst_Punto
	| S
	'''
	p[0] = p[1]

# Regla de la gramatica que reconoce todas instrucciones con puntos.
def p_Inst_Punto(p):
	'''Inst_Punto : TkId TkPunto Operacion TkPuntoYComa '''
	p[3].changeType('-Expresion: '+str(p[3]))
	p[0]=Node('PUNTO', [Node('-Contenedor: '+p[1],leaf=p[1],tipo='variable'),p[3]], 'punto', None)

# Regla de la gramatica que reconoce todas instrucciones.
def p_Lista_Instrucciones(p):
	'''Lista_Instrucciones : Inst 
	|  Inst Lista_Instrucciones'''
	if(len(p)>2):
		if (p[2].getType() == "SECUENCIACION"):
			p[2].addChildren(p[1])
			p[0] = p[2]
		else:
			p[0]=Node('SECUENCIACION', [p[1],p[2]], None, None)
	else:
		p[0] = p[1]
	  
# Regla de la gramatica que reconoce todas instrucciones de los condicionales
def p_Inst_If(p):
	'''Inst_If : TkIf Operacion TkHacer Lista_Instrucciones TkEnd
	| TkIf Operacion TkHacer Lista_Instrucciones TkOtherwise TkHacer Lista_Instrucciones TkEnd
	'''
	p[2].changeType('-Guardia: '+str(p[2]))
	p[4].changeType('-Exito: '+str(p[4]))
	if (len(p)==6):
		p[0] = Node('CONDICIONAL', [p[2],p[4]], 'if', None)
	else:
		p[7].changeType('-Fracaso: '+str(p[7]))
		p[0] = Node('CONDICIONAL', [p[2],p[4],p[7]], 'if', None)

# Regla de la gramatica que reconoce todas instrucciones de ciclos o while.
def p_Inst_Bool(p):
	'''Inst_Bool : TkWhile Operacion TkHacer Lista_Instrucciones TkEnd
	'''
	p[2].changeType('-Guardia: '+str(p[2]))
	p[4].changeType('-Exito: '+str(p[4]))
	p[0] = Node('CICLO', [p[2],p[4]], 'while', None)
	
# Regla de la gramatica que reconoce todas instrucciones de iteracion determinada (For).
def p_Inst_For(p):
	'''Inst_For : TkFor TkId TkFrom Operacion TkTo Operacion TkHacer Inst TkEnd
	| TkFor TkId TkFrom Operacion TkTo Operacion TkStep Operacion TkHacer Inst TkEnd
	'''
	p[2] = Node('Variable de iteracion: (var "'+p[2]+'")', None, None, None)
	
	p[4].changeType('-Limite inferior: '+str(p[4]))
	
	p[6].changeType('-Limite superior: '+str(p[6]))

	if (len(p)==10):
		p[8].changeType('-Exito: '+str(p[8]))
		p[0]= Node('ITERACION DETERMINADA', [p[2],p[4],p[6],p[8]], 'for', None)
	else:
		p[8].changeType('-Step: '+str(p[8]))
		p[10].changeType('-Exito: '+str(p[10]))
		p[0]= Node('ITERACION DETERMINADA', [p[2], p[4], p[6], p[8], p[10]], 'for', None)

# Regla de la gramatica que reconoce todas las operaciones del lenguaje.
def p_Operacion(p):
	'''Operacion : TkParAbre Operacion TkParCierra
	| Operacion TkSuma Operacion 
	| Operacion TkResta Operacion 
	| Operacion TkMult Operacion 
	| Operacion TkDiv Operacion 
	| Operacion TkMod Operacion 
	| TkResta Operacion 
	| Operacion TkMenor Operacion
	| Operacion TkMenorIgual Operacion 
	| Operacion TkMayor Operacion 
	| Operacion TkMayorIgual Operacion 
	| Operacion TkIgual Operacion 
	| Operacion TkDesigual Operacion
	| Operacion TkConjuncion Operacion
	| Operacion TkDisyuncion Operacion 
	| TkNegacion Operacion
	| OpCaracter
	| Op_Arreglo
	| TkId  
	| TkNum
	| TkTrue 
	| TkFalse
	| TkCaracter
	'''
	
	if (len(p) == 2):
		if isinstance(p[1],int):
			p[0] = Node('literal entero ('+str(p[1])+')', None, None, 'int', int(p[1]))
		elif (p[1] == 'true'):
			p[0] = Node('booleano (TRUE)', None, None, 'bool', True)
		elif (p[1] == 'false'):
			p[0] = Node('booleano (FALSE)', None, None, 'bool', False)
		elif isinstance(p[1],Node):
			p[0] = p[1]
		else:
			caracter = re.compile('[\'][a-zA-Z_][\']|["][a-zA-Z_]["]')
			if caracter.match(p[1]):
				try:
					p[0] = Node('caracter ('+p[1]+')', None, None, 'char', nodoNuevo.getValue(p[1]))
				except:
					p[0] = Node('caracter ('+p[1]+')', None, None, 'char', 0)
			else:
				try:
					p[0] = Node('variable ("'+p[1]+'")', None, p[1], 'variable', nodoNuevo.getValue(p[1]))
				except:
					p[0] = Node('variable ("'+p[1]+'")', None, p[1], 'variable', 0)
	
	elif (len(p) == 3):
		if (p[1] == '-'):
			if (p[2].getTipo()=='int' or p[2].getTipo()=='variable'):
				p[0]=Node("MENOS UNARIO",  [p[2]], 'menos u', 'int', - p[2].valor)
			else:
				print("Operacion invalida")
				exit()
		else:
			if (p[2].getTipo()=='bool' or p[2].getTipo()=='variable'):
				p[0]=Node("NOT",  [p[2]], 'not', 'bool', not p[2].valor)
			else:
				print("Operacion invalida")
				exit()
	
	else:
		if not (p[1]=='('):
			p[1].changeType('-Operando izquierdo: '+str(p[1]))
			p[3].changeType('-Operando derecho: '+str(p[3]))
		else:
			p[0] = p[2]

		if (p[2] == '+'):
			if (p[1].getTipo() == 'int' or p[1].getTipo() == 'variable') and (p[3].getTipo() == 'int' or p[3].getTipo() == 'variable'):
				p[0] = Node("SUMA", [p[1],p[3]], p[2], 'int', p[1].valor+p[3].valor )
			else:
				print("Operacion invalida")
				exit()
		elif (p[2] == '-'):
			if (p[1].getTipo() == 'int' or p[1].getTipo() == 'variable') and (p[3].getTipo() == 'int' or p[3].getTipo() == 'variable'):
				p[0] = Node("RESTA", [p[1],p[3]], p[2], 'int',p[1].valor-p[3].valor)
			else:
				print("Operacion invalida")
				exit()
		elif (p[2] == '*'):
			if (p[1].getTipo() == 'int' or p[1].getTipo() == 'variable') and (p[3].getTipo() == 'int' or p[3].getTipo() == 'variable'):
				p[0] = Node("MULTIPLICACION", [p[1],p[3]], p[2], 'int', p[1].valor*p[3].valor)
			else:
				print("Operacion invalida")
				exit()
		elif (p[2] == '/'):
			if (p[1].getTipo() == 'int' or p[1].getTipo() == 'variable') and (p[3].getTipo() == 'int' or p[3].getTipo() == 'variable'):
				p[0] = Node("DIVISION", [p[1],p[3]], p[2], 'int', p[1].valor/p[3].valor)
			else:
				print("Operacion invalida")
				exit()
		elif (p[2] == '%'):
			if (p[1].getTipo() == 'int' or p[1].getTipo() == 'variable') and (p[3].getTipo() == 'int' or p[3].getTipo() == 'variable'):
				p[0] = Node("MODULO", [p[1],p[3]], p[2], 'int', p[1].valor%p[3].valor)
			else:
				print("Operacion invalida")
				exit()
		elif (p[2] == '<'):
			if (p[1].getTipo() == 'int' or p[1].getTipo() == 'bool' or p[1].getTipo() == 'variable') and (p[3].getTipo() == 'int' or p[3].getTipo() == 'bool' or p[3].getTipo() == 'variable'):
				p[0]=Node("MENOR QUE", [p[1],p[3]], p[2], 'bool', p[1].valor<p[3].valor)
			else:
				print("Operacion invalida")
				exit()
		elif (p[2] == '<='):
			if (p[1].getTipo() == 'int' or p[1].getTipo() == 'bool' or p[1].getTipo() == 'variable') and (p[3].getTipo() == 'int' or p[3].getTipo() == 'bool' or p[3].getTipo() == 'variable'):
				p[0]=Node("MENOR O IGUAL QUE", [p[1],p[3]], p[2], 'bool',p[1].valor<=p[3].valor)
			else:
				print("Operacion invalida")
				exit()
		elif (p[2] == '>'):
			if (p[1].getTipo() == 'int' or p[1].getTipo() == 'bool' or p[1].getTipo() == 'variable') and (p[3].getTipo() == 'int' or p[3].getTipo() == 'bool' or p[3].getTipo() == 'variable'):
				p[0]=Node("MAYOR", [p[1],p[3]], p[2], 'bool',p[1].valor>p[3].valor)
			else:
				print("Operacion invalida")
				exit()
		elif (p[2] == '>='):
			if (p[1].getTipo() == 'int' or p[1].getTipo() == 'bool' or p[1].getTipo() == 'variable') and (p[3].getTipo() == 'int' or p[3].getTipo() == 'bool' or p[3].getTipo() == 'variable'):
				p[0]=Node("MAYOR QUE", [p[1],p[3]], p[2], 'bool', p[1].valor>=p[3].valor)
			else:
				print("Operacion invalida")
				exit()
		elif (p[2] == '='):
			if (p[1].getTipo() == 'int' or p[1].getTipo() == 'bool' or p[1].getTipo() == 'variable') and (p[3].getTipo() == 'int' or p[3].getTipo() == 'bool' or p[3].getTipo() == 'variable'):
				p[0]=Node("IGUAL QUE", [p[1],p[3]], p[2], 'bool',p[1].valor==p[3].valor)
			else:
				print("Operacion invalida")
				exit()
		elif (p[2] == '/='):
			if (p[1].getTipo() == 'int' or p[1].getTipo() == 'bool' or p[1].getTipo() == 'variable') and (p[3].getTipo() == 'int' or p[3].getTipo() == 'bool' or p[3].getTipo() == 'variable'):
				p[0]=Node("DESIGUAL QUE", [p[1],p[3]], p[2], 'bool',p[1].valor!=p[3].valor)
			else:
				print("Operacion invalida")
				exit()
		elif (p[2] == '/\\'):
			if (p[1].getTipo() == 'bool' or p[1].getTipo() == 'variable') and (p[3].getTipo() == 'bool' or p[3].getTipo() == 'variable'):
				p[0]=Node("CONJUNCION", [p[1],p[3]], p[2], 'bool',p[1].valor and p[3].valor)
			else:
				print("Operacion invalida")
				exit()
		elif (p[2] == '\/'):
			if (p[1].getTipo() == 'bool' or p[1].getTipo() == 'variable') and (p[3].getTipo() == 'bool' or p[3].getTipo() == 'variable'):
				p[0]=Node("DISYUNCION", [p[1],p[3]], p[2], 'bool',p[1].valor or p[3].valor)
			else:
				print("Operacion invalida")
				exit()

# Regla de la gramatica que reconoce todas las operaciones de los arreglos.
def p_Op_Arreglo(p):
	'''Op_Arreglo : TkId TkCorcheteAbre Operacion TkCorcheteCierra
	| TkShift TkId 
	| TkId TkConca TkId
	| Op_Arreglo TkCorcheteAbre Operacion TkCorcheteCierra
	| TkShift Op_Arreglo 
	| Op_Arreglo TkConca TkId
	| Op_Arreglo TkConca Op_Arreglo
	| TkId TkConca Op_Arreglo
	'''
	
	if (len(p)==5):
		p[3].changeType("-Index: "+str(p[3]))
		if isinstance(p[1],str):
			p[1] = Node("-Arreglo: "+p[1], None, p[1], 'variable')			
		else:
			p[1].changeType("-Arreglo: "+str(p[1]))
		p[0] = Node("INDEXACION", [p[1],p[3]], 'index', None)
	
	elif (len(p)==3):
		if isinstance(p[2],str):
			p[2] = Node("-Arreglo: "+p[2], None, p[2], 'variable')
		else:
			p[2].changeType("-Arreglo: "+str(p[2]))
		p[0] = Node("SHIFT", [p[2]], 'shift', None)
	
	else:
		if isinstance(p[1],str):
			p[1] = Node("-Arreglo izquierdo: "+p[1], None, p[1], 'variable')
		else:
			p[1].changeType("-Arreglo izquierdo: "+str(p[1]))

		if isinstance(p[3],str):
			p[3] = Node("-Arreglo derecho: "+p[3], None, p[3], 'variable')
		else:
			p[3].changeType("-Arreglo derecho: "+str(p[3]))
		
		if (p[1].getTipo()==p[3].getTipo()):
			p[0] = Node("CONCATENACION", [p[1],p[3]], 'concat', None)
		else:
			print("Operacion invalida")
			exit()


# Regla de la gramatica que reconoce todas las operaciones con los caracteres.
def p_OpCaracter(p):
	'''OpCaracter : TkCaracter TkSiguienteCar 
	| TkCaracter TkAnteriorCar 
	| TkValorAscii TkCaracter
	| TkId TkSiguienteCar 
	| TkId TkAnteriorCar 
	| TkValorAscii TkId
	| OpCaracter TkSiguienteCar 
	| OpCaracter TkAnteriorCar 
	| TkValorAscii OpCaracter
	'''
	global lista, actualLevel
	caracter = re.compile('[\'][a-zA-Z_][\']|["][a-zA-Z_]["]')
	if (p[1]=='#'):
		if isinstance(p[2],Node):
			p[2].changeType('-Caracter: '+str(p[2]))
		elif caracter.match(p[2]):
			p[2] = Node('-Caracter: '+str(p[2]), None,None, 'char')
		else:
			p[2] = Node('-Variable: '+str(p[2]), None,p[2], 'variable')

		
		p[0] = Node('VALOR ASCII',[p[2]], 'ascii', p[2].getTipo(), p[2].valor)
	
	else:
		if isinstance(p[1],Node):
			p[1].changeType('-Caracter: '+str(p[1]))
		elif caracter.match(p[1]):
			p[1] = Node('-Caracter: '+str(p[1]),None,'char')
		else:
			p[1] = Node('-Variable: '+str(p[1]),p[1],'variable')

		
		if (p[2]=='[+][+]'):
			p[0] = Node('CARACTER SIGUIENTE',[p[1]], 'siguiente', 'char')
		else:
			p[0] = Node('CARACTER ANTERIOR',[p[1]], 'anterior', 'char')

# Regla de la gramatica utilizada para reconocer una asignacion
def p_Inst_Asignacion(p):
	'''Inst_Asignacion : TkId TkAsignacion Operacion TkPuntoYComa
	| Op_Arreglo TkAsignacion Operacion TkPuntoYComa'''  
	
	global lista, actualLevel
		
	if (isinstance(p[3].type,int)):
		p[3].changeType('-Expresion: literal entero('+str(p[3])+')')
	else:
		p[3].changeType('-Expresion: '+str(p[3]))

	if (isinstance(p[1],str)):
		p[1] = Node('-Contenedor: variable ("'+p[1]+'")',None,p[1],'variable')
		p[0] = Node('ASIGNACION', [p[1], p[3]], 'variable')
	else:
		p[0] = Node('ASIGNACION', [p[1], p[3]],'arreglo')


# Regla de la gramatica que reconoce los print
def p_Inst_Salida(p):
	'''Inst_Salida : TkPrint Operacion TkPuntoYComa '''
	p[0]= Node('PRINT', [p[2]], None, 'print')

# Regla de la gramatica que reconoce los read	
def p_Inst_Entrada(p):
	'''Inst_Entrada : TkRead TkId TkPuntoYComa '''
	p[0]= Node('READ', [Node(type='-Variable: '+p[2],leaf=p[2],tipo='variable')], None,'read')


#Regla de los errores sintacticos
def p_error(p):
	global parserErrorFound
	parserErrorFound = True
	print('Error de sintaxis en la linea: ' + str(p.lineno) + ', columna: '+str(obtenerColumna(p.lexer.lexdata,p))+', token inesperado: ' + str(p.type))
	exit()

# Funcion que recorre el arbol y lo imprime. 
def printTree(nodo, tabs):
	print('\t'*tabs + str(nodo))
	if not (isinstance(nodo, Node)):
		return
	for i in range(len(nodo.children)):
			if nodo.children[i] != None:
				printTree(nodo.children[i], tabs+1)

#Funcion que verifica si las variables ya fueron declaradas
def verifyVariable(nodo, level):
	global lista
	if not (isinstance(nodo, Node)):
		return
	
	if nodo.getLeaf() == 'with':
		level+=1

	for i in range(len(nodo.children)):
		verifyVariable(nodo.children[i], level)
		if nodo.children[i] != None:
			temp = nodo.children[i].getTipo()
			if temp == 'variable':
				buscando = lista.search(nodo.children[i].getLeaf(),level)
				if buscando != None:
					nodo.children[i].setTipo(buscando[0])
				else:
					print("La variable "+nodo.children[i].getLeaf()+" no ha sido declarada")
					exit()
			
			elif temp != None:
				if temp[:5] == "array":
					print("encontramos un array "+nodo.children[i].getLeaf())
					temp2 = ""
					while temp[:9] == "array de ":
						temp2 += temp[:9]
						temp = temp[9:]
						
					if temp == 'variable':
						buscando = lista.search(nodo.children[i].getLeaf(),level)
						if buscando != None:
							nodo.children[i].setTipo(temp2+buscando[0])
						else:
							print("La variable "+nodo.children[i].getLeaf()+" no ha sido declarada")
							exit()
	

#Funcion que verifica si las variables ya fueron declaradas
def verifySemantics(nodo):
	global lista
	if not (isinstance(nodo, Node)):
		return
	
	for i in range(len(nodo.children)):
		verifySemantics(nodo.children[i])
	
	fatherOperation = nodo.getLeaf()
	if fatherOperation == 'punto':
		if nodo.children[0].getTipo() != 'int':
			print("No puede hacer operaciones aritmeticas con la variable " + nodo.children[0].getLeaf())
			exit()
		if nodo.children[1].getTipo() != 'int':
			print("No puede asignar valores de tipo " + nodo.children[1].getTipo() + " a una variable de tipo int")
			exit()
		nodo.setTipo('int')

	elif fatherOperation == 'if':
		if nodo.children[0].getTipo() != 'bool':
			print("La condicion para una instruccion IF debe ser de tipo booleana")
			exit()
	
	elif fatherOperation == 'while':
		if nodo.children[0].getTipo() != 'bool':
			print("La condicion para una instruccion WHILE debe ser de tipo booleana")
			exit()

	elif fatherOperation == 'for':
		if nodo.children[0].getTipo() != 'int':
			print("La variable de iteracion no puede ser de tipo "+ str(nodo.children[0].getTipo()))
			exit()
		if nodo.children[1].getTipo() != 'int' or nodo.children[2].getTipo() != 'int':
			print("Los limites de iteracion deben ser de tipo int")
			exit()
		if len(nodo.children)==5:
			if nodo.children[3].getTipo() != 'int':
				print("No se puede iterar con steps de tipo "+nodo.children[4].getTipo())
				exit()

	elif fatherOperation == 'menos u':
		if nodo.children[0].getTipo() != 'int':
			print("El operador menos unitario solo puede ser usado con expresiones arimeticas")
			exit()
	
	elif fatherOperation == 'not':
		if nodo.children[0].getTipo() != 'bool':
			print("El operador NOT solo puede ser usado con expresiones booleanas")
			exit()

	elif fatherOperation == '+' or fatherOperation == '-' or fatherOperation == '*' or fatherOperation == '/' or fatherOperation == '%':
		if nodo.children[0].getTipo() != 'int' or nodo.children[1].getTipo() != 'int':
			print("El operador "+fatherOperation+" solo puede ser usado con expresiones aritmeticas")
			exit()
	
	elif fatherOperation == '<' or fatherOperation == '<=' or fatherOperation == '>' or fatherOperation == '>=' or fatherOperation == '=' or fatherOperation == '/=':
		if (nodo.children[0].getTipo() != 'bool' and nodo.children[0].getTipo() != 'int') or (nodo.children[1].getTipo() != 'int' and nodo.children[1].getTipo() != 'bool'):
			print("El operador "+fatherOperation+" solo puede ser usado con expresiones aritmeticas")
			exit()

	elif fatherOperation == '/\\' or fatherOperation == '\/':
		if (nodo.children[0].getTipo() != 'bool' or nodo.children[1].getTipo() != 'bool'):
			print("El operador "+fatherOperation+" solo puede ser usado con expresiones booleanas")
			exit()

	elif fatherOperation == 'index':
		if (nodo.children[0].getTipo()[:5] != 'array'):
			print("El operador "+fatherOperation+" solo puede ser usado con arreglos")
			exit()
		if (nodo.children[1].getTipo() != 'int'):
			print("Para indexar un arreglo la expresion debe ser artimetica")
			exit()
		nodo.setTipo(nodo.children[0].getTipo())

	elif fatherOperation == 'shift':
		if (nodo.children[0].getTipo()[:5] != 'array'):
			print("El operador "+fatherOperation+" solo puede ser usado con arreglos")
			exit() 
		nodo.setTipo(nodo.children[0].getTipo())

	elif fatherOperation == 'concat':
		if (nodo.children[0].getTipo() != nodo.children[1].getTipo() or nodo.children[0].getTipo()[:5] != 'array' or nodo.children[1].getTipo()[:5] != 'array'):
			print("El operador "+fatherOperation+" solo puede ser usado con array")
			exit()
		nodo.setTipo(nodo.children[0].getTipo())

	elif fatherOperation == 'ascii':
		if (nodo.children[0].getTipo() != 'char'):
			print("El operador ascii solo puede ser usado con expresiones de tipo char")
			exit()
		nodo.setTipo(nodo.children[0].getTipo())

	elif fatherOperation == 'siguiente':
		if (nodo.children[0].getTipo() != 'char'):
			print("El operador siguiente solo puede ser usado con expresiones de tipo char")
			exit()
		nodo.setTipo(nodo.children[0].getTipo())

	elif fatherOperation == 'anterior':
		if (nodo.children[0].getTipo() != 'char'):
			print("El operador anterior solo puede ser usado con expresiones de tipo char")
			exit()	
		nodo.setTipo(nodo.children[0].getTipo())

	if nodo.getType() == 'ASIGNACION':
		if fatherOperation=='variable':
			if (nodo.children[0].getTipo() != nodo.children[1].getTipo()):
				print("A la variable de tipo "+nodo.children[0].getTipo()+" no se le puede asignar un valor de tipo "+nodo.children[1].getTipo())
				exit()	
		else:
			pass


def main():
	global actualLevel
	if (len(sys.argv) != 2):
		print("Usage: python3 parser.py nombreArchivo")
		return -1
		
	# Construyendo el parser
	parser = yacc.yacc(errorlog=yacc.NullLogger())
		
	# Se abre el archivo con permisos de lectura
	string = str(open(str(sys.argv[1]),'r').read())
	result = parser.parse(string)
	
	#Si no hay errores, imprime el arbol.
	if (not lexerErrorFound) and (not parserErrorFound) and (not semanticErrorFound):
		printTree(result, 0)
		verifyVariable(result,-1)
		verifySemantics(result)
		print('\n***********************\n** TABLA DE SIMBOLOS **\n***********************\n')
		
		lista.printList()

#Llamada a la funcion
main()
