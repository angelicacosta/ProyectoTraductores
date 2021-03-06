# Autores:
# Giulianne Tavano. 13-11389
# Angelica Acosta. 14-10005

import ply.yacc as yacc
import ply.lex as lex
import sys
import re
from Entrega1 import tokens, obtenerColumna, lexerErrorFound

parserErrorFound=False

# Indica cuales son el orden de precedencia de los tokens y hacia donde asocian.
precedence = (
	('left', 'TkSuma', 'TkResta'),
	('left', 'TkMult', 'TkDiv','TkMod'),
	('left', 'TkDisyuncion'),
	('right', 'TkConjuncion'),
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
	def __init__(self,type,children=None,leaf=None):
		self.type = type
		if children:
			self.children = children
		else:
			self.children = [ ]
		self.leaf = leaf
	def __str__(self):
		return str(self.type)

	#Cambia el type del nodo.
	def changeType(self,newType):
		self.type = newType

	#Obtiene el type del nodo.
	def getType(self):
		return self.type

	#Agrega hijos a un nodo.
	def addChildren(self,newChildren):
		self.children = [newChildren] + self.children

# Regla inicial de la gramatica
def p_S(p):
	''' S : TkWith Lista_Declaraciones Bloque_Inst 
	| Bloque_Inst
	'''
	if (p[1]=='with'):
		p[0] = p[3]
	else: 
		p[0] =  p[1]

# Regla de la gramatica que reconoce todas las declaracios de variables.
def p_Lista_Declaraciones(p):
	'''Lista_Declaraciones : TkVar Lista_Variables Lista_Declaraciones 
	| TkVar Lista_Variables '''

	if(len(p)>3):
		p[0]= str(p[2]) +'\n' + str(p[3])  
	else:
		p[0] = p[2]

# Regla de la gramatica que reconoce toda la lista de variables.
def p_Lista_Variables(p):
	''' Lista_Variables : TkId TkComa Lista_Variables 
	| TkId TkAsignacion Operacion TkComa Lista_Variables
	| TkId TkAsignacion Operacion TkDosPuntos Tipo
	| TkId TkDosPuntos Tipo '''
	if (len(p)==6):
		p[0]= str(Node(p[2], [p[1], p[3]], None)) + ' '+ str(p[5])
	else:
		p[0]= str(p[1]) +'\n' + str(p[3])   

# Regla de la gramatica que reconoce todo los tipos de variables.
def p_Tipo(p):
	''' Tipo : TkInt 
	| TkBool 
	| TkChar
	| TkArray  TkCorcheteAbre TkId TkCorcheteCierra TkOf TkId 
	| TkArray  TkCorcheteAbre TkNum TkCorcheteCierra TkOf TkId 
	'''
	if (p[1]=='array'):
		p[0] = Node(p[1], [p[2],p[3],p[4],p[6]], p[5])
	else: 
		p[0]=p[1]

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
	if (isinstance(p[3],int)):
		p[0]=Node('PUNTO', [Node('-Contenedor: '+p[1]),Node('-Expresion: '+str(p[3]))],None )
	else:
		p[3].changeType('-Expresion: '+str(p[3]))
		p[0]=Node('PUNTO', [Node('-Contenedor: '+p[1]),p[3]])

# Regla de la gramatica que reconoce todas instrucciones.
def p_Lista_Instrucciones(p):
	'''Lista_Instrucciones : Inst 
	|  Inst Lista_Instrucciones'''
	if(len(p)>2):
		if (p[2].getType() == "SECUENCIACION"):
			p[2].addChildren(p[1])
			p[0] = p[2]
		else:
			p[0]=Node('SECUENCIACION', [p[1],p[2]], None)

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
		p[0] = Node('CONDICIONAL', [p[2],p[4]], None)
	else:
		p[7].changeType('-Fracaso: '+str(p[7]))
		p[0] = Node('CONDICIONAL', [p[2],p[4],p[7]], None)

# Regla de la gramatica que reconoce todas instrucciones de ciclos o while.
def p_Inst_Bool(p):
	'''Inst_Bool : TkWhile Operacion TkHacer Lista_Instrucciones TkEnd
	'''
	p[2].changeType('-Guardia: '+str(p[2]))
	p[4].changeType('-Exito: '+str(p[4]))
	p[0] = Node('CICLO', [p[2],p[4]], None)
	
# Regla de la gramatica que reconoce todas instrucciones de iteracion determinada (For).
def p_Inst_For(p):
	'''Inst_For : TkFor TkId TkFrom Operacion TkTo Operacion TkHacer Inst TkEnd
	| TkFor TkId TkFrom Operacion TkTo Operacion TkStep Operacion TkHacer Inst TkEnd
	'''
	p[2] = Node('Variable de iteracion: (var "'+p[2]+'")', None, None)
	
	p[4].changeType('-Limite inferior: '+str(p[4]))
	
	p[6].changeType('-Limite superior: '+str(p[6]))

	if (len(p)==10):
		p[8].changeType('-Exito: '+str(p[8]))
		p[0]= Node('ITERACION DETERMINADA', [p[2],p[4],p[6],p[8]], None )
	else:
		p[8].changeType('-Step: '+str(p[8]))
		p[10].changeType('-Exito: '+str(p[10]))
		p[0]= Node('ITERACION DETERMINADA', [p[2], p[4], p[6], p[8], p[10]], None )

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
			p[0] = Node('literal entero ('+str(p[1])+')', None, None)
		elif (p[1] == 'true'):
			p[0] = Node('booleano (TRUE)+', None, None)
		elif (p[1] == 'false'):
			p[0] = Node('booleano (FALSE)+', None, None)
		elif isinstance(p[1],Node):
			p[0] = p[1]
		else:
			caracter = re.compile('[\'][a-zA-Z_][\']|["][a-zA-Z_]["]')
			if caracter.match(p[1]):
				p[0] = Node('caracter ('+p[1]+')', None, None)
			else:
				p[0] = Node('variable ("'+p[1]+'")', None, None)
	
	elif (len(p) == 3):
		if (p[1] == '-'):
			p[0]=Node("MENOS UNARIO",  [p[2]], None)
		else:
			p[0]=Node("NOT",  [p[2]], None)
	
	else:
		if not (p[1]=='('):
			p[1].changeType('-Operando izquierdo: '+str(p[1]))
			p[3].changeType('-Operando derecho: '+str(p[3]))
		else:
			p[0] = p[2]

		if (p[2] == '+'):
			p[0] = Node("SUMA", [p[1],p[3]], p[2])
		elif (p[2] == '-'):
			p[0] = Node("RESTA", [p[1],p[3]], p[2])
		elif (p[2] == '*'):
			p[0] = Node("MULTIPLICACION", [p[1],p[3]], p[2])
		elif (p[2] == '/'):
			p[0] = Node("DIVISION", [p[1],p[3]], p[2])
		elif (p[2] == '%'):
			p[0] = Node("MODULO", [p[1],p[3]], p[2])
		elif (p[2] == '<'):
			p[0]=Node("MENOR QUE", [p[1],p[3]], p[2])
		elif (p[2] == '<='):
			p[0]=Node("MENOR O IGUAL QUE", [p[1],p[3]], p[2])
		elif (p[2] == '>'):
			p[0]=Node("MAYOR", [p[1],p[3]], p[2])
		elif (p[2] == '>='):
			p[0]=Node("MAYOR QUE", [p[1],p[3]], p[2])
		elif (p[2] == '='):
			p[0]=Node("IGUAL QUE", [p[1],p[3]], p[2])
		elif (p[2] == '/='):
			p[0]=Node("DESIGUAL QUE", [p[1],p[3]], p[2])
		elif (p[2] == '/\\'):
			p[0]=Node("CONJUNCION", [p[1],p[3]], p[2])
		elif (p[2] == '\/'):
			p[0]=Node("DISYUNCION", [p[1],p[3]], p[2])

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
			p[1] = Node("-Arreglo: "+p[1])
		else:
			p[1].changeType("-Arreglo: "+str(p[1]))
		p[0] = Node("INDEXACION", [p[1],p[3]])
	
	elif (len(p)==3):
		if isinstance(p[2],str):
			p[2] = Node("-Arreglo: "+p[2])
		else:
			p[2].changeType("-Arreglo: "+str(p[2]))
		p[0] = Node("SHIFT", [p[2]])
	
	else:
		if isinstance(p[1],str):
			p[1] = Node("-Arreglo izquierdo: "+p[1])
		else:
			p[1].changeType("-Arreglo izquierdo: "+str(p[1]))
		if isinstance(p[3],str):
			p[3] = Node("-Arreglo derecho: "+p[3])
		else:
			p[3].changeType("-Arreglo derecho: "+str(p[3]))
		
		p[0] = Node("CONCATENACION", [p[1],p[3]])


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
	caracter = re.compile('[\'][a-zA-Z_][\']|["][a-zA-Z_]["]')
	if (p[1]=='#'):
		if isinstance(p[2],Node):
			p[2].changeType('-Caracter: '+str(p[2]))
		elif caracter.match(p[2]):
			p[2] = Node('-Caracter: '+str(p[2]))
		else:
			p[2] = Node('-Variable: '+str(p[2]))
		p[0] = Node('VALOR ASCII',[p[2]])
	
	else:
		if isinstance(p[1],Node):
			p[1].changeType('-Caracter: '+str(p[1]))
		elif caracter.match(p[1]):
			p[1] = Node('-Caracter: '+str(p[1]))
		else:
			p[1] = Node('-Variable: '+str(p[1]))
		
		if (p[2]=='[+][+]'):
			p[0] = Node('CARACTER SIGUIENTE',[p[1]])
		else:
			p[0] = Node('CARACTER ANTERIOR',[p[1]])
				


# Regla de la gramatica utilizada para reconocer una asignacion
def p_Inst_Asignacion(p):
	'''Inst_Asignacion : TkId TkAsignacion Operacion TkPuntoYComa
	| Op_Arreglo TkAsignacion Operacion TkPuntoYComa'''  
	
	p[1] = Node('-Contenedor: variable ("'+p[1]+'")',None,None)
	if (isinstance(p[3].type,int)):
		p[3].changeType('-Expresion: literal entero('+str(p[3])+')')
	else:
		p[3].changeType('-Expresion: '+str(p[3]))
	p[0] = Node('ASIGNACION', [p[1], p[3]], None)

# Regla de la gramatica que reconoce los print
def p_Inst_Salida(p):
	'''Inst_Salida : TkPrint Operacion TkPuntoYComa '''
	p[0]= Node('PRINT', [p[2]], None)

# Regla de la gramatica que reconoce los read	
def p_Inst_Entrada(p):
	'''Inst_Entrada : TkRead TkId TkPuntoYComa '''
	p[0]= Node('READ', [p[2]], None)


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


def main():
	if (len(sys.argv) != 2):
		print("Usage: python3 parser.py nombreArchivo")
		return -1
		
	# Construyendo el parser
	parser = yacc.yacc(errorlog=yacc.NullLogger())
		
	# Se abre el archivo con permisos de lectura
	string = str(open(str(sys.argv[1]),'r').read())
	result = parser.parse(string)
	
	#Si no hay errores, imprime el arbol.
	if (not lexerErrorFound) and (not parserErrorFound):
		printTree(result, 0)

#Llamada a la funcion
main()
