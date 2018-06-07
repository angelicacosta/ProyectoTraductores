
import ply.yacc as yacc
import ply.lex as lex
import sys
import re
from Entrega1 import tokens, obtenerColumna

precedence = (
	('left', 'TkSuma', 'TkResta'),
	('left', 'TkMult', 'TkDiv','TkMod'),
	#('right', 'UMINUS'),
	('left', 'TkDisyuncion'),
	('right', 'TkConjuncion'),
	('right', 'TkNegacion'),
	('left','TkSiguienteCar'),
	('left', 'TkAnteriorCar'), 
	('left', 'TkValorAscii'),
	('left','TkConca'),
	('left','TkShift'),
	('left', 'TkCorcheteAbre', 'TkCorcheteCierra'),
	('nonassoc', 'TkMenor', 'TkMenorIgual','TkMayor','TkMayorIgual','TkIgual','TkDesigual'),
	('left', 'TkParAbre','TkParCierra'),
	('left', 'TkHacer')
	#('right', 'UMINUS'),
)

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

	def changeType(self,newType):
		self.type = newType

	def getType(self):
		return self.type

	def addChildren(self,newChildren):
		self.children = [newChildren] + self.children

def p_S(p):
	''' S : TkWith Lista_Declaraciones Bloque_Inst 
	| Bloque_Inst
	'''
	if (p[1]=='with'):
		p[0] = p[3]
		pass
	else: 
		p[0] =  p[1]

def p_Lista_Declaraciones(p):
	'''Lista_Declaraciones : TkVar Lista_Variables Lista_Declaraciones 
	| TkVar Lista_Variables '''

	if(len(p)>3):
		p[0]= str(p[2]) +'\n' + str(p[3])  
	else:
		p[0] = p[2]

def p_Lista_Variables(p):
	''' Lista_Variables : TkId TkComa Lista_Variables 
	| TkId TkAsignacion Operacion TkComa Lista_Variables
	| TkId TkAsignacion Operacion TkDosPuntos Tipo
	| TkId TkDosPuntos Tipo '''
	if (len(p)==6):
		p[0]= str(Node(p[2], [p[1], p[3]], None)) + ' '+ str(p[5])
	else:
		p[0]= str(p[1]) +'\n' + str(p[3])   

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

def p_Bloque_Inst(p):	
	'''Bloque_Inst : TkBegin Lista_Instrucciones TkEnd 
	| TkBegin TkEnd 
	'''
	if(p[2]!='end'):	
		p[0]= p[2]
	else:
		p[0] = p[1] + p[2]

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

def p_Inst_Punto(p):
	'''Inst_Punto : TkId TkPunto Operacion TkPuntoYComa '''
	if (isinstance(p[3],int)):
		p[0]=Node('PUNTO', [Node('-Contenedor: '+p[1]),Node('-Expresion: '+str(p[3]))],None )
	else:
		p[3].changeType('-Expresion: '+str(p[3]))
		p[0]=Node('PUNTO', [Node('-Contenedor: '+p[1]),p[3]])

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
	  

def p_Inst_If(p):
	'''Inst_If : TkIf Operacion TkHacer Lista_Instrucciones TkEnd
	| TkIf Operacion TkHacer Lista_Instrucciones TkOtherwise TkHacer Lista_Instrucciones TkEnd
	'''
	
	p[2].changeType('-Guardia: '+str(p[2]))
	p[4].changeType('-Exito: '+str(p[4]))
	if (len(p)==6):
		p[0] = Node('CONDICIONAL', [p[2],p[4]], None)
	else:
		p[7].changeType('-En otro caso: '+str(p[7]))
		p[0] = Node('CONDICIONAL', [p[2],p[4],p[7]], None)


def p_Inst_Bool(p):
	'''Inst_Bool : TkWhile Operacion TkHacer Lista_Instrucciones TkEnd
	'''
	p[2].changeType('-Guardia: '+str(p[2]))
	p[4].changeType('-Exito: '+str(p[4]))
	p[0] = Node('CICLO', [p[2],p[4]], None)
	

def p_Inst_For(p):
	'''Inst_For : TkFor TkId TkFrom Expresion TkTo Expresion TkHacer Inst TkEnd
	| TkFor TkId TkFrom Expresion TkTo Expresion TkStep Expresion TkHacer Inst TkEnd
	'''
	p[2] = Node('Variable de iteracion: (var "'+p[2]+'")', None, None)
	
	if isinstance(p[4],int):
		p[4] = Node('-Limite inferior: literal entero ('+str(p[4])+')', None, None)
	else:
		p[4]= Node('-Limite inferior: variable ("'+p[4]+'")', None, None)
	
	if isinstance(p[6],int):
		p[6] = Node('-Limite superior: literal entero ('+str(p[6])+')', None, None)
	else:
		p[6] = Node('-Limite superior: variable ("'+p[6]+'")', None, None)


	if (len(p)==10):
		p[8].changeType('-Exito: '+str(p[8]))
		p[0]= Node('ITERACION DETERMINADA', [p[2],p[4],p[6],p[8]], None )
	else:
		if isinstance(p[8],int):
			p[8].changeType('-Step: literal entero ('+str(p[8])+')')
		else:
			p[8].changeType('-Step: variable ("'+str(p[8])+'")')
		p[10].changeType('-Exito:'+str(p[10]))
		p[0]= Node('ITERACION DETERMINADA', [p[2], p[4], p[6], p[8], p[10]], None )

def p_Expresion(p):
	'''Expresion : TkNum
	| TkId'''
	
	p[0]=p[1]


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

def p_Op_Arreglo(p):
	'''Op_Arreglo : TkId TkCorcheteAbre Operacion TkCorcheteCierra 
	| TkShift TkId 
	| TkId TkConca TkId
	'''
	p[0]=p[1]

def p_OpCaracter(p):
	'''OpCaracter : TkCaracter TkSiguienteCar 
	| TkCaracter TkAnteriorCar 
	| TkValorAscii TkCaracter
	| TkId TkSiguienteCar 
	| TkId TkAnteriorCar 
	| TkValorAscii TkId
	'''
	caracter = re.compile('[\'][a-zA-Z_][\']|["][a-zA-Z_]["]')
	if (p[1]=='#'):
		if caracter.match(p[2]):
			p[0] = Node('VALOR ASCII',[Node('-Caracter: '+str(p[2]))])
		else:
			p[0] = Node('VALOR ASCII',[Node('-Variable: '+str(p[2]))])
	else:
		if (p[2]=='[+][+]'):
			if caracter.match(p[1]):
				p[0] = Node('CARACTER SIGUIENTE',[Node('-Caracter: '+str(p[1]))])
			else:
				p[0] = Node('CARACTER SIGUIENTE',[Node('-Variable: '+str(p[1]))])
		else:
			if caracter.match(p[1]):
				p[0] = Node('CARACTER SIGUIENTE',[Node('-Caracter: '+str(p[1]))])
			else:
				p[0] = Node('CARACTER SIGUIENTE',[Node('-Variable: '+str(p[1]))])	


#Regla de la gramatica utilizada para reconocer una asignacion
def p_Inst_Asignacion(p):
	'''Inst_Asignacion : TkId TkAsignacion Operacion TkPuntoYComa
	| Op_Arreglo TkAsignacion Operacion TkPuntoYComa'''  
	
	p[1] = Node('-Contenedor: variable ("'+p[1]+'")',None,None)
	if (isinstance(p[3].type,int)):
		p[3].changeType('-Expresion: literal entero('+str(p[3])+')')
	else:
		p[3].changeType('-Expresion: '+str(p[3]))
	p[0] = Node('ASIGNACION', [p[1], p[3]], None)

def p_Inst_Salida(p):
	'''Inst_Salida : TkPrint Operacion TkPuntoYComa '''
	p[0]= Node('PRINT', [p[2]], None)

	
def p_Inst_Entrada(p):
	'''Inst_Entrada : TkRead TkId TkPuntoYComa '''
	p[0]= Node('READ', [p[2]], None)


#Error rule for syntax errors
#def p_error(p):
#	print ("Error de sintaxis en la linea")

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
	parser = yacc.yacc()
		
	# Se abre el archivo con permisos de lectura
	string = str(open(str(sys.argv[1]),'r').read())
	result = parser.parse(string)

	printTree(result, 0)

main()
