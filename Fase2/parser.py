
import ply.yacc as yacc
import ply.lex as lex
import sys
from Entrega1 import tokens, obtenerColumna

class Node:
	def __init__(self,type,children=None,leaf=None):
		self.type = type
		if children:
			self.children = children
		else:
			self.children = [ ]
		self.leaf = leaf
		#print("type: "+self.type+", children[0]: "+str(children[0])+", children[1]: "+str(children[1])+", leaf: "+str(leaf))
	
	def __str__(self):
		return str(self.type) + '\n'

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
	if (p[2]==','):
		p[0]= str(p[1]) + ' '+ str(p[3])
	elif (len(p)==6):
		p[0]= str(Node(p[2], [p[1], p[3]], None)) + ' '+ str(p[5])
	else: 

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
	| OpCaracter
	| Inst_Punto
	'''
	p[0] = p[1]

def p_Inst_Punto(p):
	'''Inst_Punto : TkId TkPunto  TkPuntoYComa ''' ##Chequear
	p[0]=Node(p[2], [p[1]],None )

def p_Lista_Instrucciones(p):
	'''Lista_Instrucciones : Inst 
	|  Inst Lista_Instrucciones'''
	if(len(p)>2):
		#print('Secuenciacion \n')
		p[0]=Node('Secuencia', [p[1],p[2]], None)
		#p[0]= str(p[1]) +'\n' + str(p[2])  
	else:
		p[0] = p[1]
	  

def p_Inst_If(p):
	'''Inst_If : TkIf Operacion TkHacer Lista_Instrucciones TkEnd
	| TkIf Operacion TkHacer Lista_Instrucciones TkOtherwise TkHacer Lista_Instrucciones TkEnd
	'''
	if(len(p)==6):
		p[0]= Node('Condicional', [Node(p[1],[p[2],p[4]], None)], p[5])
	else:
		p[0]= Node('Condicional', [Node(p[1],[p[2], p[4]]), Node(p[5],[p[7]])], p[8])


def p_Inst_Bool(p):
	'''Inst_Bool : TkWhile Operacion TkHacer Lista_Instrucciones TkOtherwise TkHacer Lista_Instrucciones TkEnd
	| TkWhile Operacion TkHacer Lista_Instrucciones TkEnd
	'''
	if (len(p)==6):
		p[0]= Node(p[1], [p[2], p[4]], p[1])
	else:
		p[0]= Node(p[1], [p[2], p[4], p[7]], p[1])

def p_Inst_For(p):
	'''Inst_For : TkFor TkId TkFrom TkNum TkTo TkNum 
	| TkFor TkId TkFrom TkNum TkTo TkNum TkStep Operacion TkHacer Inst TkEnd
	'''
	if (len(p)==6):
		p[0]= Node(p[1], [p[2], p[3], p[4], p[5], p[6]], None )
	else:
		pass 


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
    | TkId  
    | TkNum
    | TkTrue 
    | TkFalse
    | TkCaracter
	'''
	if (len(p) == 2):
		if (isinstance(p[1], int) ):
			p[0]=Node(p[1], None, None)
		elif (isinstance(p[1], str) ):
			p[0]=Node(p[1], None, None)
		elif (isinstance(p[1], str)):  #Investigar caracter
			p[0]=Node("Id", None, p[1])
		elif(p[1] == 'true'):
			p[0]=Node(p[1], None, None)
		elif(p[1] == 'false'):
			p[0]=Node(p[1], None, None)
		else:
			p[0]=Node("Caracter", None, p[1])
	elif (len(p) == 3):
		if (p[1]=='-'):
			p[0]=Node(p[1],  [p[2]], None)
		else:
			p[0]=Node(p[1],  [p[2]], None)
	else:
		if (p[2] == '+'):
			p[0]=Node(p[2],  [p[1],p[3]], None)
		elif (p[2] == '-'):
			p[0]=Node(p[2], [p[1],p[3]], None)
		elif (p[2] == '*'):
			p[0]=Node(p[2], [p[1],p[3]], None)
		elif (p[2] == '/'):
			p[0]=Node(p[2],  [p[1],p[3]], None)
		elif (p[2] == '%'):
			p[0]=Node(p[2],  [p[1],p[3]], None)
		elif (p[2] == '<'):
			p[0]=Node(p[2],  [p[1],p[3]], None)
		elif (p[2] == '<='):
			p[0]=Node(p[2],  [p[1],p[3]], None)
		elif (p[2] == '>'):
			p[0]=Node(p[2],  [p[1],p[3]], None)
		elif (p[2] == '>='):
			p[0]=Node(p[2],  [p[1],p[3]],None)
		elif (p[2] == '='):
			p[0]=Node(p[2],  [p[1],p[3]], None)
		elif (p[2] == '/='):
			p[0]=Node(p[2],  [p[1],p[3]],None)
		elif (p[2] == '/\\'):
			p[0]=Node(p[2],  [p[1],p[3]], None)
		elif (p[2] == '\/'):
			p[0]=Node(p[2],  [p[1],p[3]], None)

'''
def p_Op_Aritmetica(p):
	Op_Aritmetica : Op_Aritmetica TkSuma Op_Aritmetica 
	| Op_Aritmetica TkResta Op_Aritmetica 
	| Op_Aritmetica TkMult Op_Aritmetica 
	| Op_Aritmetica TkDiv Op_Aritmetica 
	| Op_Aritmetica TkMod Op_Aritmetica 
	| TkResta Op_Aritmetica 
	| Exp 
	
	if (len(p) == 2):
		if (p[1] != TkNum):
			Node("Id", None, p[1])
		else:
			Node("Entero", None, p[1])
	elif (len(p) == 3):
		Node("Menos Unario", [None,p[2]], p[1])
	else:
		if (p[2] == TkSuma):
			Node("Suma", [p[1],p[3]], p[2])
		elif (p[2] == TkResta):
			Node("Resta", [p[1],p[3]], p[2])
		elif (p[2] == TkMult):
			Node("Multiplicacion", [p[1],p[3]], p[2])
		elif (p[2] == TkDiv):
			Node("Division", [p[1],p[3]], p[2])
		elif (p[2] == TkMod):
			Node("Modulo", [p[1],p[3]], p[2])

def p_Op_booleana(p):
	Op_booleana :  Operacion
	
	p[0]=p[1]

def p_OpRelacional(p):
	OpRelacional : Op_Aritmetica TkMenor Op_Aritmetica
	| Op_Aritmetica TkMenorIgual Op_Aritmetica 
	| Op_Aritmetica TkMayor Op_Aritmetica 
	| Op_Aritmetica TkMayorIgual Op_Aritmetica 
	| Op_Aritmetica TkIgual Op_Aritmetica 
	| Op_Aritmetica TkDesigual Op_Aritmetica 
	
	p[0]=p[1]

'''

def p_Op_Arreglo(p):
	'''Op_Arreglo : TkId TkCorcheteAbre TkId TkCorcheteCierra 
	| TkId TkCorcheteAbre TkNum TkCorcheteCierra 
	| TkShift TkId 
	| TkId TkConca TkId
	'''
	p[0]=p[1]

def p_OpCaracter(p):
	'''OpCaracter : TkId TkSiguienteCar 
	| TkId TkAnteriorCar 
	| TkValorAscii TkId
	'''
	p[0]=p[1]

#Regla de la gramatica utilizada para reconocer una asignacion
def p_Inst_Asignacion(p):
	'''Inst_Asignacion : TkId TkAsignacion Operacion TkPuntoYComa
	| TkId TkAsignacion Op_Arreglo TkPuntoYComa'''  
	
	#p[0] = 'ASIGNACION \n\t - contenedor: VARIABLE \n\t - identificador: ' + str(p[1]) +'\n\t - expresion: '+ tipo + ' \n\t - valor:' + str(p[3])
	p[0] = Node(p[2], [p[1], p[3]], None)

def p_Inst_Salida(p):
	'''Inst_Salida : TkPrint Operacion TkPuntoYComa
	| TkPrint Op_Arreglo TkPuntoYComa'''
	p[0]= Node(p[1], [p[2]], None)
	#p[0] = 'PRINT ' + p[1] + ' expresion: ' + p[2]

	
def p_Inst_Entrada(p):
	'''Inst_Entrada : TkRead TkId TkPuntoYComa '''
	p[0]= Node(p[1], [p[2]], None)
	#p[0] = 'READ ' + p[1] + ' identificador: ' + p[2]


# Error rule for syntax errors
def p_error(p):
	print ("Error de sintaxis en la linea")


def printTree(nodo, tabs):
	print('\t'*tabs + str(nodo))
	if not (isinstance(nodo, Node)):
		return
	for i in range(len(nodo.children)):
			if nodo.children[i] != None:
				printTree(nodo.children[i], tabs+1)

# Build the parser
parser = yacc.yacc()

s = input('Ingresa: ')
result = parser.parse(s)
printTree(result, 0)
