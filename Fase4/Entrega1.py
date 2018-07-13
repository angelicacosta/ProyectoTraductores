
# Autores:
# Giulianne Tavano. 13-11389
# Angelica Acosta. 14-10005

import ply.lex as lex
import sys

reservadas = {
	'with' 		: 'TkWith',
	'var' 		: 'TkVar',
	'begin'   	: 'TkBegin',
	'end'     	: 'TkEnd',
	'int'     	: 'TkInt',
	'bool'    	: 'TkBool',
	'char'		: 'TkChar',
	'array'		: 'TkArray',
	'of'		: 'TkOf',	 
	'read'    	: 'TkRead',
	'if'      	: 'TkIf',
	'otherwise'	: 'TkOtherwise',
	'for'     	: 'TkFor',
	'from'		: 'TkFrom',
	'step'		: 'TkStep',
	'to'		: 'TkTo',   
	'while'   	: 'TkWhile',
	'true'    	: 'TkTrue',
	'false'  	: 'TkFalse', 
	'print'		: 'TkPrint', 
	'not'		: 'TkNegacion'
}

lexerErrorFound = False

# Lista de tokens y se concatena el diccionario
tokens = ['TkNum', 'TkId', 'TkCaracter','TkComa', 'TkPuntoYComa', 'TkPunto', 'TkDosPuntos', 'TkParAbre', 
		'TkParCierra', 'TkCorcheteAbre', 'TkCorcheteCierra', 'TkHacer', 'TkAsignacion', 'TkSuma',
		'TkResta','TkMult', 'TkDiv', 'TkMod', 'TkConjuncion', 'TkDisyuncion',  'TkMenor',
		'TkMenorIgual', 'TkMayor', 'TkMayorIgual', 'TkShift', 'TkIgual', 'TkDesigual', 'TkSiguienteCar',
		'TkAnteriorCar', 'TkValorAscii', 'TkConca', 'TkSalto'] + list(reservadas.values())
		  
# Reglas de expresion regular para tokens:

t_TkSalto =r'\\n'
t_TkComa = r','
t_TkPuntoYComa = r'\;'
t_TkPunto = r'\.'
t_TkDosPuntos = r':' 
t_TkParAbre= r'\('
t_TkParCierra = r'\)'
t_TkCorcheteAbre = r'\['
t_TkCorcheteCierra = r'\]' 
t_TkHacer=r'->'
t_TkAsignacion  = r'<-'
t_TkSuma= r'\+'
t_TkResta= r'\-'
t_TkMult= r'\*'
t_TkDiv =r'/'
t_TkMod = r'\%'
t_TkConjuncion= r'/\\'
t_TkDisyuncion = r'\\\/'
t_TkMenor = r'<'
t_TkMenorIgual = r'<='
t_TkMayor = r'>'
t_TkMayorIgual = r'>='
t_TkIgual = r'='
t_TkDesigual = r'/='
t_TkSiguienteCar= r'[+][+]'
t_TkAnteriorCar =r'[-][-]'
t_TkValorAscii = r'\#'
t_TkConca = r'::' 
t_TkShift = r'\$'

# Regla que maneja los errores
def t_error(t):
	global lexerErrorFound
	lexerErrorFound = True
	print('Error: Caracter inesperado "' + t.value[0] + '" en la fila ' + str(t.lineno) + ", columna " + str(obtenerColumna(t.lexer.lexdata,t))) 
	t.lexer.skip(1)

# Regla de Expresion Regular 
def t_TkNum(t):
	r'\d+'
	t.value = int(t.value)    
	return t

# Regla que detecta saltos de linea
def t_newline(t):
	r'\n+'
	t.lexer.lineno += len(t.value)

# String que contiene los caracteres a ignorar (espacios, tabs y saltos de linea)
t_ignore  = ' \t'

# Se obtiene la columna donde esta un token.
def obtenerColumna(input,token):
	line_start = input.rfind('\n', 0, token.lexpos) + 1
	return (token.lexpos - line_start) + 1

# Expresion regular para las palabras reservadas.
def t_RESERVED(t):
	r'[a-zA-Z_][a-zA-Z_0-9]*'
	# Verificando las palabras reservadas
	t.type = reservadas.get(t.value,'TkId') 
	return t

def t_TkCaracter(t):
	r'[\'][a-zA-Z_][\']|["][a-zA-Z_]["]'
	t.type = reservadas.get(t.value,'TkCaracter') 
	return t

lexer = lex.lex()