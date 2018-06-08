
# Autores:
# Giulianne Tavano. 13-11389
# Angelica Acosta. 14-10005

Fase 2:

El programa se ejecuta con el archivo BasicTran brindado en la entrega, con el comando:
./BasicTran ⟨Archivo⟩


La gramatica que construimos para el BasicTran consiste en:

	S : TkWith Lista_Declaraciones Bloque_Inst 
	| Bloque_Inst

	Lista_Declaraciones : TkVar Lista_Variables Lista_Declaraciones 
	| TkVar Lista_Variables 

	Lista_Variables : TkId TkComa Lista_Variables 
	| TkId TkAsignacion Operacion TkComa Lista_Variables
	| TkId TkAsignacion Operacion TkDosPuntos Tipo
	| TkId TkDosPuntos Tipo 

	Tipo : TkInt 
	| TkBool 
	| TkChar
	| TkArray  TkCorcheteAbre TkId TkCorcheteCierra TkOf TkId 
	| TkArray  TkCorcheteAbre TkNum TkCorcheteCierra TkOf TkId 
	
	Bloque_Inst : TkBegin Lista_Instrucciones TkEnd 
	| TkBegin TkEnd 

	Inst : Inst_Asignacion 
	| Inst_If 
	| Inst_Bool 
	| Inst_For 
	| Inst_Entrada 
	| Inst_Salida
	| Inst_Punto
	| S

	Inst_Punto : TkId TkPunto Operacion TkPuntoYComa 

	Lista_Instrucciones : Inst 
	|  Inst Lista_Instrucciones

	Inst_If : TkIf Operacion TkHacer Lista_Instrucciones TkEnd
	| TkIf Operacion TkHacer Lista_Instrucciones TkOtherwise TkHacer Lista_Instrucciones TkEnd

	Inst_Bool : TkWhile Operacion TkHacer Lista_Instrucciones TkEnd

	Inst_For : TkFor TkId TkFrom Operacion TkTo Operacion TkHacer Inst TkEnd
	| TkFor TkId TkFrom Operacion TkTo Operacion TkStep Operacion TkHacer Inst TkEnd

	Operacion : TkParAbre Operacion TkParCierra
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

	Op_Arreglo : TkId TkCorcheteAbre Operacion TkCorcheteCierra 
	| TkShift TkId 
	| TkId TkConca TkId

	OpCaracter : TkCaracter TkSiguienteCar 
	| TkCaracter TkAnteriorCar 
	| TkValorAscii TkCaracter
	| TkId TkSiguienteCar 
	| TkId TkAnteriorCar 
	| TkValorAscii TkId

	Inst_Asignacion : TkId TkAsignacion Operacion TkPuntoYComa
	| Op_Arreglo TkAsignacion Operacion TkPuntoYComa

	Inst_Salida : TkPrint Operacion TkPuntoYComa 

	Inst_Entrada : TkRead TkId TkPuntoYComa 

Observaciones: 
- Se trabajo con todas las operaciones booleanas, relacionales, aritmeticas, de caracteres y arreglos en una sola regla.
Esto debido a que varias instrucciones hacian uso de eso.


