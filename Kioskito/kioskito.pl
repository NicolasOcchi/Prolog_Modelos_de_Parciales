% Nicolas Daniel Occhi

%%%% PUNTO 1: calentando motores (2 puntos) %%%%

% atiende(Persona, Dia, HorarioInicio, HorarioFinalizacion).

% dodain atiende lunes, miércoles y viernes de 9 a 15.
atiende(dodain, lunes, 9, 15).
atiende(dodain, miercoles, 9,15).
atiende(dodain, viernes, 9,15).

% lucas atiende los martes de 10 a 20
atiende(lucas, martes, 10, 20).

% juanC atiende los sábados y domingos de 18 a 22.
atiende(juanC, sabados, 18,22).
atiende(juanC, domingo, 18,22).

% juanFdS atiende los jueves de 10 a 20 y los viernes de 12 a 20.
atiende(juanFdS, jueves, 10,20).
atiende(juanFdS, viernes, 12,20).

% leoC atiende los lunes y los miércoles de 14 a 18.
atiende(leoC, lunes, 14,18).
atiende(leoC, miercoles, 14,18).

% martu atiende los miércoles de 23 a 24.
atiende(martu, miercoles, 23, 24).


% Definir la relación para asociar cada persona con el rango horario que cumple, e incorporar las siguientes cláusulas:
% - vale atiende los mismos días y horarios que dodain y juanC.
atiende(valen, Dia, HorarioInicio, HorarioFinalizacion):-
	atiende(dodain, Dia, HorarioInicio, HorarioFinalizacion).

atiende(valen, Dia, HorarioInicio, HorarioFinalizacion):-
	atiende(juanC, Dia, HorarioInicio, HorarioFinalizacion).
	
% nadie hace el mismo horario que leoC
% rta que dijo el profe: por principio de universo cerrado no agregamos al base de conocimiento aquello que no tiene sentido agregar

% maiu está pensando si hace el horario de 0 a 8 los martes y miércoles
% rta que dijo el profe: por principio de universo cerrador lo desconocido se presume como falso, entonces no lo agregamos...


%%%% PUNTO 2: quién atiende el kiosko... (2 puntos) %%%%

quienAtiende(Persona, Dia, Horario):- 
	% atiende(Persona, _ ,_, _), % se liga la variable persona
	% atiende(Persona, Dia ,_, _), % se liga la variable dia
	% NO HACEN FALTA... ver clase
	atiende(Persona, Dia, HorarioInicio, HorarioFinalizacion), % se liga la variable HorarioInicio y HorarioFinalizacion
	between(HorarioInicio, HorarioFinalizacion, Horario).
	

%%%% PUNTO 3: Forever alone (2 puntos) %%%%

esForeverAlone(Persona, Dia, Horario):-
	quienAtiende(Persona, Dia, Horario),
	not((quienAtiende(OtraPersona, Dia, Horario), Persona \= OtraPersona)).

%%%% PUNTO 4: posibilidades de atención (3 puntos / 1 punto) %%%%

% Dado un día, queremos relacionar qué personas podrían estar atendiendo el kiosko
% en algún momento de ese día.

% solucion del profe lo hizo un poquito distinto
posibilidadesAtencion(Dia, Personas):-
	findall(Persona, distinct(Persona, quienAtiende(Persona, Dia, _)), PersonasPosibles),
	combinar(PersonasPosibles, Personas).
  
  combinar([], []).
  combinar([Persona|PersonasPosibles], [Persona|Personas]):-combinar(PersonasPosibles, Personas).
  combinar([_|PersonasPosibles], Personas):-combinar(PersonasPosibles, Personas).


% solucion que me salio en su momento
posiblesKioskeros(Dia, Vendedores):-
	findall(Persona, atiende(Persona, Dia, _,_), Personas),
	posiblesVendedores(Dia, Personas, Vendedores).

posiblesVendedores(_, [], []).
posiblesVendedores(Dia, [Persona|Personas], [Persona|Posibles]):-
	atiende(Persona, Dia, _,_),
	posiblesVendedores(Dia, Personas, Posibles).
posiblesVendedores(Dia, [_|Personas], Posibles):- % PONER "POSIBLES" y NOOO "[_|Posibles]" porque arma las listas con valores basura
	posiblesVendedores(Dia, Personas, Posibles).

% Punto extra: indique qué conceptos en conjunto permiten 
% resolver este requerimiento, justificando su respuesta.

% El concepto general que permite resolver este requerimento es "la exploción combinatoria".

% RTA DEL PROFE: (explosion combinatoria es un termino casero, el formal seria mecanismo de backtracking de prolog)
% Qué conceptos en conjunto resuelven este requerimiento 
% - findall como herramienta para poder generar un conjunto de soluciones que satisfacen un predicado 
% - mecanismo de backtracking de Prolog permite encontrar todas las satuciones posibles 

% Rta que se me ocurrio:
% Dicho conecepto consiste en hacer uso de recursividad a la par del motor de prolog
% el cual se caracteriza por encontrar los predicados que se satisfacen siendo estos no deterministicos.

% PUNTO 5: ventas / suertudas (4 puntos)

% usamos listas porque nos importa el orden, ya que queremos saber que es lo primero que vendió

% dodain hizo las siguientes ventas el lunes 10 de agosto: golosinas por $ 1200, cigarrillos Jockey, golosinas por $ 50
venta(dodain, lunes(10), [golosinas(1200), cigarrillos([jockey]), golosinas(50)]).

venta(dodain, miercoles(12), [bebidas(alcoholicas, 8), bebidas(no_alcoholicas, 1), golosinas(10)]).

venta(martu, miercoles(12), [golosinas(1000), cigarrillos([jockey, chesterfield, colorado, parisiennes])]).

venta(lucas, martes(11), [golosinas(600)]).

venta(lucas, martes(18), [bebidas(no_alcoholicas, 2), cigarrillos([derby])]).

esSuertuda(Persona):-
	venta(Persona, _ , _),
	forall(venta(Persona, _, [Venta|_]), ventaImportante(Venta)).

ventaImportante(golosinas(Monto)):- Monto > 100.
ventaImportante(cigarrillos(Marcas)):- length(Marcas, Largo), Largo > 2.
ventaImportante(bebidas(alcoholicas,_)).
ventaImportante(bebidas(no_alcoholicas, Cantidad)):- Cantidad > 5.

% esto de aca abajo es lo que me habia salido cuando lo hice, un poco mas rebuscado pero se llega a lo mismo

% esSuertuda(Persona):-
% 	venta(Persona, _ , _),
% 	% venta(Persona, Dia, _),
% 	% venta(Persona, Dia, Ventas), NO VA YA QUE HARIA QUE AL FORALL LE LLEGUEN TODAS LAS VAIABLES UNIFICADAS, LO QUE NO TENDRIA SENTIDO.
% 	% la fecha no importa
% 	forall(venta(Persona, _, Ventas), primeraVentaImportante(Ventas)).

% primeraVentaImportante(Ventas):-
% 	nth0(0, Ventas, PrimerProductoVendido),
% 	productoImportante(PrimerProductoVendido).



	







:- begin_tests(kioskito).

	test(fakeTest):-
		true.

:- end_tests(kioskito).