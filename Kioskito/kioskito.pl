% Nicolas Daniel Occhi

%%%% PUNTO 1: calentando motores (2 puntos) %%%%

% alternativa lista de functores (creo que es medio inhumana esta forma).

% atiende(Persona, Horarios).
% Horarios es una lista de functores

% atiende(dodain, [lunes(9,15), miercoles(9,15), viernes(9,15)]).
% atiende(lucas, [martes(10,20)]).
% atiende(juanC, [sabados(18,22), domingo(18,22)]).
% atiende(juanFdS, [jueves(10,20), viernes(12,20)]).
% atiende(leoC, [lunes(14,18), miercoles(14,18)]).
% atiende(martu, [miercoles(23,24)]).




% alterinativa haciendo hecho por hecho.

% atiende(dodain, lunes(9,15)).
% atiende(dodain, miercoles(9,15)).
% atiende(dodain, viernes(9,15)).

% atiende(lucas, martes(10,20)).

% atiende(juanC, sabados(18,22)).
% atiende(juanC, domingo(18,22)).

% atiende(juanFdS, jueves(10,20)).
% atiende(juanFdS, viernes(12,20)).

% atiende(leoC, lunes(14,18)).

% atiende(leoC, miercoles(14,18)).

% atiende(martu, miercoles(23,24)).


% alterinativa haciendo hecho por hecho VERSION 2
% atiende(Persona, Dia, HorarioInicio, HorarioFinalizacion).
atiende(dodain, lunes, 9, 15).
atiende(dodain, miercoles, 9,15).
atiende(dodain, viernes, 9,15).

atiende(lucas, martes, 10, 20).

atiende(juanC, sabados, 18,22).
atiende(juanC, domingo, 18,22).

atiende(juanFdS, jueves, 10,20).
atiende(juanFdS, viernes, 12,20).

atiende(leoC, lunes, 14,18).

atiende(leoC, miercoles, 14,18).

atiende(martu, miercoles, 23, 24).


% vale atiende los mismos días y horarios que dodain y juanC.
% para la alternitiva con lista de functores.
% atiende(valen, HorariosValen):-
% 	atiende(dodain, HorariosDodain),
% 	atiende(juanC, HorariosJuanC),
% 	append(HorariosDodain, HorariosJuanC, HorariosValen).

% para la alternitiva con hecho por hecho.
% atiende(valen, HorariosValen):-
% 	atiende(dodain, HorariosDodain),
% 	HorariosValen = HorariosDodain.
% atiende(valen, HorariosValen):-
% 	atiende(juanC, HorariosJuanC),
% 	HorariosValen = HorariosJuanC.

% para la alternitiva con hecho por hecho VERSION 2.
atiende(valen, Dia, HorarioInicio, HorarioFinalizacion):-
	atiende(dodain, Dia, HorarioInicio, HorarioFinalizacion).

atiende(valen, Dia, HorarioInicio, HorarioFinalizacion):-
	atiende(juanC, Dia, HorarioInicio, HorarioFinalizacion).
	
% nadie hace el mismo horario que leoC
% rta que dijo el profe: por principio de universo cerrado no agregamos al base de conocimiento aquiello que no tiene sentido agregar

% maiu está pensando si hace el horario de 0 a 8 los martes y miércoles
% rta que dijo el profe: lo desconocido se presume como falso, entonces por principio de universo cerrado no agregamos al base de conocimiento aquiello que no tiene sentido agregar

% -> No se modela, ya que no es un afirmación concreta, por ende no puede ser un hecho (o clausula).
% ya que por principio de universo cerrado, lo que no se encuentra en la base de conocimiento
% se lo considera falso, y no existe un "intermedio" entre verdadero y falso (como puede ser
% este caso de maiu que está en duda).


%%%% PUNTO 2: quién atiende el kiosko... (2 puntos) %%%%

quienAtiende(Persona, Dia, Horario):- 
	% NO HACEN FALTA... ver clase
	% atiende(Persona, _ ,_, _), % se liga la variable persona
	% atiende(Persona, Dia ,_, _), % se liga la variable dia
	atiende(Persona, Dia, HorarioInicio, HorarioFinalizacion), % se liga la variable HorarioInicio y HorarioFinalizacion
	between(HorarioInicio, HorarioFinalizacion, Horario).
	

%%%% PUNTO 3: Forever alone (2 puntos) %%%%

esForeverAlone(Persona, Dia, Horario):-
	quienAtiende(Persona, Dia, Horario),
	not((quienAtiende(OtraPersona, Dia, Horario), Persona \= OtraPersona)).

%%%% PUNTO 4: posibilidades de atención (3 puntos / 1 punto) %%%%

% el profe lo hizo un poquito distinto en 

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

%RTA DEL PROFE: (explosion combinatoria es un termino casero, el formal seria mecanismo de backtracking de prolog)
% Qué conceptos en conjunto resuelven este requerimiento 
% - findall como herramienta para poder generar un conjunto de soluciones que satisfacen un predicado 
% - mecanismo de backtracking de Prolog permite encontrar todas las satuciones posibles 

% Rta que se me ocurrio:
% Dicho conecepto consiste en hacer uso de recursividad a la par del motor de prolog
% el cual se caracteriza por encontrar los predicados que se satisfacen siendo estos no deterministicos.

% PUNTO 5: ventas / suertudas (4 puntos)

% venta(dodain, lunes(10), golosinas(1200)).
% venta(dodain, lunes(10), cigarrillos([jockey])).
% venta(dodain, lunes(10), golosinas(50)).
venta(dodain, lunes(10), [golosinas(1200), cigarrillos([jockey]), golosinas(50)]).

% venta(dodain, miercoles(12), bebidas(alcoholicas, 8)).
% venta(dodain, miercoles(12), bebidas(no_alcoholicas, 1)).
% venta(dodain, miercoles(12), golosinas(10)).
venta(dodain, miercoles(12), [bebidas(alcoholicas, 8), bebidas(no_alcoholicas, 1), golosinas(10)]).

% venta(martu, miercoles(12), golosinas(1000)).
% venta(martu, miercoles(12), cigarrillos([jockey, chesterfield, colorado, parisiennes])).
venta(martu, miercoles(12), [golosinas(1000), cigarrillos([jockey, chesterfield, colorado, parisiennes])]).

% venta(lucas, martes(11), golosinas(600)).
venta(lucas, martes(11), [golosinas(600)]).

% venta(lucas, martes(18), bebidas(no_alcoholicas, 2)).
% venta(lucas, martes(18), cigarrillos([derby])).
venta(lucas, martes(18), [bebidas(no_alcoholicas, 2), cigarrillos([derby])]).




% esSuertuda(Persona):-
% 	venta(Persona, _ , _),
% 	% venta(Persona, Dia, _),
% 	% venta(Persona, Dia, Ventas), NO VA YA QUE HARIA QUE AL FORALL LE LLEGUEN TODAS LAS VAIABLES UNIFICADAS, LO QUE NO TENDRIA SENTIDO.
% 	% la fecha no importa
% 	forall(venta(Persona, _, Ventas), primeraVentaImportante(Ventas)).

% primeraVentaImportante(Ventas):-
% 	nth0(0, Ventas, PrimerProductoVendido),
% 	productoImportante(PrimerProductoVendido).

% alternativa sin hacer el primeraVentaImportante
esSuertuda(Persona):-
	venta(Persona, _ , _),
	forall(venta(Persona, _, [Venta|_]), productoImportante(Venta)).

productoImportante(golosinas(Monto)):- Monto > 100.
productoImportante(cigarrillos(Marcas)):- length(Marcas, Largo), Largo > 2.
productoImportante(bebidas(alcoholicas,_)).
productoImportante(bebidas(no_alcoholicas, Cantidad)):- Cantidad > 5.


	







:- begin_tests(kioskito).

	test(fakeTest):-
		true.

:- end_tests(kioskito).