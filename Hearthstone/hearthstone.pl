% Nicolas Daniel Occhi

% SON FUNCTORES
% jugadores
	% jugador(Nombre, PuntosVida, PuntosMana, CartasMazo, CartasMano, CartasCampo).
% cartas
	% criatura(Nombre, PuntosDanio, PuntosVida, CostoMana).
	% hechizo(Nombre, FunctorEfecto, CostoMana).
% efectos
	% danio(CantidadDanio).
	% cura(CantidadCura).

jugador(nico, 100, 100, [criatura(rugbier, 20, 30, 4), hechizo(rayito, danio(50), 10)], [criatura(fusilero, 10, 10, 2), hechizo(curacion, cura(20), 15)], []).

% Se cuenta con los siguientes predicados auxiliares1:
nombre(jugador(Nombre,_,_,_,_,_), Nombre).
nombre(criatura(Nombre,_,_,_), Nombre).
nombre(hechizo(Nombre,_,_), Nombre).
vida(jugador(_,Vida,_,_,_,_), Vida).
vida(criatura(_,_,Vida,_), Vida).
vida(hechizo(_,curar(Vida),_), Vida).
danio(criatura(_,Danio,_), Danio).
danio(hechizo(_,danio(Danio),_), Danio).
mana(jugador(_,_,Mana,_,_,_), Mana).
mana(criatura(_,_,_,Mana), Mana).
mana(hechizo(_,_,Mana), Mana).
cartasMazo(jugador(_,_,_,Cartas,_,_), Cartas).
cartasMano(jugador(_,_,_,_,Cartas,_), Cartas).
cartasCampo(jugador(_,_,_,_,_,Cartas), Cartas).
% Puede que no se necesite usar todos, de acuerdo a cómo se plantee la solución

%% 1)
tieneCarta(Jugador, Carta):-
	cartasMazo(Jugador, Cartas),
	member(Carta, Cartas).
tieneCarta(Jugador, Carta):-
	cartasMano(Jugador, Cartas),
	member(Carta, Cartas).
tieneCarta(Jugador, Carta):-
	cartasCampo(Jugador, Cartas),
	member(Carta, Cartas).

%% 2)
cartaCriatura(criatura(_,_,_,_)).

esGuerrero(Jugador):-
	forall(tieneCarta(Jugador, Carta), cartaCriatura(Carta)).


%% 3)
empiezaTurno(Jugador, JugadorPostTurno):-
	tieneCarta(Jugador,_), % verificamos que tenga carta
	tieneCarta(JugadorPostTurno,_), % verificamos que tenga carta
	modificarMana(Jugador, JugadorPostTurno, 1), % porque le sumamos 1 a la mana del jugador
	cartaDeMazoAMano(Jugador, JugadorPostTurno).

modificarMana(Jugador, JugadorPostTurno, Cantidad):- 
	mana(Jugador, Mana),
	mana(JugadorPostTurno, ManaPostTurno),
	ManaPostTurno is Mana + Cantidad.

cartaDeMazoAMano(Jugador, JugadorPostTurno):-
	cartasMazo(Jugador, [PrimeraCartaMazo|_]),
	cartasMano(JugadorPostTurno, CartasMano),
	member(PrimeraCartaMazo, CartasMano). % pienso que este predicado NO agrega la carta a la mano pero bueno, agregar cosas a listas creo que no vimos 


%% 4)
% a)
cartaJugable(Jugador, CartaAJugar):-
	cartasMano(Jugador, CartasEnMano),
	member(CartaAJugar, CartasEnMano). % puede jugar la carta si la tiene en la mano y no en el mazo.

puedeJugarCarta(Jugador, Carta):-
	cartaJugable(Jugador, Carta),
	mana(Carta, CostoManaCarta),
	mana(Jugador, ManaJugador),
	ManaJugador >= CostoManaCarta.

% b)
jugador(Jugador):- nombre(Jugador, _).
cartasQuePuedeJugar(Jugador, CartasJugables):-
	empiezaTurno(Jugador, JugadorPostTurno),
	cartasMano(JugadorPostTurno, Cartas), % si el jugador tiene la carta en la mano (porque si la tiene en el mazo no la puede jugar)
	findall(Carta, (cartasMano(JugadorPostTurno, Cartas), puedeJugarCarta(JugadorPostTurno, Carta)), CartasJugables).


%% 5)

jugadasProximoTurno(Jugador, Jugadas):-
	jugador(Jugador),
	mana(Jugador, ManaJugador),
	findall(Carta, (cartasMano(Jugador, Carta), puedeJugarCarta(Jugador, Carta)), CartasJugables),
	jugadasPosibles(CartasJugables, ManaJugador, Jugadas).

jugadasPosibles([], _, []).

jugadasPosibles([Carta|Cartas], ManaJugador, [Carta|Posibles]):-
	mana(Carta, CostoManaCarta), 
	ManaJugador > CostoManaCarta, 
	ManaRestanteJugador is ManaJugador - CostoManaCarta,
	jugadasPosibles(Cartas, ManaRestanteJugador, Posibles).

jugadasPosibles([_|Cartas], ManaJugador, Posibles):-
	jugadasPosibles(Cartas, ManaJugador, Posibles).


%% 6)

cartaMasAlta(Jugador, NombreCartaMasAlta):-
	jugador(Jugador),
	forall(tieneCarta(Jugador, Carta), tieneMasDanio(Jugador, Carta)),
	nombre(Carta, NombreCartaMasAlta).

% MAL HECHO ESTO
tieneMasDanio(Jugador, Carta):-
	danio(Carta, Danio),
	tieneCarta(Jugador, OtraCarta),
	not((danio(OtraCarta, DanioOtraCarta), DanioOtraCarta > Danio)).
	% no existe otraCarta que tenga mas danio que una carta

%% 7)
% a)
jugarContra(Contrincante, Carta, ContrincantePostCarta):-
	danio(Carta, Danio),
	modificarVida(Contrincante, ContrincantePostCarta, -Danio). % el danio resta vida del jugador.

modificarVida(Jugador, JugadorPostCarta, Cantidad):-
	vida(Jugador, Vida),
	vida(JugadorPostCarta, VidaModificada),
	VidaModificada is Vida + Cantidad.

% b)
%caso en que la carta NO sea un hechizo curativo
jugar(Jugador, Carta, JugadorPostCarta):-
	not(esHechizoDeCura(Carta)),
	puedeJugarCarta(Jugador, Carta).

%caso en el la carta sea un hechizo curativo
jugar(Jugador, Carta, JugadorPostCarta):-
	puedeJugarCarta(Jugador, Carta),
	esHechizoDeCura(Carta),
	vida(hechizo(_,curar(Curacion),_), Curacion),
	modificarVida(Jugador, Curacion, JugadorPostCarta).

esHechizoDeCura(hechizo(_,curar(_),_)).

:- begin_tests(hearthstone).

	test(fakeTest):-
		true.

:- end_tests(hearthstone).