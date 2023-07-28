data Persona = Persona {nombre :: String, satisfaccion :: Int, edad :: Int, cantidadFilmaciones :: Int, plata :: Int}
data Filmacion = Filmacion {titulo :: String, puntaje :: Int, anio:: Int, duracion :: Int, listaActores :: [String], genero :: (Filmacion -> Persona -> Persona)}

pinta_buena :: Filmacion -> Bool
pinta_buena filmacion = length (listaActores filmacion) >= 5

precio_base :: Filmacion -> Int
precio_base filmacion
  | pinta_buena filmacion = 200
  | anio filmacion < 1990 = (*2) . length . titulo $ filmacion
  | otherwise = 100 + (puntaje filmacion * 3)

precio_extra :: Filmacion -> Int
precio_extra filmacion
  | duracion filmacion > 115 = min (10 * (duracion filmacion - 115)) 100
  | anio filmacion >= 1990 = 50
  | otherwise = 0

precio_total :: Filmacion -> Int
precio_total filmacion =
  let base = precioBase filmacion
      extra = precioExtra filmacion
      total = base + extra
  in
    if total > 200
      then total - (total `div` 10)
      else total


terror :: Int -> Filmacion -> Persona -> Persona
terror sangre filmacion persona = Persona{satisfaccion = satisfaccion persona - sangre, cantidadFilmaciones = cantidadFilmaciones persona + 1}

comedia :: Filmacion -> Persona -> Persona
comedia filmacion persona = Persona{satisfaccion = satisfaccion persona * 2, cantidadFilmaciones = cantidadFilmaciones persona + 1}

drama :: Int -> Filmacion -> Persona -> Persona
drama escena_felices filmacion persona  | escena_felices <= 3 = Persona{satisfaccion = satisfaccion persona + escena_felices, edad = edad persona + 1, cantidadFilmaciones = cantidadFilmaciones persona + 1}
                                        | otherwise = Persona{satisfaccion = satisfaccion persona + 3, edad = edad persona + 1, cantidadFilmaciones = cantidadFilmaciones persona + 1}

accion :: Filmacion -> Persona -> Persona
accion filmacion persona  | pinta_buena filmacion = Persona{satisfaccion = satisfaccion persona + 100, cantidadFilmaciones = cantidadFilmaciones persona + 1}
                          | otherwise = Persona{cantidadFilmaciones = cantidadFilmaciones persona + 1}

tragicomico :: Int -> Filmacion -> Persona -> Persona
tragicomico importe filmacion persona = Persona{plata = plata persona - (importe * 2), cantidadFilmaciones = cantidadFilmaciones persona + 2}

aventura :: String -> Filmacion -> Persona -> Persona
aventura numero_de_pelicula filmacion persona   | numero_de_pelicula == "IV" = Persona{cantidadFilmaciones = cantidadFilmaciones persona + 1 }
                                                | otherwise = comedia filmacion persona

arma_mortal = Filmacion "Arma Mortal" 7 1987 109 ["Mel Gibson", "Danny Glover", "Gary Busey"] accion
nueve_reinas = Filmacion "9 Reinas" 8 2000 114 ["Gastón Pauls", "Ricardo Darín", "Leticia Bredice", "Pochi Ducasse"] (drama 5)
la_odisea_de_los_giles = Filmacion "La odisea de los giles" 8 2019 116 ["Ricardo Darín", "Luis Brandoni", "Verónica Llinás", "Daniel Aráoz", "Rita Cortese"] comedia
la_flor = Filmacion "La flor" 7 2018 840 ["Pilar Gamboa"] (tragicomico 10)
indiana_jones_4 = Filmacion "Indiana Jones IV" 6 2007 125 ["Harrison Ford"] (aventura "IV")
indiana_jones_1 = Filmacion "Indiana Jones I" 8 1981 115 ["Harrison Ford"] (aventura "IV")
el_secreto_de_sus_ojos = Filmacion "El secreto de sus ojos" 9 2009 129 ["Ricardo Darín", "Soledad Villamil"] (drama 3)
moni = Persona "Moni" 50 31 0 5600
coky = Persona "Coky" 120 20 40 50

verFilmacion :: Persona -> Filmacion -> Persona
verFilmacion persona filmacion = (genero filmacion) filmacion persona

verFilmaciones :: Persona -> [Filmacion] -> Persona
verFilmaciones persona filmaciones = foldl verFilmacion persona filmaciones

tieneSatisfaccion :: (a -> a -> Bool) -> Persona -> Persona -> Persona
tieneSatisfaccion comparador persona1 persona2 = comparador (satisfaccion persona1) (satisfaccion persona2)

tieneMasSatisfaccion :: [Persona] -> Persona
tieneMasSatisfaccion listaPersonas = foldl1 (tieneSatisfaccion (max)) listaPersonas

neverPony :: [Persona] -> Filmacion -> Bool
neverPony listaPersonas filmacion   | satisfaccion (verFilmacion (tieneMasSatisfaccion listaPersonas) filmacion) > 100 = True
                                    | otherwise = False

verFilmacionSiPuede :: Persona -> Filmacion -> Persona
verFilmacionSiPuede persona filmacion   | plata persona >= precio_total filmacion = verFilmacion persona filmacion
                                        | otherwise = persona

hastaDondeDeLaBilletera :: Persona -> [Filmacion] -> Persona
hastaDondeDeLaBilletera persona filmaciones = foldl (verFilmacionSiPuede) persona filmaciones

meQuedoConLosPrimeros :: (a -> Bool) -> [a] -> [a]
meQuedoConLosPrimeros _ [] = []
meQuedoConLosPrimeros criterio (x:xs)   | criterio x = x : meQuedoConLosPrimeros criterio xs
                                        | otherwise = []

laPulenta :: Int -> Persona -> Filmacion -> Bool
laPulenta nivel_esperado persona filmacion = satisfaccion (verFilmacion persona filmacion) >= nivel_esperado

contingenteInfinito :: [Filmacion]
contingenteInfinito = cycle [nueve_reinas, la_odisea_de_los_giles]

--Si se ejecuta con la busqueda de las primeras dos darinescas el programa se detendra y devolvera la odisea de los giles dos veces
--y en caso de combo vendible devolveria siempre false en caso de que combo vendible tenga un stop para cuando una no se puede comprar una de las peliculas