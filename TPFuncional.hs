{-# OPTIONS_GHC -Wno-missing-fields #-}
data Filmacion = Filmacion {titulo :: String, puntaje :: Int, anio:: Int, duracion :: Int, listaActores :: [String]}

arma_mortal = Filmacion "Arma Mortal" 7 1987 109 ["Mel Gibson", "Danny Glover", "Gary Busey"]
nueve_reinas = Filmacion "9 Reinas" 8 2000 114 ["Gastón Pauls", "Ricardo Darín", "Leticia Bredice", "Pochi Ducasse"]
la_odisea_de_los_giles = Filmacion "La odisea de los giles" 8 2019 116 ["Ricardo Darín", "Luis Brandoni", "Verónica Llinás", "Daniel Aráoz", "Rita Cortese"]
la_flor = Filmacion "La flor" 7 2018 840 ["Pilar Gamboa"]
indiana_jones_4 = Filmacion "Indiana Jones IV" 6 2007 125 ["Harrison Ford"]
indiana_jones_1 = Filmacion "Indiana Jones I" 8 1981 115 ["Harrison Ford"]

tiene_a_darin :: [String] -> Bool
tiene_a_darin (primer_actor:_) | primer_actor == "Ricardo Darín" = True
                               | otherwise = False

darinesca :: Filmacion -> Bool
darinesca filmacion = tiene_a_darin (listaActores filmacion)

pinta_buena :: Filmacion -> Bool
pinta_buena filmacion = length (listaActores filmacion) >= 5

minutos_excedentes :: Filmacion -> Int
minutos_excedentes filmacion = abs (duracion filmacion - 115)

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

data Persona = Persona {nombre :: String, satisfaccion :: Int, edad :: Int, cantidadFilmaciones :: Int, plata :: Int}

terror :: Int -> Filmacion -> Persona -> Persona
terror sangre filmacion persona = Persona{satisfaccion = satisfaccion persona - sangre}

comedia :: Filmacion -> Persona -> Persona
comedia filmacion persona = Persona{satisfaccion = satisfaccion persona * 2}

drama :: Int -> Filmacion -> Persona -> Persona
drama escena_felices filmacion persona  | escena_felices <= 3 = Persona{satisfaccion = satisfaccion persona + escena_felices, edad = edad persona + 1}
                                        | otherwise = Persona{satisfaccion = satisfaccion persona + 3, edad = edad persona + 1}

accion :: Filmacion -> Persona -> Persona
accion filmacion persona  | pinta_buena filmacion = Persona{satisfaccion = satisfaccion persona + 100}
                          | otherwise = persona

tragicomico :: Int -> Filmacion -> Persona -> Persona
tragicomico importe filmacion persona = Persona{plata = plata persona - (importe * 2), cantidadFilmaciones = cantidadFilmaciones persona + 2}