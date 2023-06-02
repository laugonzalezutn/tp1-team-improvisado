daniresca :: Filmacion -> Bool
daniresca filmacion = case actores filmacion of
  (primerActor:_) -> primerActor == "Ricardo Darín"
  _ -> False

pinta_buena :: Filmacion -> Bool
pinta_buena filmacion = length (actores filmacion) >= 5

minutos_excedentes :: Filmacion -> Int
minutos_excedentes filmacion = abs (duracion filmacion - 115)

precio_base :: Filmacion -> Int
precio_base filmacion
  | pinta_buena filmacion = 200
  | anio filmacion < 1990 = (*2) . length . titulo $ filmacion
  | otherwise = (+) (100 + puntaje filmacion * 3)

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

actualizar_persona :: Genero -> Int -> Pelicula -> Persona -> Persona
actualizar_persona genero escenas_felices pelicula persona
  | genero == Terror = persona { nivel_de_satisfaccion = max 0 (nivel_de_satisfaccion persona - litrosDeSangre pelicula) }
  | genero == Comedia = persona { nivel_de_satisfaccion = nivel_de_satisfaccion persona * 2, nombre = nombre persona ++ " muy alegre" }
  | genero == Drama =
      let nuevas_escenas = min 3 escenas_felices
          nuevaEdad = edad persona + 1
          incrementoSatisfaccion = min 3 nuevas_escenas
          nuevaSatisfaccion = nivel_de_satisfaccion persona + incrementoSatisfaccion
      in
        persona { edad = nuevaEdad, nivel_de_satisfaccion = nuevaSatisfaccion }
  | genero == Accion ->
      if pintaBuena pelicula
        then persona { nivel_de_satisfaccion = nivel_de_satisfaccion persona + 100 }
        else persona
  | genero == Tragicomico ->
      let personaComedia = actualizarPersona Comedia 0 pelicula persona
          personaDrama = actualizarPersona Drama escenas_felices pelicula personaComedia
      in
        personaDrama { cantidad_de_films = cantidad_de_films personaDrama + 2 }
  | genero == Aventura ->
      if esVersionMala pelicula
        then persona
        else actualizarPersona Comedia 0 pelicula persona
  | otherwise = persona
