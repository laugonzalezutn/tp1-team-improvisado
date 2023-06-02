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