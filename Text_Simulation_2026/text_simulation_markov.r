# Databricks notebook source
# MAGIC %md
# MAGIC # Simulacion de texto con cadenas de Markov (bigramas)
# MAGIC ### Ejemplo didactico - R base (sin Rcpp, sin paralelizacion)
# MAGIC
# MAGIC **Objetivo:** mostrar, con un ejemplo minimo, la idea que esta "debajo" de los
# MAGIC modelos de lenguaje: predecir/samplear la siguiente palabra dado el contexto
# MAGIC actual. Aqui el contexto es **una sola palabra** (bigrama) y el "modelo" son
# MAGIC solo frecuencias contadas en un texto real -- el equivalente mas simple
# MAGIC posible de lo que hace un LLM moderno (que usa una red neuronal tipo
# MAGIC Transformer en vez de conteos).
# MAGIC
# MAGIC **Relacion con el curso** (Estadística Computacional):
# MAGIC - Alla se usaba una **matriz de transicion + Rcpp** (`mc_transition`/`mc_trajectory`)
# MAGIC   para simular una cadena de Markov, y computo paralelo (`foreach %dopar%`) para
# MAGIC   construir la matriz de bigramas de un libro completo.
# MAGIC - Aqui usamos **solo R base**: nada de Rcpp, nada de paralelizacion. En vez de
# MAGIC   una matriz V x V (enorme y llena de ceros), guardamos los bigramas como dos
# MAGIC   vectores alineados (`palabra_actual`, `palabra_siguiente`) y "muestreamos"
# MAGIC   filtrando y usando `sample()` -- la repeticion natural de los pares ya
# MAGIC   funciona como el peso/probabilidad, no hace falta `cumsum` ni tablas de
# MAGIC   frecuencia explicitas.
# MAGIC
# MAGIC **Estructura del notebook:**
# MAGIC 1. Se define cada funcion (Pasos 1-5) y, justo despues, se **corre de
# MAGIC    inmediato con datos reales del Ejemplo 1 (Jane Austen)**, imprimiendo en
# MAGIC    pantalla lo que va pasando en cada paso -- nada se queda "para el final".
# MAGIC 2. El Paso 4 y el Paso 5 incluyen ademas una celda **manual**: tu escribes
# MAGIC    una palabra, corres la celda, y ves que palabra elige el modelo como
# MAGIC    siguiente -- asi puedes "caminar" la cadena a mano, un salto a la vez.
# MAGIC 3. Una vez completo el Ejemplo 1 de principio a fin, se repiten los mismos
# MAGIC    5 pasos (celdas duplicadas a proposito) para el **Ejemplo 2** (un libro
# MAGIC    de mecanica), reutilizando las mismas funciones pero con datos nuevos.
# MAGIC 4. Al final se comparan los dos textos generados y se sacan conclusiones.

# COMMAND ----------

# MAGIC %md
# MAGIC ## Paso 1: Descargar un libro de Project Gutenberg
# MAGIC
# MAGIC Project Gutenberg publica cada libro en texto plano en una URL estandar:
# MAGIC
# MAGIC `https://www.gutenberg.org/cache/epub/<id>/pg<id>.txt`
# MAGIC
# MAGIC Usamos `download.file()` + `readLines()` (solo R base) para bajarlo y
# MAGIC pegarlo en un solo string largo.

# COMMAND ----------

descargar_libro_gutenberg <- function(id_gutenberg, titulo) {
  # id_gutenberg: numero de eBook en Project Gutenberg (se ve en la URL de la ficha del libro)
  # titulo: solo para los mensajes de progreso (cat/print), no afecta la descarga
  url <- sprintf("https://www.gutenberg.org/cache/epub/%d/pg%d.txt", id_gutenberg, id_gutenberg)
  archivo_temporal <- tempfile(fileext = ".txt")

  descarga_ok <- tryCatch({
    download.file(url, destfile = archivo_temporal, quiet = TRUE, mode = "wb")
    TRUE
  }, error = function(e) FALSE)

  if (!descarga_ok) {
    stop(sprintf("No se pudo descargar '%s' desde %s (revisa tu conexion a internet)", titulo, url))
  }

  lineas <- readLines(archivo_temporal, warn = FALSE, encoding = "UTF-8")
  texto <- paste(lineas, collapse = " ")

  cat(sprintf(
    "Descargado '%s' (Gutenberg #%d): %s caracteres\n",
    titulo, id_gutenberg, format(nchar(texto), big.mark = ",")
  ))

  return(texto)
}

# COMMAND ----------

# MAGIC %md
# MAGIC ### Ejemplo 1 - Paso 1 en accion: descargamos las dos novelas de Austen
# MAGIC
# MAGIC Llamamos la funcion con los dos libros que usaremos como corpus de
# MAGIC entrenamiento del Ejemplo 1, y le echamos un ojo al texto crudo tal como
# MAGIC viene de Gutenberg (todavia con encabezado y sin limpiar).

# COMMAND ----------

cat("\n================ EJEMPLO 1: DESCARGA DEL CORPUS DE JANE AUSTEN ================\n")

texto_pyp_crudo <- descargar_libro_gutenberg(1342, "Pride and Prejudice")
texto_syn_crudo <- descargar_libro_gutenberg(21839, "Sense and Sensibility")

cat("\nAsi se ve el texto crudo de 'Pride and Prejudice' (primeros 300 caracteres):\n")
cat(substr(texto_pyp_crudo, 1, 300), "\n")

# COMMAND ----------

# MAGIC %md
# MAGIC ## Paso 2: Limpiar el texto crudo y convertirlo en un vector de palabras
# MAGIC
# MAGIC Los archivos de Gutenberg traen un encabezado y un pie legal (licencia,
# MAGIC informacion de la fundacion, etc.) que **no** son parte del libro. Los
# MAGIC quitamos usando los marcadores estandar `*** START OF` y `*** END OF`.
# MAGIC Despues, igual que en `transmat_from_text.R` (clase 7), pasamos todo a
# MAGIC minusculas y quitamos puntuacion/numeros para quedarnos solo con palabras.

# COMMAND ----------

limpiar_texto_gutenberg <- function(texto_crudo) {
  # Cortamos el encabezado/pie legal de Gutenberg si los marcadores existen
  inicio <- regexpr("\\*\\*\\* START OF[^*]*\\*\\*\\*", texto_crudo)
  fin <- regexpr("\\*\\*\\* END OF[^*]*\\*\\*\\*", texto_crudo)
  if (inicio[1] > 0) {
    texto_crudo <- substring(texto_crudo, inicio + attr(inicio, "match.length"))
  }
  if (fin[1] > 0) {
    texto_crudo <- substring(texto_crudo, 1, fin - 1)
  }

  # minusculas + quitar saltos de linea, puntuacion y digitos
  texto <- tolower(texto_crudo)
  texto <- gsub("[\r\n]", " ", texto)
  texto <- gsub("[[:punct:]]", " ", texto)
  texto <- gsub("[[:digit:]]", " ", texto)

  # separar en palabras individuales usando cualquier espacio en blanco
  palabras <- unlist(strsplit(texto, "\\s+"))
  palabras <- palabras[palabras != ""]

  return(palabras)
}

# COMMAND ----------

# MAGIC %md
# MAGIC ### Ejemplo 1 - Paso 2 en accion: limpiamos las dos novelas
# MAGIC
# MAGIC Limpiamos cada libro por separado y despues los juntamos en un solo
# MAGIC vector de palabras: ese vector combinado es nuestro corpus de
# MAGIC entrenamiento para el Ejemplo 1.

# COMMAND ----------

palabras_pyp <- limpiar_texto_gutenberg(texto_pyp_crudo)
palabras_syn <- limpiar_texto_gutenberg(texto_syn_crudo)

# combinamos ambas novelas en un solo vector de palabras de entrenamiento
palabras_austen <- c(palabras_pyp, palabras_syn)

cat("Palabras en 'Pride and Prejudice':", length(palabras_pyp), "\n")
cat("Palabras en 'Sense and Sensibility':", length(palabras_syn), "\n")
cat("Total de palabras en el corpus de Austen:", length(palabras_austen), "\n")
cat("Vocabulario (palabras distintas):", length(unique(palabras_austen)), "\n")

cat("\nAsi se ven las primeras 20 palabras ya limpias:\n")
print(head(palabras_austen, 20))

# COMMAND ----------

# MAGIC %md
# MAGIC ## Paso 3: Construir los bigramas (la "cadena de Markov" de palabras)
# MAGIC
# MAGIC Un bigrama es el par `(palabra_actual, palabra_siguiente)`. Guardamos
# MAGIC **todos** los pares observados en el texto como dos vectores alineados por
# MAGIC posicion. Esto es, conceptualmente, la matriz de transicion de la cadena de
# MAGIC Markov: dado un "estado" (`palabra_actual`), los posibles "siguientes
# MAGIC estados" son las palabras que aparecen en `bigramas$siguiente` en esas
# MAGIC mismas posiciones. No construimos una matriz V x V explicita (seria enorme
# MAGIC y casi toda ceros); la propia repeticion de pares en el vector ya actua
# MAGIC como el peso/frecuencia.

# COMMAND ----------

construir_bigramas <- function(palabras) {
  n <- length(palabras)
  bigramas <- data.frame(
    actual = palabras[-n],     # todas las palabras menos la ultima
    siguiente = palabras[-1],  # todas las palabras menos la primera
    stringsAsFactors = FALSE
  )
  return(bigramas)
}

# COMMAND ----------

# MAGIC %md
# MAGIC ##  ¿Qué hace `construir_bigramas()`?
# MAGIC
# MAGIC Esta función toma una lista de palabras y arma **todas las parejas de palabras consecutivas** (bigramas): para cada palabra, cuál fue la palabra que la siguió en el texto original.
# MAGIC
# MAGIC ### La idea, en un ejemplo chiquito
# MAGIC
# MAGIC Supongamos que `palabras` es esto:
# MAGIC
# MAGIC ```
# MAGIC palabras <- c("el", "gato", "come", "pescado")
# MAGIC ```
# MAGIC
# MAGIC La función hace un truco muy simple: **recorre el mismo vector dos veces, desfasado por una posición**.
# MAGIC
# MAGIC | Índice | 1 | 2 | 3 | 4 |
# MAGIC |---|---|---|---|---|
# MAGIC | `palabras`            | el    | gato  | come  | pescado |
# MAGIC | `palabras[-n]`  (actual)     | el    | gato  | come  | ~~pescado~~ |
# MAGIC | `palabras[-1]`  (siguiente)  | ~~el~~ | gato  | come  | pescado |
# MAGIC
# MAGIC
# MAGIC - `palabras[-n]` = **todas menos la última** → columna `actual`
# MAGIC - `palabras[-1]` = **todas menos la primera** → columna `siguiente`
# MAGIC
# MAGIC Como ambos vectores quedan **alineados por posición**, al pegarlos lado a lado obtenemos automáticamente los pares consecutivos:
# MAGIC
# MAGIC ### Resultado: la tabla de bigramas
# MAGIC
# MAGIC | actual | siguiente |
# MAGIC |--------|-----------|
# MAGIC | el     | gato      |
# MAGIC | gato   | come      |
# MAGIC | come   | pescado   |
# MAGIC
# MAGIC ### ¿Por qué importa esto?
# MAGIC
# MAGIC Cada fila es una "transición" observada: **"el" fue seguido por "gato"**, **"gato" fue seguido por "come"**, etc. Esta tabla es, en esencia, la matriz de transición de nuestra cadena de Markov de palabras — solo que en vez de guardarla como una matriz enorme llena de ceros, la guardamos como una lista de pares observados. Si una pareja de palabras se repite muchas veces en el texto (por ejemplo `"the" -> "same"` aparece 500 veces en un libro), esa fila también se repite 500 veces en la tabla — y esa repetición es justo lo que después usamos como "peso" o probabilidad al muestrear la siguiente palabra.

# COMMAND ----------

# MAGIC %md
# MAGIC ### Ejemplo 1 - Paso 3 en accion: armamos los bigramas de Austen

# COMMAND ----------

bigramas_austen <- construir_bigramas(palabras_austen)

cat("Total de bigramas construidos:", nrow(bigramas_austen), "\n")
cat("\nAsi se ven los primeros bigramas (palabra_actual -> palabra_siguiente):\n")
print(head(bigramas_austen, 10))

# COMMAND ----------

# MAGIC %md
# MAGIC ## Paso 4: Muestrear la siguiente palabra dado el estado actual
# MAGIC
# MAGIC Version en R base de lo que en el curso hacia `sampleC()`/`mc_transition()`
# MAGIC en Rcpp: dado el estado actual, miramos todas las veces que en el texto de
# MAGIC entrenamiento esa palabra fue seguida de otra, y elegimos una al azar entre
# MAGIC esas opciones. Como una palabra frecuente aparece varias veces en el vector
# MAGIC de candidatos, `sample()` ya la esta ponderando correctamente sin necesidad
# MAGIC de calcular probabilidades explicitas.

# COMMAND ----------

palabra_actual <- "back"  
bigramas_austen$siguiente[bigramas_austen$actual == palabra_actual]

# COMMAND ----------

siguiente_palabra <- function(palabra_actual, bigramas) {
  candidatos <- bigramas$siguiente[bigramas$actual == palabra_actual]

  if (length(candidatos) == 0) {
    # si la palabra actual nunca aparecio como "actual" en el entrenamiento,
    # reiniciamos la cadena tomando una palabra cualquiera del texto
    candidatos <- bigramas$actual
  }

  return(sample(candidatos, size = 1))
}

# COMMAND ----------

# MAGIC %md
# MAGIC ### Ejemplo 1 - Paso 4 en modo manual: prueba tu misma que palabra sigue
# MAGIC
# MAGIC Cambia el valor de `palabra_actual` por la palabra que quieras probar,
# MAGIC corre la celda y observa la palabra que el modelo elige como
# MAGIC siguiente. Despues, copia esa palabra sugerida dentro de `palabra_actual`
# MAGIC y vuelve a correr la celda para dar otro paso -- asi puedes "caminar" la
# MAGIC cadena de Markov a mano, un salto a la vez.

# COMMAND ----------

palabra_actual <- "back"  # <-- cambia esta palabra por la que quieras probar
palabra_siguiente <- siguiente_palabra(palabra_actual, bigramas_austen)

cat("Si la palabra actual es '", palabra_actual, "', el modelo sugiere seguir con: '", palabra_siguiente, "'\n", sep = "")

# COMMAND ----------

# MAGIC %md
# MAGIC ## Paso 5: Generar texto encadenando muestreos (la "trayectoria" de la cadena)
# MAGIC
# MAGIC Equivalente en espiritu a `mc_trajectory()` del curso: partimos de una
# MAGIC palabra semilla y vamos saltando de palabra en palabra `n_palabras - 1`
# MAGIC veces, guardando cada salto.

# COMMAND ----------

generar_texto <- function(semilla, bigramas, n_palabras = 50) {
  texto_generado <- semilla
  palabra_actual <- semilla

  for (i in 1:(n_palabras - 1)) {
    palabra_actual <- siguiente_palabra(palabra_actual, bigramas)
    texto_generado <- c(texto_generado, palabra_actual)
  }

  return(paste(texto_generado, collapse = " "))
}

# COMMAND ----------

# MAGIC %md
# MAGIC ### Ejemplo 1 - Paso 5 en accion, palabra por palabra
# MAGIC
# MAGIC Antes de usar la funcion `generar_texto()` de un jalon, veamos las
# MAGIC primeras 10 palabras generandose una por una, para que quede clarisimo
# MAGIC que la funcion no hace nada distinto a repetir `siguiente_palabra()` en
# MAGIC un loop.

# COMMAND ----------

set.seed(2026)
palabra_actual <- "close"
cat("Palabra 1:", palabra_actual, "\n")

for (i in 1:19) {
  palabra_actual <- siguiente_palabra(palabra_actual, bigramas_austen)
  cat(sprintf("Palabra %d: %s\n", i + 1, palabra_actual))
}

# COMMAND ----------

# MAGIC %md
# MAGIC ### Ejemplo 1 - Paso 5 completo: generamos 60 palabras de un jalon

# COMMAND ----------

#set.seed(2027)
texto_generado_austen <- generar_texto("elizabeth", bigramas_austen, n_palabras = 60)

cat("\n--- TEXTO GENERADO (estilo Austen, 60 palabras) ---\n")
cat(texto_generado_austen, "\n")

# COMMAND ----------

# MAGIC %md
# MAGIC # Ejemplo 2: Entrenar con un tema totalmente distinto (mecanica)
# MAGIC
# MAGIC Usamos **Practical Mechanics for Boys** de James Slough Zerbe: un libro
# MAGIC tecnico/instructivo, con vocabulario de herramientas, engranes, tornillos,
# MAGIC motores, etc. Repetimos exactamente los mismos 5 pasos de arriba
# MAGIC (reutilizando las mismas funciones, sin redefinir nada), solo que ahora
# MAGIC con un corpus de tema completamente distinto -- la idea es mostrar que el
# MAGIC mismo algoritmo, entrenado con datos distintos, produce texto con un
# MAGIC "sabor" completamente diferente: el modelo no "entiende" mecanica ni
# MAGIC literatura, solo repite patrones estadisticos del texto con el que fue
# MAGIC entrenado.

# COMMAND ----------

# MAGIC %md
# MAGIC ### Ejemplo 2 - Paso 1 en accion: descargamos el libro de mecanica

# COMMAND ----------

cat("\n================ EJEMPLO 2: DESCARGA DE 'PRACTICAL MECHANICS FOR BOYS' ================\n")

texto_mecanica_crudo <- descargar_libro_gutenberg(22298, "Practical Mechanics for Boys")

cat("\nAsi se ve el texto crudo (primeros 300 caracteres):\n")
cat(substr(texto_mecanica_crudo, 1, 300), "\n")

# COMMAND ----------

# MAGIC %md
# MAGIC ### Ejemplo 2 - Paso 2 en accion: limpiamos el libro de mecanica

# COMMAND ----------

palabras_mecanica <- limpiar_texto_gutenberg(texto_mecanica_crudo)

cat("Total de palabras en el corpus de mecanica:", length(palabras_mecanica), "\n")
cat("Vocabulario (palabras distintas):", length(unique(palabras_mecanica)), "\n")

cat("\nAsi se ven las primeras 20 palabras ya limpias:\n")
print(head(palabras_mecanica, 20))

# COMMAND ----------

# MAGIC %md
# MAGIC ### Ejemplo 2 - Paso 3 en accion: armamos los bigramas de mecanica

# COMMAND ----------

bigramas_mecanica <- construir_bigramas(palabras_mecanica)

cat("Total de bigramas construidos:", nrow(bigramas_mecanica), "\n")
cat("\nAsi se ven los primeros bigramas (palabra_actual -> palabra_siguiente):\n")
print(head(bigramas_mecanica, 10))

# COMMAND ----------

# MAGIC %md
# MAGIC ### Ejemplo 2 - Paso 4 en modo manual: prueba tu misma que palabra sigue
# MAGIC
# MAGIC Igual que en el Ejemplo 1: cambia `palabra_actual`, corre la celda, y
# MAGIC copia la palabra sugerida de vuelta en `palabra_actual` para dar otro
# MAGIC paso manualmente.

# COMMAND ----------

palabra_actual <- "machine"  # <-- cambia esta palabra por la que quieras probar
palabra_siguiente <- siguiente_palabra(palabra_actual, bigramas_mecanica)

cat("Si la palabra actual es '", palabra_actual, "', el modelo sugiere seguir con: '", palabra_siguiente, "'\n", sep = "")

# COMMAND ----------

# MAGIC %md
# MAGIC ### Ejemplo 2 - Paso 5 en accion, palabra por palabra

# COMMAND ----------

set.seed(2026)
palabra_actual <- "the"
cat("Palabra 1:", palabra_actual, "\n")

for (i in 1:9) {
  palabra_actual <- siguiente_palabra(palabra_actual, bigramas_mecanica)
  cat(sprintf("Palabra %d: %s\n", i + 1, palabra_actual))
}

# COMMAND ----------

# MAGIC %md
# MAGIC ### Ejemplo 2 - Paso 5 completo: generamos 60 palabras de un jalon

# COMMAND ----------

set.seed(2026)
texto_generado_mecanica <- generar_texto("the", bigramas_mecanica, n_palabras = 60)

cat("\n--- TEXTO GENERADO (estilo mecanica, 60 palabras) ---\n")
cat(texto_generado_mecanica, "\n")

# COMMAND ----------

# MAGIC %md
# MAGIC # Paso final: Comparacion
# MAGIC
# MAGIC Ponemos lado a lado los dos textos generados para ver como el mismo
# MAGIC algoritmo (cadena de Markov de bigramas) refleja el "estilo" de cada corpus
# MAGIC de entrenamiento sin necesidad de cambiar una sola linea de codigo: el
# MAGIC conocimiento vive enteramente en los datos (los bigramas), no en el
# MAGIC algoritmo.

# COMMAND ----------

cat("\n================ COMPARACION FINAL ================\n")
cat("\n[AUSTEN]\n", texto_generado_austen, "\n")
cat("\n[MECANICA]\n", texto_generado_mecanica, "\n")

# COMMAND ----------

# MAGIC %md
# MAGIC ## Conclusiones
# MAGIC
# MAGIC 1. El vocabulario y "tono" del texto generado viene 100% del corpus de entrenamiento.
# MAGIC 2. Mas alla de 2-3 palabras el texto deja de ser coherente: un bigrama solo
# MAGIC    "recuerda" la palabra inmediata anterior, no el sentido global de la oracion.
# MAGIC 3. Esta es la version mas simple posible de la idea que usan los LLM modernos
# MAGIC    (predecir la siguiente palabra dado el contexto): la diferencia es que un LLM
# MAGIC    usa una red neuronal tipo Transformer entrenada sobre miles de millones de
# MAGIC    palabras, con un contexto de miles de tokens en vez de solo 1, y con
# MAGIC    representaciones (embeddings) que capturan significado, no solo conteos. Más adelante 
# MAGIC    se mostrará un ejemplo basico de una NN entrenado con un corpus pequeño.