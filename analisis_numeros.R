leer_numeros <- function(nombre_archivo) {
  if (!file.exists(nombre_archivo)) {
    stop("El archivo no existe.")
  }
  numeros <- as.integer(readLines(nombre_archivo))
  return(numeros)
}

# Usar la ruta completa para leer el archivo
numeros <- leer_numeros("c:\\Users\\maria\\Downloads\\1729760754907-numeros.txt")

# Calcular los estadísticos
calcular_estadisticos <- function(numeros) {
  media <- mean(numeros)
  mediana <- median(numeros)
  desviacion_estandar <- sd(numeros)
  return(list(media = media, mediana = mediana, desviacion_estandar = desviacion_estandar))
}

estadisticos <- calcular_estadisticos(numeros)

# Verificar la desviación estándar
if (estadisticos$desviacion_estandar > 10) {
  print("Hay alta variabilidad en los datos.")
}

# Calcular los cuadrados de los números
calcular_cuadrados <- function(numeros) {
  cuadrados <- sapply(numeros, function(x) x^2)
  return(cuadrados)
}

cuadrados <- calcular_cuadrados(numeros)

# Escribir los resultados en el archivo resultados.txt
escribir_resultados <- function(estadisticos, cuadrados, nombre_archivo) {
  archivo <- file(nombre_archivo, "w")
  writeLines(c(
    paste("Media:", estadisticos$media),
    paste("Mediana:", estadisticos$mediana),
    paste("Desviación estándar:", estadisticos$desviacion_estandar),
    "Cuadrados de los números:",
    paste(cuadrados, collapse = ", ")
  ), archivo)
  close(archivo)
}

# Guardar los resultados
escribir_resultados(estadisticos, cuadrados, "resultados.txt")
getwd()
