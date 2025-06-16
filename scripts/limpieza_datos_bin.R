library(tidyverse)

# Cargamos los datos y eliminamos la primera columna (Marca Temporal)
pretestBIN <- read_csv("data/pretest.csv")
pretestBIN <- pretestBIN[-1]
test1BIN <- read_csv("data/test1.csv")
test1BIN <- test1BIN[-1]
test2BIN <- read_csv("data/test2.csv")
test2BIN <- test2BIN[-1]
test3BIN <- read_csv("data/test3.csv")
test3BIN <- test3BIN[-1]

# Cambiamos los nombres de las columnas
colnames(pretestBIN) = c("P1", "P2", "P3", "P4", "P5", "P6", "P7")
colnames(test1BIN) = c("P1", "P2", "P3", "P4", "P5")
colnames(test2BIN) = c("Participación", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9")
colnames(test3BIN) = c("Participación", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9")

# Aplicamos el formato necesario para analizar
pretestBIN <- pretestBIN %>% mutate(P1 = if_else(pretestBIN$P1 == "\"¿Cuántos hermanos tienes?\"", 1, 0)) %>% 
  mutate(P2 = if_else(pretestBIN$P2 == "\"¿Cuál es tu altura (en centímetros)?\"", 1, 0)) %>% 
  mutate(P3 = if_else(pretestBIN$P3 == "\"¿Tienes hermanos?\"", 1, 0)) %>% 
  mutate(P4 = if_else(pretestBIN$P4 == "La nota media de matemáticas de cada estudiante de cada centro participante en el estudio", 1, 0)) %>% 
  mutate(P5 = if_else(pretestBIN$P5 == 12, 1, 0)) %>% 
  mutate(P6 = if_else(pretestBIN$P6 == "Comparar el porcentaje de alcohólicos en Palma y en Barcelona", 1, 0)) %>% 
  mutate(P7 = if_else(pretestBIN$P7 == "Un diagrama de barras", 1, 0))

test1BIN <- test1BIN %>% mutate(P1 = if_else(test1BIN$P1 == "No", 1, 0)) %>% 
  mutate(P2 = if_else(test1BIN$P2 == "Sí", 1, 0)) %>% 
  mutate(P3 = if_else(test1BIN$P3 == "Cuantitativa contínua", 1, 0)) %>% 
  mutate(P4 = if_else(test1BIN$P4 == "Cualitativa", 1, 0)) %>% 
  mutate(P5 = if_else(test1BIN$P5 == "Cuantitativa discreta", 1, 0))

test2BIN <- test2BIN %>% mutate(P1 = if_else(test2BIN$P1 == "Sí", 1, 0)) %>% 
  mutate(P2 = if_else(test2BIN$P2 == "Cuantitativa contínua", 1, 0)) %>% 
  mutate(P3 = if_else(test2BIN$P3 == "Cuantitativa discreta", 1, 0)) %>% 
  mutate(P4 = if_else(test2BIN$P4 == "Los residentes de las Islas Baleares responsables de hacer la compra", 1, 0)) %>% 
  mutate(P5 = if_else(test2BIN$P5 == "Seleccionar gente proporcional a cada municipio de las Islas Baleares", 1, 0)) %>% 
  mutate(P6 = if_else(test2BIN$P6 == "Fórmula 1", 1, 0)) %>% 
  mutate(P7 = if_else(test2BIN$P7 == "Escala lineal", 1, 0)) %>% 
  mutate(P8 = if_else(test2BIN$P8 == "No, porque está abierta a interpretación", 1, 0)) %>% 
  mutate(P9 = if_else(test2BIN$P9 == "Sí, siempre y cuando se reformule para que no esté cargada", 1, 0))

test3BIN <- test3BIN %>% mutate(P1 = if_else(test3BIN$P1 == "Cuantitativa contínua", 1, 0)) %>% 
  mutate(P2 = if_else(test3BIN$P2 == "Los alumnos de ESO de todos los institutos de la ciudad", 1, 0)) %>% 
  mutate(P3 = if_else(test3BIN$P3 == "Sí, la pregunta es correcta", 1, 0)) %>% 
  mutate(P4 = if_else(test3BIN$P4 == "La mediana", 1, 0)) %>% 
  mutate(P5 = if_else(test3BIN$P5 == "El gráfico B", 1, 0)) %>% 
  mutate(P6 = if_else(test3BIN$P6 == "El 50% central de los empleados cobran entre 1500€ y 2000€", 1, 0)) %>% 
  mutate(P7 = if_else(test3BIN$P7 == "En frecuencia relativa, los hombres tiene más demencia que las mujeres", 1, 0)) %>% 
  mutate(P8 = if_else(test3BIN$P8 == "Existe un valor influyente que cambia la regresión", 1, 0)) %>% 
  mutate(P9 = if_else(test3BIN$P9 == "Por los bigotes, el 4ºB tiene mayor dispersión que el 4ºA", 1, 0))
