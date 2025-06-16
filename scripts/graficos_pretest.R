#library(tidyverse)

data <- read.csv("data/datos_falsos.csv", header = TRUE, sep = ";") %>% as_tibble()

circular_pretest <- ggplot(data, aes(x="", y=Cantidad, fill=Modelo)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + 
  theme_void()

barras_pretest <- ggplot(data, aes(x=Modelo, y=Cantidad, fill=Modelo)) + 
  geom_col() + 
  theme_minimal()

data2 <- tibble(
  x=1:410,
  y=abs(rnorm(410, 50, 25)),
  Modelo=c(rep("Chromebook", 100), rep("Windows", 90), rep("Mac", 115), rep("Linux", 105)) %>% as_factor()
)

puntos_pretest <- ggplot(data2, aes(x=x, y=y, color=Modelo)) + 
  geom_point() + 
  labs(x="Modelo", y="Cantidad") + 
  theme_minimal()

# data3 <- tibble(
#   x=1:4,
#   y=c(100, 90, 115, 105),
#   Modelo=c("Chromebook","Windows","Mac","Linux")
# )

data3 <- tibble(
  x=c(1,1.5,1.5,2,2.5,2.5,3,3.5,3.5,4),
  y=c(100,95,95,90,102.5,102.5,115,110,110,105),
  Modelo=c(rep("Chromebook",2),rep("Windows",3),rep("Mac",3),rep("Linux",2)) %>% as_factor()
)

linea_pretest <- ggplot(data3, aes(x=x, y=y, color=Modelo)) + 
  geom_line(linewidth=1) + 
  theme_minimal() + 
  labs(x="", y="Cantidad") +
  theme(axis.text.x=element_blank())
