#library(tidyverse)
#library(ggpubr)

data <- tibble(
  Modelo=c("IPhone","Samsung","Xiaomi"),
  Cantidad=c(55,22,23)
)

pregunta5a_test3 <- ggplot(data, aes(x="", y=Cantidad, fill=Modelo)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + 
  theme_void()

pregunta5b_test3 <- ggplot(data, aes(y=Cantidad, x=Modelo, fill=Modelo)) + 
  geom_col() + 
  labs(x="Modelo", y="Porcentaje") + 
  theme_minimal()

pregunta5c_test3 <- ggarrange(pregunta5a_test3, pregunta5b_test3, labels = c("A", "B"), ncol=2)

pregunta5_test3 <- annotate_figure(pregunta5c_test3,
                top = text_grob("¿Que tipo de teléfono tienes?", color = "black", face = "bold", size = 14))


data2 <- tibble(
  y=c(rep(1250,200),rep(1500,400),rep(1750,600),rep(2000,350),rep(2750,447),3700,3900,4000),
  Grupo=rep("Empleados",2000)
)

pregunta6_test3 <- ggplot(data2, aes(x=Grupo, y=y)) + 
  geom_boxplot(fill="#AA6666") + 
  scale_y_continuous(limits = c(1250,4000), breaks = seq(1250,4000,250)) + 
  geom_rect(aes(xmin = 0.97, ymin = 2475, xmax = 1.035, ymax = 2525)) +
  labs(x="", y="Salario") + 
  theme_minimal()

data3 <- tibble(
  num=c(21,9,12,31,11,48)*5,
  genero=c(rep("Hombre",3),rep("Mujer",3)),
  Enfermedad=c(rep(c("Alzheimer","Otro","No"),2)) %>% as_factor()
)

pregunta7_test3 <- ggplot(data3, aes(x=genero, y=num, fill=Enfermedad)) + 
  geom_col(position = "dodge") + 
  labs(x="Género", y="", title="Presencia de demencia en la población anciana") +
  theme_minimal()

data4 <- tibble(
  peso=c(seq(-3,3,0.2),7.5),
  sem=c(peso[-32]^3/4+peso[-32]^2/10-(3*peso[-32])/2+1.5,6)
)

pregunta8_test3 <- ggplot(data4, aes(x=peso, y=sem)) + 
  geom_jitter() +
  geom_vline(aes(xintercept = 0)) +
  geom_hline(aes(yintercept = 0)) +
  labs(x="Peso perdido (en Kg)", y="Meses de dieta", title="Peso perdido por gente sometida a una dieta experimental") +
  theme_minimal()

data5 <- tibble(
  y=c(0,0,0,0,3,5,7,7,9,9,10,10,10,10,8,12,14,14,14,20,0,2,3,6,6,6,7,7,7,7,9,19,19,10,16,12,17,18,18,20),
  Grupo=c(rep("4ºA",20), rep("4ºB",20))
)

pregunta9_test3 <- ggplot(data5, aes(x=Grupo, y=y)) + 
  geom_boxplot(fill="#AA6666") + 
  scale_y_continuous(limits = c(0,20)) +
  labs(x="Clase", y="Nota del examen (sobre 20)", title="Notas de un examen sobre dos clases") +
  theme_minimal()
