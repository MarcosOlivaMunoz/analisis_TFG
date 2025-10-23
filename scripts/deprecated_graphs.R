pretest1 <- pretest1 %>% 
  mutate(P1 = case_when(pretest1$P1=="R1 (correcta)" ~ "Cuan. Disc.", 
                        pretest1$P1=="R2" ~ "Cual.", 
                        pretest1$P1=="R3" ~ "Distractor", 
                        pretest1$P1=="R4" ~ "Cuan. Cont.")) %>% 
  mutate(P2 = case_when(pretest1$P2=="R1" ~ "Cuan. Disc.", 
                        pretest1$P2=="R2" ~ "Cual.", 
                        pretest1$P2=="R3" ~ "Distractor", 
                        pretest1$P2=="R4 (correcta)" ~ "Cuan. Cont.")) %>% 
  mutate(P3 = case_when(pretest1$P3=="R1" ~ "Cuan. Disc.", 
                        pretest1$P3=="R2 (correcta)" ~ "Cual.", 
                        pretest1$P3=="R3" ~ "Distractor", 
                        pretest1$P3=="R4" ~ "Cuan. Cont.")) %>% 
  mutate(P4 = case_when(pretest1$P4=="R1" ~ "Distractor 1", 
                        pretest1$P4=="R2" ~ "Distractor 2", 
                        pretest1$P4=="R3 (correcta)" ~ "Resp. Correcta", 
                        pretest1$P4=="R4" ~ "Distractor 3")) %>% 
  mutate(P5 = case_when(pretest1$P5=="R1" ~ "Moda", 
                        pretest1$P5=="R2 (correcta)" ~ "Mediana", 
                        pretest1$P5=="R3" ~ "Media", 
                        pretest1$P5=="R4" ~ "Distractor")) %>% 
  mutate(P6 = case_when(pretest1$P6=="R1" ~ "Freq. Abs.", 
                        pretest1$P6=="R2 (correcta)" ~ "Freq. Rel.", 
                        pretest1$P6=="R3" ~ "Distractor 1", 
                        pretest1$P6=="R4" ~ "Distractor 2")) %>% 
  mutate(P7 = case_when(pretest1$P7=="R1" ~ "Circular", 
                        pretest1$P7=="R2 (correcta)" ~ "Barras", 
                        pretest1$P7=="R3" ~ "Puntos", 
                        pretest1$P7=="R4" ~ "Línea"))
pretest1 <- pretest1 %>% 
  mutate(P1=factor(P1,levels=c("Cuan. Disc.","Cual.","Distractor","Cuan. Cont.")),
         P2=factor(P2,levels=c("Cuan. Disc.","Cual.","Distractor","Cuan. Cont.")),
         P3=factor(P3,levels=c("Cuan. Disc.","Cual.","Distractor","Cuan. Cont.")),
         P4=factor(P4,levels=c("Distractor 1","Distractor 2","Resp. Correcta","Distractor 3")),
         P5=factor(P5,levels=c("Moda","Mediana","Media","Distractor")),
         P6=factor(P6,levels=c("Freq. Abs.","Freq. Rel.","Distractor 1","Distractor 2")),
         P7=factor(P7,levels=c("Circular","Barras","Puntos","Línea")))

plot1a <- ggplot(data=pretest1,aes(x=P1)) + 
  geom_bar(position = "dodge", fill=c("chartreuse4","coral3","coral3","coral3")) + 
  guides(fill = "none") + 
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,4)) +
  labs(x="P1: Cuantitativo Discreto", y="Frecuencia absoluta") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot1b <- ggplot(data=pretest1,aes(x=P2, fill=P2)) + 
  geom_bar(position = "dodge", fill=c("coral3","coral3","coral3","chartreuse4")) + 
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,4)) +
  labs(x="P2: Cuantitativo Continuo", y="") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot1c <- ggplot(data=pretest1,aes(x=P3, fill=P3)) + 
  geom_bar(position = "dodge", fill=c("coral3","chartreuse4","coral3","coral3")) + 
  guides(fill = "none") + 
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,4)) +
  labs(x="P3: Cualitativo", y="") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot1 <- ggarrange(plot1a, plot1b, plot1c, ncol=3, align = "v")