# AULA 4 - 11/09/2024, Programado por SEU NOME
# Carregando pacotes
library(tidyverse)

# Entrada de dados
data_set <- read_rds("data/aula4.rds")

# Fazer a análise de variância - Delineamento Inteiramente
# Casualizado
# Calculando o G (soma total, fator de correção
# e os graus de liberdade de cada FV)
data_set %>%
  group_by(trat) %>%
  mutate(Ti = sum(y)) %>%
  ungroup() %>%
  summarise(
    I = n_distinct(trat),
    J = n_distinct(rep),
    G=sum(y),
    C = G^2/n(),
    GLtr = n_distinct(trat)-1,
    GLt = n() - 1,
    GLr =GLt-GLtr,
    SQtotal = sum(y^2) - C,
    SQtr = 1/J*sum((Ti)^2/J) - C,
    SQr = SQtotal - SQtr,
    QMtr = SQtr/GLtr,
    QMr = SQr/GLr,
    Ftr = QMtr/QMr,
    r2 = SQtr/SQtotal,
    dp = sqrt(QMr),
    media_g = G/(I*J),
    cv = 100*dp/media_g,
    epm = sqrt(QMr/J)
  ) %>% t() %>%
  round(.,2)


modelo <- aov(y ~ trat,
              data = data_set)
anova(modelo)

data_set %>%
  group_by(trat) %>%
  summarise(
    medias = mean(y)
  ) %>%
  ggplot(aes(x=trat, y=medias)) +
  geom_col(fill="gray", color="black")

data_set %>%
  group_by(trat) %>%
  summarise(
    medias = mean(y)
  ) %>% arrange(desc(medias))

# Fazer Procedimento Para Comparação de Médias
# Realizar o teste t-Student
library(agricolae)
trat <- data_set %>% pull(trat)
y <- data_set %>% pull(y)
LSD.test(y,trat,28,2.757,
         console = TRUE)

# Realizar o teste de Tukey
HSD.test(y,trat,28,2.757,
         console = TRUE)
plot(TukeyHSD(modelo, conf.level=.95), las = 2)
## Utilizando o pacote ExpDes.pt
library(ExpDes.pt)
dic(trat,y)







