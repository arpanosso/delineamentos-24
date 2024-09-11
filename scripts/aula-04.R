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
    r2 = SQtr/SQtotal
  ) %>% t() %>%
  round(.,2)


modelo <- aov(y ~ trat,
              data = data_set)
anova(modelo)


# Fazer Procedimento Para Comparação de Médias
# Realizar o teste t-Student

# Realizar o teste de Tukey
