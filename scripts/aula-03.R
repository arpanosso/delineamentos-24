# Carregar os Pacotes
library(tidyverse)

# Entrada de Dados
data_set <- read_rds("data/aula2.rds")
data_set

# Representação gráfica
data_set %>%
  ggplot(aes(x=trat, y=resp)) +
  geom_point()

# Soma de Quadrados TOTAL (SQT)
data_set %>%
  mutate(
    desvio = resp - mean(resp),
    desvio2 = desvio^2
  )   %>%
  summarise(
    SQT = sum(desvio2)
  ) %>% pull(SQT)

# SQTR - Soma de quadrados de Tratamentos
data_set %>%
  mutate(
    media_geral = mean(resp)
  ) %>%
  group_by(trat) %>%
  mutate(
    modelo_media = mean(resp),
    desvio2 = (modelo_media-media_geral)^2
  ) %>%
  ungroup() %>%
  summarise(
    SQTR = sum(desvio2)
  ) %>% pull(SQTR)
# SQ erro puro = SQT - SQTR
18.255-17.415

data_set %>%
  group_by(trat) %>%
  mutate(
    modelo_media = mean(resp)
  ) %>%
  ungroup() %>%
  mutate(
    desvio2 = (resp - modelo_media)^2
  ) %>%
  summarise(
    SQER = sum(desvio2)
  )

# Análise nas funções BASES
modelo_media <- aov(resp ~ trat,
                    data = data_set)
anova(modelo_media)

# PAcote ExpDes.pt
library(ExpDes.pt)
trat <- data_set %>% pull(trat)
resp <- data_set %>% pull(resp)
dic(trat,resp,quali = TRUE)

# Modelo de Reta
lm(resp ~ dose,
   data = data_set)

# Modelo reta
data_set %>%
  mutate(
    modelo_reta = 7.55 + 1.09*dose,
    desvio2 = (resp - modelo_reta)^2
  ) %>%
  summarise(
    SQD = sum(desvio2)
  ) %>% pull(SQD)
# Preciso descontar o EP
6.374-0.84

# Análise de regressão
dose <- data_set %>% pull(dose)
dic(dose,resp,quali = FALSE)


# Modelo Parábola
lm(resp ~ dose + dose2,
   data = data_set %>%
     mutate(dose2 = dose^2))

data_set %>%
  mutate(
    modelo_parab = 3.675+4.965*dose-0.775*dose^2,
    desvio2 = (resp - modelo_parab)^2
  ) %>%
  summarise(
    SQD = sum(desvio2)
  ) %>% pull(SQD)
# Temos que descontar o erro puro
1.569 - 0.84




# Modelo Cúbico
lm(resp ~ dose + dose2 + dose3,
   data = data_set %>%
     mutate(dose2 = dose^2,
            dose3 = dose^3))

data_set %>%
  mutate(
    modelo_cubo = 8.4-2.55*dose+2.6*dose^2-0.45*dose^3,
    desvio2 = (resp - modelo_cubo)^2
  ) %>%
  summarise(
    SQD = sum(desvio2)
  ) %>% pull(SQD)
# Temos que descontar o erro puro
1.569 - 0.84

















