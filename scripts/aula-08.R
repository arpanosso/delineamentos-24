## Aula 08 - Experimentos Fatoriais
## Interação Significativa
## Carregando pacotes necessários
library(tidyverse)
library(agricolae)
library(ExpDes.pt)

## Carregar o Banco de dados aula8.rds
## que está na pasta "data"
data_set <- read_rds("data/aula8.rds")
glimpse(data_set)

## Gráfico de interaçõe AxB
data_set %>%
  group_by(inseticida, dose) %>%
  summarise(
    media = mean(prod)
  ) %>%
  ggplot(aes(x=as.numeric(dose),
             y= media,
             color = as_factor(inseticida))) +
  geom_point() +
  geom_line() +
  theme_bw()

## Gráfico de interaçõe BxA
data_set %>%
  group_by(inseticida, dose) %>%
  summarise(
    media = mean(prod)
  ) %>%
  ggplot(aes(x=as.numeric(inseticida),
             y= media,
             color = as_factor(dose))) +
  geom_point() +
  geom_line() +
  theme_bw()
# gráfico boxplot dos tratamentos
data_set %>%
  mutate(
    trat = interaction(inseticida, dose)
  ) %>%
  group_by(trat) %>%
  mutate(
    media = mean(prod)) %>%
  ungroup() %>%
  mutate(
    trat = trat %>% fct_reorder(desc(media))
  ) %>%
  ggplot(aes(x=trat,y=prod,fill=trat)) +
  geom_boxplot() +
  theme_minimal()

## Análise de variância
# extraindo os fatores e a variável resposta
inseticida <- data_set %>% pull(inseticida)
dose <- data_set %>% pull(dose)
y <- data_set %>% pull(prod)

# criar o modelo da análise
modelo <- aov(y ~ inseticida*dose)
anova(modelo)

## Análise de Pressupostos.
# Montar o modelo no delineamento de
# Experimento (DIC, DBC, DQL...)
trat = interaction(inseticida,dose)
modelo_diag <- aov(y ~ trat)
plot(modelo_diag)

# Teste da Normalidade dos Resíduos
# Extrair o Resíduos Studentzado
rs <- rstudent(modelo_diag)
hist(rs)

# Teste de normalidade
# rejeita H0 se p-value <= 0.05
shapiro.test(rs)
nortest::lillie.test(rs)
nortest::ad.test(rs)
nortest::cvm.test(rs)

# Testes de Homogeneidade de Variâncias
# Homocedasticidade
# Rejeita H0 se p-value <= 0.05
lawstat::levene.test(y,trat)
lawstat::levene.test(y,trat,
                     location = "mean")
MASS::boxcox(modelo_diag)
# não rejeita H0 se lambda = 1 pertencer
# ao intervalo de confiança.

## Diagnóstico de Outliers
y_pred <- predict(modelo_diag)
tibble(rs, y_pred) %>%
  ggplot(aes(x = y_pred, y=rs)) +
  geom_point() +
  ylim(-3.5,3.5) +
  geom_hline(yintercept = c(-3,3),
             color="red") +
  theme_bw()

## Desdobramento da Interação
# criar o modelo da análise
# Desdobramento de Inseticidade dentro de dose

























