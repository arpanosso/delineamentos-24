## Aula 07 - Experimentos Fatoriais
## Carregando pacotes necessários
library(tidyverse)
library(agricolae)
library(ExpDes.pt)

## Carregar o Banco de dados aula7.rds
## que está na pasta "data"
data_set <- read_rds("data/aula7.rds")
glimpse(data_set)

## Gráfico de interaçõe AxB
data_set %>%
  group_by(fator_A, fator_B) %>%
  summarise(
    media = mean(resp)
  ) %>%
  ggplot(aes(x=as.numeric(fator_B),
             y= media,
             color = fator_A)) +
  geom_point() +
  geom_line() +
  theme_bw()
## Gráfico de interaçõe BxA
data_set %>%
  group_by(fator_A, fator_B) %>%
  summarise(
    media = mean(resp)
  ) %>%
  ggplot(aes(x=as.numeric(fator_A),
             y= media,
             color = fator_B)) +
  geom_point() +
  geom_line() +
  theme_bw()
# gráfico boxplot dos tratamentos
data_set %>%
  mutate(
    trat = interaction(fator_A,fator_B)
  ) %>%
  group_by(trat) %>%
  mutate(
    media = mean(resp)) %>%
  ungroup() %>%
  mutate(
    trat = trat %>% fct_reorder(desc(media))
  ) %>%
  ggplot(aes(x=trat,y=resp,fill=trat)) +
  geom_boxplot() +
  theme_minimal()

## Análise de variância
# extraindo os fatores e a variável resposta
variedade <- data_set %>% pull(fator_A)
dose <- data_set %>% pull(fator_B)
y <- data_set %>% pull(resp)

# criar o modelo da análise
modelo <- aov(y ~ variedade*dose)
anova(modelo)

# Teste de Tukey para as Médias
# de A (variedade)
HSD.test(modelo,"variedade",
         group = TRUE,
         console = TRUE)

# Teste de Tukey para as Médias
# de B (dose)
HSD.test(modelo,"dose",
         group = TRUE,
         console = TRUE)

## Análise de Pressupostos.
# Montar o modelo no delineamento de
# Experimento (DIC, DBC, DQL...)
trat = interaction(variedade,dose)
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

























