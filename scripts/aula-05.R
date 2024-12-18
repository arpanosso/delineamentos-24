# AULA 5 - 25/09/2024, Programado por SEU NOME
# Carregando pacotes
library(tidyverse)

## Entrada de dados
data_set <- read_rds("data/aula5.rds")
data_set

# caso 1: Sem informação prévia sobre trats;
## Anova pelo ExpDes
trat <- data_set %>% pull(trat) %>% as_factor()
y <- data_set %>% pull(resp)
modelo <- aov(y ~ trat)
anova(modelo)
agricolae::HSD.test(modelo,"trat",
                    group = TRUE,
                    console = TRUE)

# caso 2: com duas origens ou acessos;
contrasts(trat)

# construção dos coeficientes dos contrastes
contrastes_01 <- cbind(
  c(-1, -1, 1, 1),
  c(-1, 1, 0, 0),
  c(0, 0, -1, 1)
)

# atribuição dos contrastes
contrasts(trat) <- contrastes_01
contrasts(trat)

# Definição do novo modelo para o desdobramento dos graus
# de liberdade de tratamentos
modelo_01 <- aov(y ~ trat)
summary(modelo_01,
        split = list(trat=
                      list("origem A vs origem B"= 1,
                           "trat dentro da origem A"= 2,
                           "trat dentro da origem B"= 3)))

# Criando as variáveis auxiliáres.
modelo_01_1 <- lm(resp ~ Origem + OdA + OdB,
            data = data_set %>%
              mutate(
                Origem = ifelse(origem == "A",-1,1),
                OdA = ifelse(trat==1,-1,ifelse(trat==2,1,0)),
                OdB = ifelse(trat==3,-1,ifelse(trat==4,1,0))
              )
)
anova(modelo_01_1)

# caso 3: com uma testemunha;
contrasts(trat)

# construção dos coeficientes dos contrastes
contrastes_02 <- cbind(
  c(-3, 1, 1, 1),
  c(0, -2, 1, 1),
  c(0, 0, -1, 1)
)

# atribuição dos contrastes
contrasts(trat) <- contrastes_02
contrasts(trat)

#Definição do novo modelo para o desdobramento dos graus de liberdade
modelo_02<-aov(y~trat)
summary(modelo_02,
        split= list(trat=
                      list("testemunha vs demais"= 1,
                           "origem dentro dos novos"= 2,
                           "trat dentro da origem B"= 3)))


# ou utilizando variáveis auxiliares
modelo_02_1 <- lm(resp ~ TestVsNovo + OdNovo + TratdOb,
            data=data_set %>%
              mutate(
                TestVsNovo = ifelse(trat==1,-3,1),
                OdNovo = ifelse(trat==2,-2,ifelse(trat==1,0,1)),
                TratdOb = ifelse(trat==3,-1,ifelse(trat==4,1,0))
              ))
anova(modelo_02_1)

# caso 4: dialelos ou fatorial;
contrasts(trat)

# construção dos coeficientes dos contrastes
contrastes_03 <- cbind(
  c(-1, -1, 1, 1),
  c(-1, 1, -1, 1),
  c(1, -1, -1, 1)
)

# atribuição dos contrastes
contrasts(trat) <- contrastes_03
contrasts(trat)

#Definição do novo modelo para o desdobramento dos graus de liberdade
modelo_03<-aov(y~trat)
summary(modelo_03,
        split= list(trat=
                      list("mae a1 vs mae a2"= 1,
                           "pai b1 vs pai b2"= 2,
                           "interação mae e pai"= 3)))


## Ou simplesmente
modelo_03_1 <- lm(resp ~ mae*pai, data=
              data_set)
anova(modelo_03_1)

# caso 5: ajuste de regressão
contrasts(trat)

# construção dos coeficientes dos contrastes
contrastes_04 <- cbind(
  c(-3, -1, 1, 3),
  c(1, -1, -1, 1),
  c(-1, 3, -3, 1)
)

# atribuição dos contrastes
contrasts(trat) <- contrastes_04
contrasts(trat)

#Definição do novo modelo para o desdobramento dos graus de liberdade
modelo_04<-aov(y~trat)
summary(modelo_04,
        split= list(trat=
                      list("linear"= 1,
                           "quadrático"= 2,
                           "cúbico"= 3)))

# analise de regressão
ExpDes.pt::dic(trat, y, quali=FALSE)

