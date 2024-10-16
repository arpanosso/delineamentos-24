## Aula 08 - Experimentos Fatoriais
## Interação Significativa
## Carregando pacotes necessários
library(tidyverse)
library(ExpDes.pt)

# Experimentos Fatoriais
# Variável aleatória y - Produção do Milho (kg/parcela)
library(tidyverse)
y <- c(58,	45,	47, 61,	65,	47,
       31,	35,	29, 43,	51,	49,
       45,	55,	79, 31,	37,	37,
       78,	83,	62, 36,	34,	34)
hist(y)
inseticida <- gl(4,6) # gerando o fator A
dose <- gl(2,3,24) # gerando o fator B
trat <- interaction(inseticida, dose)

# gráficos da interação
interaction.plot(inseticida, dose, y, mean)
interaction.plot(dose, inseticida, y, mean)

# Criar o modelo da anova.
modelo <- aov(y ~ inseticida*dose)
anova(modelo)

# Desdobramento do Dose dentro de Inseticida.
modelo_1 <- aov(y ~ inseticida/dose)
anova(modelo_1)

# Para desdobrar a interação, precimos
# olhar a posição dos inseticidas dentro
# do vetor de efeitos (effects)
effects(modelo_1) %>% names() %>% tibble()

# Observamos que o Inseticida1 está na
# posição 1 do vetor da interação
# inseticida1:dose2 - 1
# inseticida2:dose2 - 2
# inseticida3:dose2 - 3
# inseticida4:dose2 - 4
# Assim, precisamos dos valores 1,2,3 e 4
# esses valores serão inseridos na lista
# do argumento "split" da função summary
summary(modelo_1,
        split = list(
          "inseticida:dose" = list(
            "Dose:I1" = 1,
            "Dose:I2" = 2,
            "Dose:I3" = 3,
            "Dose:I4" = 4
          )
        ))

# Desdobramento do Inseticida dentro da Dose.
modelo_2 <- aov(y ~ dose/inseticida)
anova(modelo_2)

# Para desdobrar a interação, precimos
# olhar a posição dos inseticidas dentro
# do vetor de efeitos (effects)
effects(modelo_2) %>% names() %>% tibble()

# Observamos que o Dose1 está na
# posição 1,  3 e 5 do vetor da interação
# "dose1:inseticida2"
# "dose2:inseticida2"
# "dose1:inseticida3"
# "dose2:inseticida3"
# "dose1:inseticida4"
# "dose2:inseticida4"
# Assim, precisamos dos valores 1,3 e 5 para dose1
# e os valores 2, 4 e 6 para a dose2
# esses valores serão inseridos na lista
# do argumento "split" da função summary
summary(modelo_2,
        split = list(
          "dose:inseticida" = list(
            "Inset:D1" = c(1,3,5),
            "Inset:D2" = c(2,4,6)
          )
        ))

# Análise utilizando pacote ExpDes.pt
ExpDes.pt::fat2.dic(inseticida,dose,y)

# Exercício prático
y <- c(39.5,	41.3,	38.1,	36.0,
       37.8,	35.6,	41.7,	35.8,
       36.3,	36.3,	34.5,	39.3,
       25.6,	22.0,	20.4,	25.8,
       29.0,	24.9,	29.9,	27.1,
       20.7,	20.3,	19.5,	17.8)
cult <- gl(2,12)
esp <- gl(3,4,24)
bloco <- gl(4,1,24)

# Análise de Variância
modelo <- aov(y ~ bloco + cult*esp)
anova(modelo)

# desdobrando a interação cultivar dentro
# espaçamento
modelo_1 <- aov(y ~ bloco + esp/cult)
effects(modelo_1) %>% names() %>% tibble()
summary(modelo_1,
        split = list(
          "esp:cult" = list(
            "Cult:E1" = 1,
            "Cult:E2" = 2,
            "Cult:E3" = 3
          )
        ))
# desdobrando a interação espaçamento dentro
# cultivar
modelo_2 <- aov(y ~ bloco + cult/esp)
effects(modelo_2) %>% names() %>% tibble()
summary(modelo_2,
        split = list(
          "cult:esp" = list(
            "Esp:C1" = c(1,3),
            "Esp:C2" = c(2,4)
          )
        ))

# Análise utilizando pacote ExpDes.pt
ExpDes.pt::fat2.dbc(cult,esp,bloco,y,
      fac.names = c("Cultivar","Espaçamento"))
