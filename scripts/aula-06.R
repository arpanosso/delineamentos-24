# Aula 06 - Análise para várias variáveis
## Exemplo para análise com várias variáveis
## Experimento Fatorial em DIC
### Fator A - macrófitas
### Fator B - Doses de substratos
### Variávies - quimica do solo (macro e micro nutrientes)

# Realizar a anova para várias variáveis contendo
## 1) Estatística Descritiva
## 2) Gráficos da variável por tratamentos (combinação dos níveis dos fatores)
## 3) Análise de variância

# Carrgando Pacotes
library(tidyverse)

# Entrada de dados
data_set <- read_rds("data/aula6-01.rds")
glimpse(data_set)

# Seleção da variável resposta
y <- data_set %>% pull(mo)

# Seleção dos fatores
macrofitas <- data_set %>% pull(macrofita)
doses <- data_set %>% pull(dose_t_ha)

# Criar os tratamentos
trat <- interaction(macrofitas,doses,sep = "_")

# Estatistica Descritiva
n <- length(y)
media <- mean(y,na.rm = TRUE)
variancia <- var(y,na.rm = TRUE)
desv_pad <- sd(y,na.rm = TRUE)
epm <- sqrt(variancia/n)
cv <- 100*desv_pad / media

# Gráfico de médias por tratamentos
tibble(trat,macrofitas,doses,y) %>%
  ggplot(aes(x=trat,y=y,fill=macrofitas))+
  geom_boxplot()+
  labs(title = "Boxplot: mo",
       y = "mo",
       x = "Tratamentos") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = .5,size=15)
    )

tibble(trat,macrofitas,doses,y) %>%
  ggplot(aes(x=doses, y=y, colour = macrofitas)) +
  geom_point() +
  facet_wrap(~macrofitas, scales = "free",ncol=3)+
  labs(y="mo", x= "Doses") +
  theme_bw() +
  theme(
    legend.position = "none"
  )

# Anova
doses <- as_factor(doses)
modelo <- aov( y ~ macrofitas*doses)
anova(modelo)

# Estrutura de repetição
## buscando os nomes das variávies
variaveis <- data_set %>%
  select(mo:al) %>%
  names()

## realizando o laço
for(i in seq_along(variaveis)){
  my_var <- variaveis[i]
  print("-----------------------")
  print(my_var)
  print("-----------------------")
  # Seleção da variável resposta
  y <- data_set %>% pull(my_var)

  # Seleção dos fatores
  macrofitas <- data_set %>% pull(macrofita)
  doses <- data_set %>% pull(dose_t_ha)

  # Criar os tratamentos
  trat <- interaction(macrofitas,doses,sep = "_")

  # Estatistica Descritiva
  n <- length(y)
  media <- mean(y,na.rm = TRUE)
  variancia <- var(y,na.rm = TRUE)
  desv_pad <- sd(y,na.rm = TRUE)
  epm <- sqrt(variancia/n)
  cv <- 100*desv_pad / media
  cat("N:", n, "\n")
  cat("Média:", round(media,2), "\n")
  cat("Variância:", round(variancia,2), "\n")
  cat("Desvio-padrão:", round(desv_pad,2), "\n")
  cat("Erro-padrão:", round(epm,2), "\n")
  cat("Coef. Variação:", round(cv,2), "%","\n")

  # Gráfico de médias por tratamentos
  plot_box <- tibble(trat,macrofitas,doses,y) %>% #####
    ggplot(aes(x=trat,y=y,fill=macrofitas))+
    geom_boxplot()+
    labs(title = my_var, #### <-
         y = my_var, #### <-
         x = "Tratamentos") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = .5,size=15)
    )
  print(plot_box) #### print do gráfico

  plot_disper <- tibble(trat,macrofitas,doses,y) %>%
    ggplot(aes(x=doses, y=y, colour = macrofitas)) +
    geom_point() +
    facet_wrap(~macrofitas, scales = "free",ncol=3)+
    labs(y=my_var, x= "Doses") +
    theme_bw() +
    theme(
      legend.position = "none"
    )
  print(plot_disper)

  # Anova
  doses <- as_factor(doses)
  modelo <- aov( y ~ macrofitas*doses)
  print(anova(modelo))
}


















