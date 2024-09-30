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

# Estatistica Descritiva
y <- data_set %>% pull(mo)
macrofita <- data_set %>% pull(macrofita) %>% as_factor()
dose <- data_set %>% pull(dose_t_ha)
trat <- interaction(macrofita, dose)
media <- mean(y, na.rm = TRUE)
desv_pad <- sd(y, na.rm = TRUE)

# Gráfico de médias por tratamentos
data_set %>%
  ggplot(aes(x=as_factor(dose_t_ha) , y=mo, fill=macrofita)) +
  geom_boxplot() +
  facet_wrap(~macrofita) +
  theme_bw()

# Anova
modelo <- aov(y ~ macrofita*dose)
anova(modelo)

# Estrutura de repetição
## buscando os nomes das variávies
variaveis <- data_set %>%
  select(mo:al) %>%
  names()
## realizando o laço
for(i in seq_along(variaveis)){
  print("------------------------------------")
  print(paste0("Análise para: ", variaveis[i]))
  print("------------------------------------")
  y <- data_set %>% pull(variaveis[i])
  macrofita <- data_set %>% pull(macrofita) %>% as_factor()
  dose <- data_set %>% pull(dose_t_ha)
  trat <- interaction(macrofita, dose)
  media <- mean(y, na.rm = TRUE)
  desv_pad <- sd(y, na.rm = TRUE)

  cat("Média: ", round(media,2), "\n")
  cat("Desvio Padrão: ", round(desv_pad,2), "\n")


  # Gráfico de médias por tratamentos
  plot_boxplot <- data_set %>%
    ggplot(aes(x=as_factor(dose_t_ha),
               y= !!sym(variaveis[i]),
               fill=macrofita)) +
    geom_boxplot() +
    facet_wrap(~macrofita) +
    theme_bw()
    print(plot_boxplot)

  # Anova
  modelo <- aov(y ~ macrofita*dose)
  print(anova(modelo))
}
