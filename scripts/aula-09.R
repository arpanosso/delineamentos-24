library(tidyverse)
library(readxl)
data_set <- read_xlsx("data-raw/aula09-psub.xlsx")
glimpse(data_set)

mod <- aov(prod ~ epoca + bloco + bloco/epoca +
             variedade + epoca:variedade,
           data = data_set %>%
             mutate(
               epoca = as_factor(epoca),
               variedade = as_factor(variedade),
               bloco = as_factor(bloco)
             ))
anova(mod)

# Utilizando o ExpDes
variedade <- data_set %>% pull(variedade )
bloco <- data_set %>% pull(bloco)
epoca <- data_set %>% pull(epoca)
y <- data_set %>% pull(prod)

ExpDes.pt::psub2.dbc(epoca, variedade,
                     bloco, y)

# Análise de resíduos
mod <- aov(prod ~ trat + bloco,
           data = data_set %>%
             mutate(
               epoca = as_factor(epoca),
               variedade = as_factor(variedade),
               bloco = as_factor(bloco),
               trat = interaction(epoca,variedade)
             ))
rs <- rstudent(mod)
hist(rs)
shapiro.test(rs)
trat <- interaction(variedade,epoca)
lawstat::levene.test(y,trat)


