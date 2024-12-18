# Faxina dos dados da aulas 02
library(tidyverse)
library(readxl)

# Entrada de Dados
data_set <- read_xlsx("data-raw/aula2.xlsx")

# Mostrando o objeto
data_set

# Média de RESP
data_set %>%
  pull(RESP) %>%
  mean()

data_set %>%
  mutate(RESP2 = RESP^2)

# Arrumando os nomes das colunas
data_set <- data_set %>%
  janitor::clean_names()
data_set

# Visualização
data_set %>%
  ggplot(aes(y= resp)) +
  geom_boxplot(fill="orange")+
  xlim(-1,1)

data_set %>%
  ggplot(aes(y= resp, x=trat, fill = trat)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_viridis_d() +
  labs(fill="",y="Produção da Cana (TCH)")

data_set

# Salvando o banco de dados arrumado
write_rds(data_set,"data/aula2.rds")

# Faxina para aula3
data_set <- read_xlsx("data-raw/aula3.xlsx") %>%
  janitor::clean_names()
# Salvando o banco de dados arrumado
write_rds(data_set,"data/aula4.rds")

# Faxina para aula4
data_set <- readxl::read_xlsx("data-raw/aula4.xlsx") %>%
  janitor::clean_names()
# Salvando o banco de dados arrumado
write_rds(data_set,"data/aula5.rds")


# Faxina para aula4
data_set <- readxl::read_xlsx("data-raw/aula6-varias-variaveis.xlsx") %>%
  janitor::clean_names() %>%
  select(macrofita:al)
glimpse(data_set)
# Salvando o banco de dados arrumado
write_rds(data_set,"data/aula6-01.rds")

# Faxina para aula7
data_set <- readxl::read_xlsx("data-raw/aula7-2.xlsx") %>%
  janitor::clean_names()

glimpse(data_set)
# Salvando o banco de dados arrumado
write_rds(data_set,"data/aula7-01.rds")

# Faxina para aula8
data_set <- readxl::read_xlsx("data-raw/aula8.xlsx") %>%
  janitor::clean_names()

glimpse(data_set)
# Salvando o banco de dados arrumado
write_rds(data_set,"data/aula8.rds")

# Faxina para aula9
data_set <- readxl::read_xlsx("data-raw/aula09-psub.xlsx") %>%
  janitor::clean_names()

glimpse(data_set)
# Salvando o banco de dados arrumado
write_rds(data_set,"data/aula9.rds")
