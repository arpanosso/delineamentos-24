# Aula 01 - Seu Nome - 14/08/2024
# Quais pacotes estão no "ambiente"
(.packages())

# Instalar Pacotes
# \Aba Packages \Install \digitar o nome \ install

# Instalar por comando
# install.packages("nortest")

# quais pacotes estão no ambiente
(.packages())

# CARREGANDO PACOTES NO AMBIENTE
library(agricolae)

# quais pacotes estão no ambiente
(.packages())

# TIPOS PRIMITIVOS
# operador de atribuição
# é com ele que salvo os dados em objetos
# símbolo é:
# vamos ATRIBUIR o número 1 ao objeto x
x <- 1 # tipo inteiro
y <- 4.25 # tipo real ou double
z <- 3 > 5 # tipo lógico
w <- "seu nome" # tipo caracter

# PRINCIPAIS OBJETOS NO R
# Vetor
idade_alunos <- c(21, 24, 27, 23)

# acessar a idade na posição 3
idade_alunos[3]

# acessar a idade na posição 1 e 3
idade_alunos[c(1,3)]

# adicionar mais uma idade ao vetor
idade_alunos[5] <- 22

# Qual o tamanho do vetor? qual o n
length(idade_alunos)
mean(idade_alunos) # média aritmética
sum(idade_alunos) # soma dos elementos
var(idade_alunos) # variância amostral

# variância calculada por partes
# s2 = SQD/(n-1)
n <- length(idade_alunos)
sqd <- sum(idade_alunos^2) - sum(idade_alunos)^2 / n
sqd/(n-1)

# MATRIZ
vetor_x <- 1:12
matriz_x <- matrix(vetor_x,
                   ncol = 3,
                   byrow = FALSE)
matriz_x[3,2]
matriz_x[3,]
matriz_x[,2]

# LISTAS
disciplina <- list(
  profs = c("alan", "dilermando"),
  alunos = c("taina","davi","laura","aldo"),
  horarios = matrix(c(8,12,14,17),
                    ncol = 2,
                    byrow = TRUE),
  notas = rep(NA,4)
)

# Acessar os nomes dos aluno
disciplina$alunos

# Acessar os horários
disciplina$horarios

# Adicionar "rafael" em nomes
disciplina$alunos[5] <- "rafael"

# Adicionar NA em notas
disciplina$notas[5] <- NA

# Criar o material
disciplina$material <- c("caderno","caneta",
                         "notebook","agua")

#acessar todos os elementos da lista
str(disciplina)

# DATA FRAME
data_set <- data.frame(
  alunos = c("taina","davi","laura","aldo"),
  idade = c(21,24,27,23),
  altura = c(155, 156, 173, 179),
  notas = c("A","A","A","A"),
  prova1 = c(7,8,9,10),
  prova2 = c(10,9,8,7)
)

data_set
# número de linhas e colunas da tabela
nrow(data_set)
ncol(data_set)

# acessar a idade
data_set$idade

# qual a média de altura dos alunos?
mean(data_set$altura)

# calcular a média final
data_set$mf <- (2*data_set$prova1+3*data_set$prova2)/5
data_set






