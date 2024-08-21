# Distribuição Binomial
# Lançar 3 moedas e contar o número de caras.
# x são os possíveis valores da variável
# p é a probabilidade de sucesso (cair cara)
# q é a probabilidade de fracasso (1 - p)
library(tidyverse)
n <- 10
x <- 0:n
p <- 1/2
q <- 1-p

t_binomail <- tibble(x)

t_binomail %>%
  mutate(px=dbinom(x,n,p)) %>%
  ggplot(aes(x=x, y=px)) +
  geom_col(fill="orange", color="black") +
  theme_bw()

# Distribuição Normal
media <- 70
desv_pad <- 12

curve(dnorm(x,media,desv_pad), 30, 110)
abline(v=media,col="red")
abline(v=78, col="blue")
1-pnorm(78, media, desv_pad) # P(X>=78)

curve(dnorm(x,media,desv_pad), 30, 110)
abline(v=media,col="red")
abline(v=65, col="blue")
pnorm(65,media, desv_pad) # P(X <= 65)

curve(dnorm(x,media,desv_pad), 30, 110)
abline(v=media,col="red")
abline(v=68, col="blue")
abline(v=75, col="blue")
pnorm(75,media, desv_pad) - pnorm(68,media, desv_pad)
# P(68 <= X <= 75)

# DISTRIBUIÇÃO DA MÉDIA
# simular uma população qualquer
pop <- rep(1:9,c(57,600,4,68,888,
                 500,500,1000,976))
hist(pop)
mean(pop)
var(pop)

# coletar uma amostra aleatória simples
# com reposição tamanho n = 5
n=500
sample(pop,n,replace = TRUE)

#calcular a média para essa amostra
mean(sample(pop,n,replace = TRUE))

# simular k amostragens
k = 10000
amostra_medias <- 0
for(i in 1:k){
  amostra_medias[i] <- mean(sample(pop,
                                   n,
                                   replace = TRUE))
}
hist(amostra_medias)
mean(amostra_medias)















