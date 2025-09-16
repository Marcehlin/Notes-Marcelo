library(ggplot2)
lambda <- 4
p <- 0.25
pk_de_poisson_truncada <- function(lambda, k) {
  p_k <- (exp(-lambda) * lambda ^ k) / (factorial(k) * (1 - exp(-lambda)))
  return (p_k)
}

qk_de_geometrica <- function(p, k) {
  q_k <- (1 - p)^(k - 1) *p
  return (q_k)
}

k_valores <- 1:(10*lambda)
p_k <- sapply(k_valores, pk_de_poisson_truncada, lambda = lambda)
q_k <- sapply(k_valores, qk_de_geometrica, p = p)

r_k <- p_k / q_k
c <- max(r_k)
cq_k <- c * q_k

df_ex2 <- data.frame(k = k_valores, p_k = p_k, cq_k = cq_k)
ggplot(df_ex2, aes(x = k)) +
  geom_segment(aes(x = k, xend = k, y = 0, yend = cq_k), color = "black") +
  geom_point(aes(y = cq_k), color = "green", size = 3) +
  geom_point(aes(y = p_k), color = "red", size = 3) +
  labs(x = "", y = "") +
  ylim(0, 0.6) +
  scale_x_continuous(breaks = 1:(10*lambda)) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("p(x)" = "red", "cq(x)" = "green"), name = "") +
  guides(color = guide_legend(override.aes = list(shape = 16))) +
  geom_point(aes(y = p_k, color = "p(x)"), size = 3) +
  geom_point(aes(y = cq_k, color = "cq(x)"), size = 3) 


#hist(r_k)
#stripchart(r_k, method = "jitter", pch =19)
#plot(r_k)

n_amostras <- 5000

names(p_k) <- k_valores
names(q_k) <- k_valores


# para calcular quantas amostras foram rejeitadas, e avaliar a eficiencia depois
num_de_amostras_testadas_total <- 0
numero_de_amostras <- 10000
amostras_aceitas <- c()
while (length(amostras_aceitas) < numero_de_amostras) {
  #tirar uma amostra aleatoria em k_valores, nesse caso de 1 a 10*lambda
  y <- rgeom(1, prob = p) + 1

  #gerar uma amostra de uniforme(0, 1)
  u <- runif(1)

  #o valor para decidir se aceitamos a amostra gerada ou não
  acceptance_ratio <-  p_k[as.character(y)] / (c * q_k[as.character(y)])

  #incrementa 1 para saber quantas vezes esse loop rodou
  num_de_amostras_testadas_total <- num_de_amostras_testadas_total + 1

  if (u <= acceptance_ratio) {
    # Se aceito, adicione o valor ao vetor de amostras
    amostras_aceitas <- c(amostras_aceitas, y)
  }
}
hist(amostras_aceitas, probability = TRUE)
#stripchart(amostras_aceitas, method = "jitter", pch =19)

#tabela de frequencia
tabela_frequencia <- table(factor(amostras_aceitas, levels = k_valores))
frequencia_relativa <- prop.table(tabela_frequencia)

#data.frame final para exibir os resultados
resultados_df <- data.frame(
  `Valor (k)` = as.integer(names(tabela_frequencia)),
  `Frequência Absoluta` = as.integer(frequencia_relativa),
  `Frequência Relativa (Observada)` = as.numeric(frequencia_relativa),
  `Probabilidade Teórica (p_k)` = p_k,
  check.names = FALSE
)

ggplot(resultados_df, aes(x = `Valor (k)`)) +
  geom_segment(aes(x = `Valor (k)`, xend = `Valor (k)`, y = 0, yend = `Frequência Relativa (Observada)`), color = "black") +
  geom_point(aes(y = `Frequência Relativa (Observada)`), color = "green", size = 3) +
  geom_point(aes(y = `Probabilidade Teórica (p_k)`), color = "red", size = 3) +
  labs(x = "", y = "") +
  ylim(0, 0.20) +
  scale_x_continuous(breaks = 1:40) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Probabilidade Teórica (p_k)" = "red", "`Frequência Relativa (Observada)`" = "green"), name = "") +
  guides(color = guide_legend(override.aes = list(shape = 16))) +
  geom_point(aes(y = `Probabilidade Teórica (p_k)`, color = "Probabilidade Teórica (p_k)"), size = 3) +
  geom_point(aes(y = `Frequência Relativa (Observada)`, color = "`Frequência Relativa (Observada)`"), size = 3)
