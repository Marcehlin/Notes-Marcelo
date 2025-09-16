library(ggplot2)

j_values <- 1:15
p_j <- log(j_values + 1) / sum(log(1 + 1:15))

q_j <- rep(1 / 15, length(p_j))
c <- max(p_j / q_j)
cq_j <- c * q_j

df <- data.frame(j = j_values, p_j = p_j, cq_j = cq_j)

ggplot(df, aes(x = j)) +
  geom_segment(aes(x = j, xend = j, y = 0, yend = cq_j), color = "black") +
  geom_point(aes(y = cq_j), color = "green", size = 3) +
  geom_point(aes(y = p_j), color = "red", size = 3) +
  labs(x = "", y = "") +
  ylim(0, 0.2) +
  scale_x_continuous(breaks = 1:15) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("p(x)" = "red", "cq(x)" = "green"), name = "") +
  guides(color = guide_legend(override.aes = list(shape = 16))) +
  geom_point(aes(y = p_j, color = "p(x)"), size = 3) +
  geom_point(aes(y = cq_j, color = "cq(x)"), size = 3) 

# número de amostras que quero
n_samples <- 10000
# vetor para armazenar as amostras aceitas
accepted_samples <- c()

# implmentando o método da rejeição ---
# nomeando o vetor p_j com os valores correspondentes de j
names(p_j) <- j_values
# distribuição uniforme q_j, o valor é sempre 1/15
q_val <- 1 / 15

num_de_amostras_testadas_total <- 0 # para calcular quantas amostras foram rejeitadas

# Loop até que o número desejado de amostras seja gerado
while (length(accepted_samples) < n_samples) {
  # 1. Gere um valor Y a partir da distribuição proposta q_j
  # Como q_j é uniforme, usamos a função sample() para escolher aleatoriamente um valor de j_values
  y <- sample(j_values, 1)
  
  # 2. Gere U ~ Uniform(0,1)
  u <- runif(1)
  
  # 3. Teste de aceitação
  # Acessamos p(y) usando o nome do vetor (convertendo y para caractere para busca)
  acceptance_ratio <- p_j[as.character(y)] / (c * q_val)

  num_de_amostras_testadas_total <- num_de_amostras_testadas_total + 1

  if (u <= acceptance_ratio) {
    # Se aceito, adicione o valor ao vetor de amostras
    accepted_samples <- c(accepted_samples, y)
  }
}

# Criação da tabela de frequência
freq_table <- table(factor(accepted_samples, levels = j_values))
relative_freq <- prop.table(freq_table)

# Montamos um data.frame final para exibir os resultados
results_df <- data.frame(
  `Valor (j)` = as.integer(names(freq_table)),
  `Frequência Absoluta` = as.integer(freq_table),
  `Frequência Relativa (Observada)` = as.numeric(relative_freq),
  `Probabilidade Teórica (p_j)` = p_j,
  check.names = FALSE
)
print(results_df, row.names = FALSE)

ggplot(results_df, aes(x = `Valor (j)`)) +
  geom_segment(aes(x = `Valor (j)`, xend = `Valor (j)`, y = 0, yend = `Frequência Relativa (Observada)`), color = "black") +
  geom_point(aes(y = `Frequência Relativa (Observada)`), color = "green", size = 3) +
  geom_point(aes(y = `Probabilidade Teórica (p_j)`), color = "red", size = 3) +
  labs(x = "", y = "") +
  ylim(0, 0.15) +
  scale_x_continuous(breaks = 1:15) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Probabilidade Teórica (p_j)" = "red", "`Frequência Relativa (Observada)`" = "green"), name = "") +
  guides(color = guide_legend(override.aes = list(shape = 16))) +
  geom_point(aes(y = `Probabilidade Teórica (p_j)`, color = "Probabilidade Teórica (p_j)"), size = 3) +
  geom_point(aes(y = `Frequência Relativa (Observada)`, color = "`Frequência Relativa (Observada)`"), size = 3) 

print(n_samples / num_de_amostras_testadas_total)
print(1 / c)
