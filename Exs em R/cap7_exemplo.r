library(ggplot2)

# Valores de j e probabilidades p_j e q_j
j_values <- 1:10
p_j <- c(0.11, 0.12, 0.09, 0.08, 0.12, 0.10, 0.09, 0.09, 0.10, 0.10)
q_j <- rep(1 / 10, length(p_j))  # valores de q_j são iguais, distribuição uniforme

#a constante para ajustar q_j
c <- max(p_j / q_j)
cq_j <- c * q_j

df <- data.frame(j = j_values, p_j = p_j, cq_j = cq_j)

ggplot(df, aes(x = j)) +
  geom_segment(aes(x = j, xend = j, y = 0, yend = cq_j), color = "black") +
  geom_point(aes(y = cq_j), color = "green", size = 3) +
  geom_point(aes(y = p_j), color = "red", size = 3) +
  labs(x = "", y = "") +
  ylim(0, 0.2) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("p(x)" = "red", "cq(x)" = "green"), name = "") +
  guides(color = guide_legend(override.aes = list(shape = 16))) +
  geom_point(aes(y = p_j, color = "p(x)"), size = 3) +
  geom_point(aes(y = cq_j, color = "cq(x)"), size = 3) 


# Número de amostras desejadas
n_samples <- 10000
# Vetor para armazenar as amostras aceitas
accepted_samples <- c()

# --- Implementação do Método da Rejeição ---
# Para facilitar a busca, nomeamos o vetor p_j com os valores correspondentes de j
names(p_j) <- j_values
# Para a distribuição uniforme q_j, o valor é sempre 1/10
q_val <- 1 / 10

# Loop até que o número desejado de amostras seja gerado
while (length(accepted_samples) < n_samples) {
  # 1. Gere um valor Y a partir da distribuição proposta q_j
  # Como q_j é uniforme, usamos a função sample() para escolher um valor de j_values
  y <- sample(j_values, 1)
  
  # 2. Gere U ~ Uniform(0,1)
  u <- runif(1)
  
  # 3. Teste de aceitação
  # Acessamos p(y) usando o nome do vetor (convertendo y para caractere para busca)
  acceptance_ratio <- p_j[as.character(y)] / (c * q_val)
  
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
