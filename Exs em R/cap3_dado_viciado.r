faces <- 1:6

# Probabilidades associadas às faces do dado
probabilidades <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.25)

n_lancamentos <- 10000

resultados <- sample(faces, size = n_lancamentos, replace = TRUE, prob = probabilidades)
frequencias <- table(resultados) / n_lancamentos
cat("Frequências de cada face após", n_lancamentos, "lançamentos:\n")
for (face in faces) {
  cat("Face", face, ":", frequencias[as.character(face)], "vezes\n")
}
library(ggplot2)

dados <- data.frame(faces = as.factor(faces), frequencias = as.numeric(frequencias))

ggplot(dados, aes(x = faces, y = frequencias)) +
  geom_bar(stat = "identity", fill = "lightcoral", color = "black") +
  ggtitle(paste0("Simulação de Lançamentos de um Dado Viciado\n", n_lancamentos, " lançamentos")) +
  xlab("Face do Dado") +
  ylab("Frequência de Ocorrência") +
  theme_minimal() +
  geom_text(aes(label = round(frequencias, 4)), vjust = -0.5) +
  theme(panel.grid.major = element_line(color = "grey80"))

# Gráfico das probabilidades ajustadas
dados_probabilidades <- data.frame(faces = as.factor(faces), probabilidades = probabilidades)
ggplot(dados_probabilidades, aes(x = faces, y = probabilidades)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  ggtitle("Probabilidades Ajustadas para o Dado Viciado") +
  xlab("Face do Dado") +
  ylab("Probabilidade") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey80"))

#alt, usando uniforme
# Definindo as faces do dado e as probabilidades associadas (não uniformes)
faces <- 1:6
# Probabilidades associadas às faces do dado
probabilidades <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.25)

# Função para gerar uma amostra baseada em intervalos de probabilidades
gerar_amostra_por_intervalos <- function(probabilidades, faces) {
  u <- runif(1)  # Gerando um número aleatório uniforme
  limite_inferior <- 0  # Limite inferior do intervalo
  # Percorrendo as probabilidades e verificando em qual intervalo o número cai
  for (i in seq_along(probabilidades)) {
    # Definindo o limite superior do intervalo
    limite_superior <- limite_inferior + probabilidades[i]
    if (limite_inferior <= u && u < limite_superior) {
      return(faces[i])  # Retorna a face correspondente ao intervalo
    }
    # Atualiza o limite inferior para o próximo intervalo
    limite_inferior <- limite_superior
  }
}

# Simulando lançamentos do dado viciado utilizando a verificação dos intervalos
n_lancamentos <- 10000
set.seed(123)  # Definindo seed para reprodutibilidade
resultados <- replicate(n_lancamentos, gerar_amostra_por_intervalos(probabilidades, faces))

# Contando as frequências de cada face
frequencias <- sapply(faces, function(face) sum(resultados == face) / n_lancamentos)

# Exibindo os resultados da simulação
cat("Frequências de cada face após", n_lancamentos, "lançamentos:\n")
for (i in seq_along(faces)) {
  cat("Face", faces[i], ":", frequencias[i], "(em %)\n")
}
# Gráfico das frequências obtidas
dados_frequencias <- data.frame(faces = as.factor(faces), frequencias = frequencias)
ggplot(dados_frequencias, aes(x = faces, y = frequencias)) +
  geom_bar(stat = "identity", fill = "lightcoral", color = "black") +
  ggtitle(paste0("Simulação de Lançamentos de um Dado Viciado\n", n_lancamentos, " lançamentos")) +
  xlab("Face do Dado") +
  ylab("Frequência de Ocorrência") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey80"))
