qtd_pontos <- 10000
#pontos no primeiro quadrante
coor_x <- runif(qtd_pontos) #entre 0 e 1
coor_y <- runif(qtd_pontos) #tambem entre 0 e 1

#calcular a distância do ponto ao centro
distancia <- sqrt(coor_x^2 + coor_y^2)
#se a distancia é menor ou igual que 1, o ponto tá dentro da circunferência
#e contamos quantos pontos tá dentro da circunferencia
dentro_do_circulo <- distancia <= 1
pi_estimado <- 4 * sum(dentro_do_circulo) / qtd_pontos
cat("Valor estimado de π:", pi_estimado, "\n")

library(ggplot2)

dados <- data.frame(x = coor_x, y = coor_y, dentro_do_circulo = dentro_do_circulo)

ggplot(dados, aes(x = coor_x, y = coor_y, color = dentro_do_circulo)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("red", "blue")) +
  ggtitle(paste0("Estimativa de π usando Monte Carlo\nValor estimado: ", round(pi_estimado, 5))) +
  theme_minimal() +
  coord_equal() +
  labs(x = "x", y = "y")

#alt
qtd_pontos <- 10000
#usando 4 quadrantes
x <- runif(qtd_pontos, -1, 1) #entre -1 e 1
y <- runif(qtd_pontos, -1, 1) #tambem entre -1 e 1

#calcular a distância do ponto ao centro
distancia <- sqrt(x^2 + y^2)
#se a distancia é menor ou igual que 1, o ponto tá dentro da circunferência
#e contamos quantos pontos tá dentro da circunferencia
dentro_circulo <- distancia <= 1
#ainda temos que multiplicar por 4 porque ????
alt_pi_estimado <- 4 * sum(dentro_circulo) / qtd_pontos
cat("Valor estimado de π:", alt_pi_estimado, "\n")

alt_dados <- data.frame(x = x, y = y, dentro_circulo = dentro_circulo)

ggplot(alt_dados, aes(x = x, y = y, color = dentro_circulo)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("red", "blue")) +
  ggtitle(paste0("Estimativa de π usando Monte Carlo\nValor estimado: ", round(pi_estimado, 5))) +
  theme_minimal() +
  coord_equal() +
  labs(x = "x", y = "y")
