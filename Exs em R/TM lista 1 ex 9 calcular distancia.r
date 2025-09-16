vetor1 <- c(168, 70, 1)
vetor2 <- c(173, 92, 0)
vetor3 <- c(167, 82, 0)
vetor4 <- c(164, 60, 1)
vetor5 <- c(176, 105, 0)
vetor_media <- c(169.6, 81.8, 0.4)
#juntar os vetores em uma matriz
mat <- rbind(vetor1, vetor2, vetor3, vetor4, vetor5, vetor_media)

#calcula matriz de distancias
dist_matrix <- as.matrix(dist(mat, method = "euclidean"))
dist_matrix