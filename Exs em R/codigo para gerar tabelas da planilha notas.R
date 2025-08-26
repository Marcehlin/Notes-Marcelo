library(readxl)
GA_e_Algelin <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", 
                        sheet = "G.A e Alge.Lin", na = "N/A")
View(GA_e_Algelin)

boxplot(GA_e_Algelin[["AlgeLin Média"]],
        main = "(R)Distribuição das Médias Finais em Algebra Linear",
        ylab = "Média",
        ylim = c(0, 10),
        col = "skyblue")

boxplot(`AlgeLin Média` ~ GA, data = GA_e_Algelin,
        main = "(R)Médias Finais em Algebra Linear, separando em 2 grupos",
        ylab = "Média",
        xlab = "Professor de GA",
        col = "skyblue",
        ylim = c(0, 10),
        names = c("Fábio", "Olímpio")) # Renomeia categorias

stripchart(GA_e_Algelin$`AlgeLin Média`,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "blue",
           vertical = TRUE,
           ylim = c(0, 10),
           main = "(R)Distribuição das Médias em AlgeLin",
           ylab = "Média")

stripchart(`AlgeLin Média` ~ GA, data = GA_e_Algelin,
           method = "jitter",
           pch = 19,
           col = c("blue", "skyblue"),
           ylim = c(0, 10),
           vertical = TRUE,      # garante que fique no sentido correto
           main = "(R)Média em AlgeLin por Professor de GA ",
           ylab = "Média")

AlgeLin_Bruta <- c(0.5, 1, 7.1, 4, 1.9, 6.8, 1.3, 5.8, 4.2, 4, 5.3, 6.3, 7.2, 7.9, 8, 3.4, 8.8, 7.1, 6.1, 7.8, 6.9, 5.8, 9.1, 6.3, 4.2, 2.3, 2, 6.4, 5.8, 2.4, 7.7, 3.7, 5.3, 7.1, 0.7)
boxplot(AlgeLin_Bruta,
        main = "(B)Distribuição das Médias Finais em Algebra Linear",
        ylab = "Média",
        ylim = c(0, 10),
        col = "skyblue")

stripchart(AlgeLin_Bruta,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "skyblue",
           vertical = TRUE,
           ylim = c(0, 10),
           main = "(B)Distribuição das Médias em AlgeLin",
           ylab = "Média")

# Teste t de médias (H0: não há diferença significativa)
t.test(`AlgeLin Média` ~ GA,
       data = GA_e_Algelin,
       var.equal = TRUE)  # TRUE = assume variâncias iguais

CDI <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", 
                        sheet = "CDI", na = "N/A")

boxplot(CDI$`CDI Média`,
           method = "jitter",
           ylim = c(0, 10),
           pch = 19,            
           col = "blue",
           vertical = TRUE,
           main = "(R)Distribuição das Médias em CDI",
           ylab = "Média")

stripchart(CDI$`CDI Média`,
           method = "jitter",
           ylim = c(0, 10),
           pch = 19,    
           col = "blue",
           vertical = TRUE,
           main = "(R)Distribuição das Médias em CDI",
           ylab = "Média")

CDI_Bruto <-c(4.3, 4.1, 4.0, 5.1, 10, 9.1, 8.8, 4.6, 6.7, 6.3, 8, 5, 10, 1.8, 8.5, 10, 6.1, 4, 10, 8.5, 9.5, 9, 6, 9.1, 6, 8, 7.9, 5.3, 9.4, 10, 4.1, 6.5, 8.8, 5.6, 7.1, 8.3, 8.0, 6.3, 7.1, 3.1, 6.9, 5.6, 6.3)
boxplot(CDI_Bruto,
        method = "jitter",
        ylim = c(0, 10),
        pch = 19,           
        col = "blue",
        vertical = TRUE,
        main = "(B)Distribuição das Médias em CDI",
        ylab = "Média")

stripchart(CDI_Bruto,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "blue",
           ylim = c(0, 10),
           vertical = TRUE,
           main = "(B)Distribuição das Médias em CDI",
           ylab = "Média")

Fund_Prob <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", 
                  sheet = "Fund.Prob", na = "N/A")

boxplot(Fund_Prob$`Fund.Prob Média`,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "springgreen",
           ylim = c(0, 10),
           vertical = TRUE,
           main = "(R)Distribuição das Médias em Fund.Prob (Turma Salasar)",
           ylab = "Média")

stripchart(Fund_Prob$`Fund.Prob Média`,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "springgreen",
           ylim = c(0, 10),
           vertical = TRUE,
           main = "(R)Distribuição das Médias em Fund.Prob (Turma Salasar)",
           ylab = "Média")
FundProb_Bruta <- c(0, 9.1, 1.4, 6, 0, 2.1, 2.1, 6, 3.3, 1.2, 2.1, 7.9, 2.8, 6.5, 9, 0, 0.5, 10, 8.2, 9.8, 0, 4.3, 4.1, 2.4, 2, 3.4, 3.9, 0, 6.4, 2.2, 7, 0.8, 5.2, 3, 9.4, 2.4, 7.5)
boxplot(FundProb_Bruta,
        method = "jitter",    # adiciona ruído para separar pontos
        pch = 19,             # formato do ponto
        col = "springgreen",
        ylim = c(0, 10),
        vertical = TRUE,
        main = "(B)Distribuição das Médias em Fund.Prob (Turma Salasar)",
        ylab = "Média")
stripchart(FundProb_Bruta,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "springgreen",
           ylim = c(0, 10),
           vertical = TRUE,
           main = "(B)Distribuição das Médias em Fund.Prob (Turma Salasar)",
           ylab = "Média")
PA1_e_PA2 <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", 
                           sheet = "PA1 e PA2", na = "N/A")

boxplot(PA1_e_PA2[["PA2 MEDIA"]],
        main = "(R)Distribuição das Médias Finais em PA2",
        ylab = "Média",
        ylim = c(0, 100),
        col = "aquamarine")

boxplot(`PA2 MEDIA` ~ PROF, data = PA1_e_PA2,
           method = "jitter",
           pch = 19,
           ylim = c(0, 100),
           col = c("aquamarine", "mediumpurple1"),
           vertical = TRUE,      # garante que fique no sentido correto
           main = "(R)Médias Finais em PA2, separando em 2 grupos",
           ylab = "Média")

stripchart(PA1_e_PA2[["PA2 MEDIA"]],
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "aquamarine",
           vertical = TRUE,
           ylim = c(0, 100),
           main = "(R)Distribuição das Médias Finais em PA2",
           ylab = "Média")

stripchart(`PA2 MEDIA` ~ PROF, data = PA1_e_PA2,
           method = "jitter",
           pch = 19,
           ylim = c(0, 100),
           col = c("aquamarine", "mediumpurple1"),
           vertical = TRUE,      # garante que fique no sentido correto
           main = "(R)Médias Finais em PA2, separando em 2 grupos",
           ylab = "Média")

# Teste t de médias (H0: não há diferença significativa)
t.test(`PA2 MEDIA` ~ PROF,
       data = PA1_e_PA2,
       var.equal = TRUE)  # TRUE = assume variâncias iguais

PA2_Bruta <- c(82.67, 72.33, 85.33, 96.67, 2.33, 63.33, 86.67, 81.33, 51.00, 80, 6.67, 28,33, 77, 74.33, 84.33, 58.67, 63.67, 73, 65, 97.33, 83.33, 95, 68.33, 85, 11, 94, 2.33, 88.33)
boxplot(PA2_Bruta,
        main = "(B)Distribuição das Médias Finais em PA2",
        ylab = "Média",
        ylim = c(0, 100),
        col = "mediumseagreen")

stripchart(PA2_Bruta,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "mediumseagreen",
           vertical = TRUE,
           ylim = c(0, 100),
           main = "(B)Distribuição das Médias Finais em PA2",
           ylab = "Média")

Aded_Fogo <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", 
                        sheet = "Aded", na = "N/A")
boxplot(Aded_Fogo$`Média`,
        method = "jitter",    # adiciona ruído para separar pontos
        pch = 19,             # formato do ponto
        col = "tomato",
        ylim = c(0, 10),
        vertical = TRUE,
        main = "(R)Distribuição das Médias em Fund.Prob (Turma Fogo)",
        ylab = "Média")

stripchart(Aded_Fogo$`Média`,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "tomato",
           ylim = c(0, 10),
           vertical = TRUE,
           main = "(R)Distribuição das Médias em Aded (Turma Fogo)",
           ylab = "Média")
AdedFogo_Bruta <- c(8.7, 8, 0.8, 7.2, 2.1, 8.2, 7.1, 8.5, 7, 5.4, 7.4, 5.7, 7, 8.3, 79, 7, 8.5, 8, 6.8, 8.4, 8.6, 7.6, 7.8, 5.4)
boxplot(AdedFogo_Bruta,
        method = "jitter",    # adiciona ruído para separar pontos
        pch = 19,             # formato do ponto
        col = "tomato",
        ylim = c(0, 10),
        vertical = TRUE,
        main = "(B)Distribuição das Médias em Fund.Prob (Turma Fogo)",
        ylab = "Média")

stripchart(AdedFogo_Bruta,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "tomato",
           ylim = c(0, 10),
           vertical = TRUE,
           main = "(B)Distribuição das Médias em Aded (Turma Fogo)",
           ylab = "Média")
Probs <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", 
                        sheet = "Prob1 e Prob2", na = "N/A")
boxplot(Probs$`Prob1 Média Final`,
        method = "jitter",    # adiciona ruído para separar pontos
        pch = 19,             # formato do ponto
        col = "steelblue",
        ylim = c(0, 10),
        vertical = TRUE,
        main = "(R)Distribuição das Médias em Prob1 (Turma Diniz)",
        ylab = "Média")
stripchart(Probs$`Prob1 Média Final`,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "steelblue",
           ylim = c(0, 10),
           vertical = TRUE,
           main = "(R)Distribuição das Médias em Prob1 (Turma Diniz)",
           ylab = "Média")

boxplot(Probs$`Prob2 Média Final`,
        method = "jitter",    # adiciona ruído para separar pontos
        pch = 19,             # formato do ponto
        col = "steelblue2",
        ylim = c(0, 10),
        vertical = TRUE,
        main = "(R)Distribuição das Médias em Prob2 (Turma Andressa)",
        ylab = "Média")
stripchart(Probs$`Prob2 Média Final`,
        method = "jitter",    # adiciona ruído para separar pontos
        pch = 19,             # formato do ponto
        col = "steelblue2",
        ylim = c(0, 10),
        vertical = TRUE,
        main = "(R)Distribuição das Médias em Prob2 (Turma Andressa)",
        ylab = "Média")

Prob1_Bruta <- c(1, 0, 0.5, 3.2, 0, 5, 8.5, 0, 0, 0, 0, 8.4, 8.4, 8.2, 9.4, 8.6, 6, 3.8, 8.4, 5.3, 7.8, 8.4, 5.8, 2.7, 6.2, 5.8, 1.9, 3.4, 3.6, 3.5)
boxplot(Prob1_Bruta,
        method = "jitter",    # adiciona ruído para separar pontos
        pch = 19,             # formato do ponto
        col = "steelblue",
        ylim = c(0, 10),
        vertical = TRUE,
        main = "(B)Distribuição das Médias em Prob1 (Turma Diniz)",
        ylab = "Média")
stripchart(Prob1_Bruta,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "steelblue",
           ylim = c(0, 10),
           vertical = TRUE,
           main = "(B)Distribuição das Médias em Prob1 (Turma Diniz)",
           ylab = "Média")

Prob2_Bruta <- c(8.5, 0.4, 8.5, 6, 9.9, 0.1, 0.2, 8.9, 0.2, 7.6, 2.8, 0.8, 6.5, 6.7, 8.6, 0.7, 0.6, 8.1, 9.1, 9, 0.1, 3.5, 6.2, 0.4)
boxplot(Prob2_Bruta,
        method = "jitter",    # adiciona ruído para separar pontos
        pch = 19,             # formato do ponto
        col = "steelblue2",
        ylim = c(0, 10),
        vertical = TRUE,
        main = "(B)Distribuição das Médias em Prob2 (Turma Andressa)",
        ylab = "Média")
stripchart(Prob2_Bruta,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "steelblue2",
           ylim = c(0, 10),
           vertical = TRUE,
           main = "(B)Distribuição das Médias em Prob2 (Turma Andressa)",)

SeriesEDO <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", 
                    sheet = "Séries e EDOs", na = "N/A")
boxplot(SeriesEDO$`Séries Média Final`,
        method = "jitter",    # adiciona ruído para separar pontos
        pch = 19,             # formato do ponto
        col = "palevioletred2",
        ylim = c(0, 10),
        vertical = TRUE,
        main = "(R)Médias em Séries e EDOs (Turma Talpo)",
        ylab = "Média")
stripchart(SeriesEDO$`Séries Média Final`,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "palevioletred2",
           ylim = c(0, 10),
           vertical = TRUE,
           main = "(R)Médias em Séries e EDOs (Turma Talpo)",
           ylab = "Média")

Séries_EDOs_Bruta <- c(6, 6, 6, 6, 6.2, 1.7, 6.8, 0.7, 3, 0.5, 5, 0.2, 0.5, 3, 1.5, 1.5, 5, 0, 0, 0.3, 2.5, 3.2, 1, 6.7, 0.7, 2.2, 1, 6, 6, 0, 2.3, 0, 8.5, 3.2, 6.7, 9, 4, 6, 3.5, 8)
boxplot(Séries_EDOs_Bruta,
        method = "jitter",    # adiciona ruído para separar pontos
        pch = 19,             # formato do ponto
        col = "palevioletred2",
        ylim = c(0, 10),
        vertical = TRUE,
        main = "(B)Médias em Séries e EDOs (Turma Talpo)",
        ylab = "Média")
stripchart(Séries_EDOs_Bruta,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "palevioletred2",
           ylim = c(0, 10),
           vertical = TRUE,
           main = "(B)Médias em Séries e EDOs (Turma Talpo)",
           ylab = "Média")

Int_Inf_Gus <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", 
                        sheet = "Int.Inf", na = "N/A")
boxplot(Int_Inf_Gus$`Int.Inf MF`,
        method = "jitter",    # adiciona ruído para separar pontos
        pch = 19,             # formato do ponto
        col = "maroon",
        ylim = c(0, 10),
        vertical = TRUE,
        main = "(R)Médias em Int à Inferêncaiia (Turma Gustavo)",
        ylab = "Média")
stripchart(Int_Inf_Gus$`Int.Inf MF`,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "maroon",
           ylim = c(0, 10),
           vertical = TRUE,
           main = "(R)Médias em Int à Inferêncaiia (Turma Gustavo)",
           ylab = "Média")

Int_Inf_Bruta <- c(6.7, 2.5, 5, 6.7, 5.3, 6.5, 5.8, 6.9, 5.1, 0.7, 8.6, 2.5, 2.3, 6.4, 6, 8.6, 7.8, 7.7, 7, 8.3, 7.7, 3.1, 7.7, 6.4, 8.7, 5.7, 7, 5.6)
boxplot(Int_Inf_Bruta,
        method = "jitter",    # adiciona ruído para separar pontos
        pch = 19,             # formato do ponto
        col = "maroon",
        ylim = c(0, 10),
        vertical = TRUE,
        main = "(B)Médias em Int à Inferêncaiia (Turma Gustavo)",
        ylab = "Média")
stripchart(Int_Inf_Bruta,
           method = "jitter",    # adiciona ruído para separar pontos
           pch = 19,             # formato do ponto
           col = "maroon",
           ylim = c(0, 10),
           vertical = TRUE,
           main = "(B)Médias em Int à Inferêncaiia (Turma Gustavo)",
           ylab = "Média")