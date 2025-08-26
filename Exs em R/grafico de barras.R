library(readxl)
cores <-c("green","yellow","red")
nomes <- c("Aprovado", "Recuperação","Reprovado")

CDI <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", sheet = "CDI", 
                  na = "N/A")
View(CDI)
rnotas_CDI <- na.omit(CDI$`CDI Média`)
rCDI_apro <- sum(rnotas_CDI>=6)
rCDI_rec <- sum(rnotas_CDI>=5 & rnotas_CDI <6)
rCDI_rep <- sum(rnotas_CDI<5)
rCDI_valores <-c(rCDI_apro, rCDI_rec, rCDI_rep)
barplot(rCDI_valores,
        names.arg = nomes,
        col = cores,
        main = "(R)Situação da turma em CDI",
        ylab = "Quantidade",
        ylim = c(0,30))

CDI_Bruto <- c(4.3, 4.1, 4.0, 5.1, 10, 9.1, 8.8, 4.6, 6.7, 6.3, 8, 5, 10, 1.8, 8.5, 10, 6.1, 4, 10, 8.5, 9.5, 9, 6,9.1, 6, 8, 7.9, 5.3, 9.4, 10, 4.1, 6.5, 8.8, 5.6, 7.1, 8.3, 8.0, 6.3, 7.1, 3.1, 6.9, 5.6, 6.3)
bCDI_apro <- sum(CDI_Bruto>=6)
bCDI_rec <- sum(CDI_Bruto>=5 & CDI_Bruto <6)
bCDI_rep <- sum(CDI_Bruto<5)
bCDI_valores <-c(bCDI_apro, bCDI_rec, bCDI_rep)
barplot(bCDI_valores,
        names.arg = nomes,
        col = cores,
        main = "(B)Situação da turma em CDI",
        ylab = "Quantidade",
        ylim = c(0,30))
        

Fund_Prob <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", sheet = "Fund.Prob", 
                  na = "N/A")        
View(Fund_Prob)
rnotas_Fund_Prob <- na.omit(Fund_Prob$`Fund.Prob Média`)
rFund_Prob_apro <- sum(rnotas_Fund_Prob>=6)
rFund_Prob_rec <- sum(rnotas_Fund_Prob>=5 & rnotas_Fund_Prob <6)
rFund_Prob_rep <- sum(rnotas_Fund_Prob<5)
rFund_Prob_valores <-c(rFund_Prob_apro, rFund_Prob_rec, rFund_Prob_rep)
barplot(rFund_Prob_valores,
        names.arg = nomes,
        col = cores,
        main = "(R)Situação da turma em Fund.Prob(Turma Salasar)",
        ylab = "Quantidade",
        ylim = c(0,16))

Fund_Prob_Bruta <- c(0, 9.1, 1.4, 6, 0, 2.1, 2.1, 6, 3.3, 1.2, 2.1, 7.9, 2.8, 6.5, 9, 0, 0.5, 10, 8.2, 9.8, 0, 4.3, 4.1, 2.4, 2, 3.4, 3.9, 0, 6.4, 2.2, 7, 0.8, 5.2, 3, 9.4, 2.4, 7.5)
bFund_Prob_apro <- sum(Fund_Prob_Bruta>=6)
bFund_Prob_rec <- sum(Fund_Prob_Bruta>=5 & Fund_Prob_Bruta <6)
bFund_Prob_rep <- sum(Fund_Prob_Bruta<5)
bFund_Prob_valores <-c(bFund_Prob_apro, bFund_Prob_rec, bFund_Prob_rep)
barplot(bFund_Prob_valores,
        names.arg = nomes,
        col = cores,
        main = "(B)Situação da turma em Fund.Prob(Turma Salasar)",
        ylab = "Quantidade",
        ylim = c(0,25))

AlgeLin <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", sheet = "G.A e Alge.Lin", 
                        na = "N/A")        
View(AlgeLin)
rnotas_AlgeLin <- na.omit(AlgeLin$`AlgeLin Média`)
rAlgeLin_apro <- sum(rnotas_AlgeLin>=6)
rAlgeLin_rec <- sum(rnotas_AlgeLin>=5 & rnotas_AlgeLin <6)
rAlgeLin_rep <- sum(rnotas_AlgeLin<5)
rAlgeLin_valores <-c(rAlgeLin_apro, rAlgeLin_rec, rAlgeLin_rep)
barplot(rAlgeLin_valores,
        names.arg = nomes,
        col = cores,
        main = "(R)Situação da turma em Álgebra Linear",
        ylab = "Quantidade",
        ylim = c(0,15))

AlgeLin_Bruta <- c(0.5, 1, 7.1, 4, 1.9, 6.8, 1.3, 5.8, 4.2, 4, 5.3, 6.3, 7.2, 7.9, 8, 3.4, 8.8, 7.1, 6.1, 7.8, 6.9, 5.8, 9.1, 6.3, 4.2, 2.3, 2, 6.4, 5.8, 2.4, 7.7, 3.7, 5.3, 7.1, 0.7)
bAlgeLin_apro <- sum(AlgeLin_Bruta>=6)
bAlgeLin_rec <- sum(AlgeLin_Bruta>=5 & AlgeLin_Bruta <6)
bAlgeLin_rep <- sum(AlgeLin_Bruta<5)
bAlgeLin_valores <-c(bAlgeLin_apro, bAlgeLin_rec, bAlgeLin_rep)
barplot(bAlgeLin_valores,
        names.arg = nomes,
        col = cores,
        main = "(B)Situação da turma em Álgebra Linear",
        ylab = "Quantidade",
        ylim = c(0,20))


PA2 <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", sheet = "PA1 e PA2", 
                      na = "N/A")        
View(PA2)
rnotas_PA2 <- na.omit(PA2$`PA2 MEDIA`)
rPA2_apro <- sum(rnotas_PA2>=60)
rPA2_rec <- sum(rnotas_PA2>=50 & rnotas_PA2 <60)
rPA2_rep <- sum(rnotas_PA2<50)
rPA2_valores <-c(rPA2_apro, rPA2_rec, rPA2_rep)
barplot(rPA2_valores,
        names.arg = nomes,
        col = cores,
        main = "(R)Situação da turma em PA2",
        ylab = "Quantidade",
        ylim = c(0,25))


PA2_Bruta <- c(82.67, 72.33, 85.33, 96.67, 2.33, 63.33, 86.67, 81.33, 51.00, 80, 6,67, 28,33, 77, 74.33, 84.33, 58.67, 63.67, 73, 65, 97.33, 83.33, 95, 68.33, 85, 11, 94, 2.33, 88.33 )
bPA2_apro <- sum(PA2_Bruta>=60)
bPA2_rec <- sum(PA2_Bruta>=50 & PA2_Bruta <60)
bPA2_rep <- sum(PA2_Bruta<50)
bPA2_valores <-c(bPA2_apro, bPA2_rec, bPA2_rep)
barplot(bPA2_valores,
        names.arg = nomes,
        col = cores,
        main = "(B)Situação da turma em PA2",
        ylab = "Quantidade",
        ylim = c(0,25))

ADED <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", sheet = "Aded", 
                  na = "N/A")        
View(ADED)
rnotas_ADED <- na.omit(ADED$`Média`)
rADED_apro <- sum(rnotas_ADED>=6)
rADED_rec <- sum(rnotas_ADED>=5 & rnotas_ADED <6)
rADED_rep <- sum(rnotas_ADED<5)
rADED_valores <-c(rADED_apro, rADED_rec, rADED_rep)
barplot(rADED_valores,
        names.arg = nomes,
        col = cores,
        main = "(R)Situação da turma em Aded(Turma Fogo)",
        ylab = "Quantidade",
        ylim = c(0,15))

AdedFogo_Bruta <- c(8.7, 8, 0.8, 7.2, 2.1, 8.2, 7.1, 8.5, 7, 5.4, 7.4, 5.7, 7, 8.3, 79, 7, 8.5, 8, 6.8, 8.4, 8.6, 7.6, 7.8, 5.4)
bAdedFogo_apro <- sum(AdedFogo_Bruta>=6)
bAdedFogo_rec <- sum(AdedFogo_Bruta>=5 & AdedFogo_Bruta <6)
bAdedFogo_rep <- sum(AdedFogo_Bruta<5)
bAdedFogo_valores <-c(bAdedFogo_apro, bAdedFogo_rec, bAdedFogo_rep)
barplot(bAdedFogo_valores,
        names.arg = nomes,
        col = cores,
        main = "(B)Situação da turma em Aded(Turma Fogo)",
        ylab = "Quantidade",
        ylim = c(0,20))

Probs <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", sheet = "Prob1 e Prob2", 
                   na = "N/A")        
View(Probs)
rnotas_Prob1 <- na.omit(Probs$`Prob1 Média Final`)
rProb1_apro <- sum(rnotas_Prob1>=6)
rProb1_rec <- sum(rnotas_Prob1>=5 & rnotas_Prob1 <6)
rProb1_rep <- sum(rnotas_Prob1<5)
rProb1_valores <-c(rProb1_apro, rProb1_rec, rProb1_rep)
barplot(rProb1_valores,
        names.arg = nomes,
        col = cores,
        main = "(R)Situação da turma em Prob1",
        ylab = "Quantidade",
        ylim = c(0,15))

Prob1_Bruta <- c(1, 0, 0.5, 3.2, 0, 5, 8.5, 0, 0, 0, 0, 8.4, 8.4, 8.2, 9.4, 8.6, 6, 3.8, 8.4, 5.3, 7.8, 8.4, 5.8, 2.7, 6.2, 5.8, 1.9, 3.4, 3.6, 3.5)
bProb1_apro <- sum(Prob1_Bruta>=6)
bProb1_rec <- sum(Prob1_Bruta>=5 & Prob1_Bruta <6)
bProb1_rep <- sum(Prob1_Bruta<5)
bProb1_valores <-c(bProb1_apro, bProb1_rec, bProb1_rep)
barplot(bProb1_valores,
        names.arg = nomes,
        col = cores,
        main = "(B)Situação da turma em Prob1",
        ylab = "Quantidade",
        ylim = c(0,15))

rnotas_Prob2 <- na.omit(Probs$`Prob2 Média Final`)
rProb2_apro <- sum(rnotas_Prob2>=6)
rProb2_rec <- sum(rnotas_Prob2>=5 & rnotas_Prob2 <6)
rProb2_rep <- sum(rnotas_Prob2<5)
rProb2_valores <-c(rProb2_apro, rProb2_rec, rProb2_rep)
barplot(rProb2_valores,
        names.arg = nomes,
        col = cores,
        main = "(R)Situação da turma em Prob2",
        ylab = "Quantidade",
        ylim = c(0,15))

Prob2_Bruta <- c(8.5, 0.4, 8.5, 6, 9.9, 0.1, 0.2, 8.9, 0.2, 7.6, 2.8, 0.8, 6.5, 6.7, 8.6, 0.7, 0.6, 8.1, 9.1, 9, 0.1, 3.5, 6.2, 0.4)
bProb2_apro <- sum(Prob2_Bruta>=6)
bProb2_rec <- sum(Prob2_Bruta>=5 & Prob2_Bruta <6)
bProb2_rep <- sum(Prob2_Bruta<5)
bProb2_valores <-c(bProb2_apro, bProb2_rec, bProb2_rep)
barplot(bProb2_valores,
        names.arg = nomes,
        col = cores,
        main = "(B)Situação da turma em Prob2",
        ylab = "Quantidade",
        ylim = c(0,15))

Series <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", sheet = "Séries e EDOs", 
                    na = "N/A")        
View(Series)
rnotas_Series <- na.omit(Series$`Séries Média Final`)
rSeries_apro <- sum(rnotas_Series>=6)
rSeries_rec <- sum(rnotas_Series>=5 & rnotas_Series <6)
rSeries_rep <- sum(rnotas_Series<5)
rSeries_valores <-c(rSeries_apro, rSeries_rec, rSeries_rep)
barplot(rSeries_valores,
        names.arg = nomes,
        col = cores,
        main = "(R)Situação da turma em Série e EDOs",
        ylab = "Quantidade",
        ylim = c(0,25))

Séries_EDOs_Bruta <- c(6, 6, 6, 6, 6.2, 1.7, 6.8, 0.7, 3, 0.5, 5, 0.2, 0.5, 3, 1.5, 1.5, 5, 0, 0, 0.3, 2.5, 3.2, 1, 6.7, 0.7, 2.2, 1, 6, 6, 0, 2.3, 0, 8.5, 3.2, 6.7, 9, 4, 6, 3.5, 8)
bSeries_apro <- sum(Séries_EDOs_Bruta>=6)
bSeries_rec <- sum(Séries_EDOs_Bruta>=5 & Séries_EDOs_Bruta <6)
bSeries_rep <- sum(Séries_EDOs_Bruta<5)
bSeries_valores <-c(bSeries_apro, bSeries_rec, bSeries_rep)
barplot(bSeries_valores,
        names.arg = nomes,
        col = cores,
        main = "(B)Situação da turma em Série e EDOs",
        ylab = "Quantidade",
        ylim = c(0,25))

Int_inf <- read_excel("C:/Users/q2650/Downloads/os PDFs aleatorios/ufscar/Notas/Não Olham.xlsx", sheet = "Int.Inf", 
                     na = "N/A")        
View(Int_inf)
rnotas_Int_inf <- na.omit(Int_inf$`Int.Inf MF`)
rInt_inf_apro <- sum(rnotas_Int_inf>=6)
rInt_inf_rec <- sum(rnotas_Int_inf>=5 & rnotas_Int_inf <6)
rInt_inf_rep <- sum(rnotas_Int_inf<5)
rInt_inf_valores <-c(rInt_inf_apro, rInt_inf_rec, rInt_inf_rep)
barplot(rInt_inf_valores,
        names.arg = nomes,
        col = cores,
        main = "(R)Situação da turma em Introdução à Inferência(Turma Gustavo)",
        ylab = "Quantidade",
        ylim = c(0,15))

Int_Inf_Bruta <- c(6.7, 2.5, 5, 6.7, 5.3, 6.5, 5.8, 6.9, 5.1, 0.7, 8.6, 2.5, 2.3, 6.4, 6, 8.6, 7.8, 7.7, 7, 8.3, 7.7, 3.1, 7.7, 6.4, 8.7, 5.7, 7, 5.6)
bInt_inf_apro <- sum(Int_Inf_Bruta>=6)
bInt_inf_rec <- sum(Int_Inf_Bruta>=5 & Int_Inf_Bruta <6)
bInt_inf_rep <- sum(Int_Inf_Bruta<5)
bInt_inf_valores <-c(bInt_inf_apro, bInt_inf_rec, bInt_inf_rep)
barplot(bInt_inf_valores,
        names.arg = nomes,
        col = cores,
        main = "(B)Situação da turma em Introdução à Inferência(Turma Gustavo)",
        ylab = "Quantidade",
        ylim = c(0,20))
