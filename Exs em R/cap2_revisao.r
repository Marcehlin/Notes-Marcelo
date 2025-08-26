?exp #
help()

0/0
x <- 2+2
exp(1)

ls() #listar variavéis
rm() #remover variáveis

x = 1; y = 3
x < y

TRUE
!TRUE

x = 1; y = 0
x | y

x = "Marcelo"
y = "Huang"

paste(x)
paste(x,y)
paste(x,y, sep = " + ")

x = c(4,3,9,6)

x
x[2]
x[1]

x[2:3]

x = x[c(2,3)]
x[2:3]

x

x[-c(2, 3)]
x

y=c(5,2,3,1,6,0)
y = y-1

y =y*2
y

x = seq(from = 1, to = 100, by = 2)
x

y = seq(from = 0, to = 100, length = 12)
y

bool_de_x = x>5
bool_de_x

x = c(TRUE,TRUE,FALSE,FALSE)
y = c(TRUE,FALSE,TRUE,FALSE)

x | y

x = seq(from = 1, to = 100, by = 2)
x[x>5] = 0
x

x[!(x <= 0)] = -1
x

x = c(1,1,0,0)
y = c(1,0,1,0)
x|y
x

log(exp(1))


x = c('bom', 'dia', "marcelo")
paste(x,'__')

paste(x,"__",collapse = '+')

rep(x,10)

rep(10,2)
rep(114514,20)

rep(c(8,9),10)

rep(c(5,6),each = 20)

sort(c(1,5,2,4,6,2,5,2,4,6,2,54,2,4,3,4,34,1,43,3,43,5))

#function
funcao_potencia = function(x,y){
    return (x^y)
}
funcao_potencia(2,8)
funcao_potencia(c(2,3,4),c(2,2,2))

outra_funcao_potencia = function(x,y=rep(2,length(x))){
    return (x^y)
}
outra_funcao_potencia(c(10,2,10),c(9,9,9))

#laço
n <- 4
i <- 1
nFatorial <- 1
while(i<=n){
    nFatorial=nFatorial*i
    i = i+1
}
nFatorial
#ou
nFat <- 1
for (i in 1:10){
    nFat <- nFat*i
}
nFat

x <- 1:100000

func_soma_acumulada <- function(x){
    soma_acumulada <- NULL
    soma_acumulada[1] = x[1]
    for(i in 2:length(x)){
        soma_acumulada[i] <- soma_acumulada[i-1] + x[i]
    }
    return (soma_acumulada)
}
system.time(func_soma_acumulada(x))[1]
outra_func_soma_acumulada <- function(x){
    soma_acumulada <- rep(NA,length(x))
    soma_acumulada[1] = x[1]
    for(i in 2:length(x)){
        soma_acumulada[i] <- soma_acumulada[i-1] + x[i]
    }
    return (soma_acumulada)
}
system.time(outra_func_soma_acumulada(x))[1]
system.time(cumsum(x))[1]
testaX <- function(x) {
  if (!is.numeric(x)) {
    print("x não é um número")
  } else if (x > 0) {
    print("x é positivo")
  } else if (x < 0) {
    print("x é negativo")
  } else {
    print("x é nulo")
  }
}
testaX(2^10)

minha_lista <- list("一","2","três","four","5",sexto = FALSE,setimo=TRUE,oitavo = "eight")
minha_lista[7]

minha_lista$sexto

x <- rnorm(100)
y <- 1 + 2*x + rnorm(length(x), 0,1)
y
help(rnorm)
ajuste <- lm(y~x)
print(ajuste)
names(ajuste)
help(plot)
plot(x, y, pch = 18, bty = "l")
abline(ajuste, col = 2, lwd = 3)

mean(x)
mean(y)
x
mean(x)
var(x)
min(x)
max(x)
which.min(x)
x[51]
sort(x)
order(x)
median(x)
cor(x,y)
x
which(x>0)

x <- c(30, 1, 20, 5, 20, 60)
y <- c(12, 0, 2, 15, 22, 20)
x[which(x > 10)]
y[(x>10)]
mean(y[x>10])

x <- c("S", "R", "P", "P", "Q", "P")
y <- c("a", "a", "b", "a", "c", "d")
x[y %in% c("b","c","d")]
table(x)
table(y)
table(x,y)

dados <- data.frame(altura = rnorm(100, 1.7, 0.2), salario = rnorm(100, 3000, 500))
dados
dados$peso <- dados$altura * 35 + rnorm(100, 0, 20)
dados

dados[dados$salario > 3000 & dados$altura > 1.80 & dados$peso >70,]
help(apply)
apply(dados,2,mean)

estatisticas <- apply(dados,2,function(x) {
  lista_resultados <- NULL
  lista_resultados$media <- mean(x)
  lista_resultados$media_aparada_dez_porcento_cada_lado <- mean(x,trim = 0.1)
  lista_resultados$mediana <- median(x)
  lista_resultados$maximo <-max(x)
  return (lista_resultados)
})

estatisticas$peso

#operador pipe
library(magrittr)
x <- c(1, 2, 3, 4)
x %>% sum() %>% mean()
x %>% mean() %>% sum()
x %>% sum() %>% sqrt()
x %>% sqrt() %>% sum()

y <- rnorm(1000)
y %>% plot(x = 1:length(y), y=., pch=18)
y
help(plot)

########################################################################################################
########################################################################################################
#Pacote dplyr e hflights
library(dplyr)
library(hflights)
data(hflights)
flights <- tbl_df(hflights)
flights
flights %>% filter(Month == 1, DayofMonth == 1)
flights %>% filter(Month == 1, DayofMonth == 1)
flights %>% filter(UniqueCarrier %in% c("AA", "UA"))
flights %>% select(DepTime, ArrTime, FlightNum)
flights %>% select(contains("Taxi"), contains("Delay"))
flights %>% arrange(DepDelay) #ordenar, padrao = ordem crescente
flights %>% select(contains("DepDelay")) %>% arrange(DepDelay)
flights %>% arrange(desc(DepDelay)) # ordem descrescente
flights <- flights %>% mutate(Speed = Distance / AirTime * 60) #mutate: criar e adicionar no banco de dados
flights %>% select(contains("Speed"))
flights %>% group_by(Dest) %>% summarise(avg_delay = mean(ArrDelay, na.rm = TRUE))
flights %>% group_by(UniqueCarrier) %>% summarise_each(funs(mean), Cancelled, Diverted)
flights %>% group_by(UniqueCarrier) %>% summarise_each(funs(mean, var), Cancelled, Diverted)
flights %>% group_by(UniqueCarrier) %>% summarise_each(funs(min(., na.rm = TRUE), max(., na.rm = TRUE)), matches("Delay"))
flights %>% group_by(UniqueCarrier, DayOfWeek) %>% summarise_each(funs(min(., na.rm = TRUE), max(., na.rm = TRUE)), matches("Delay"))

########################################################################################################
########################################################################################################
#Pacote ggplot2
library(ggplot2)
head(mtcars)
mtcars
ggplot(data = mtcars) +
  geom_point(aes(x = disp, y = mpg))
ggplot(data = mtcars) +
  geom_point(aes(x = disp, y = mpg, colour = as.factor(am)))
ggplot(mtcars) +
  geom_boxplot(aes(x = as.factor(cyl), y = mpg))
ggplot(mtcars) +
  geom_boxplot(aes(x = as.factor(cyl), y = mpg, fill = as.factor(cyl)))
ggplot(mtcars) +
  geom_histogram(aes(x = mpg)) +
  xlab("Milhas por galão") +
  ylab("Frequência")
ggplot(mtcars) +
  geom_bar(aes(x = as.factor(cyl), fill = as.factor(cyl))) +
  labs(fill = "Cilindros") +
  theme(legend.position = "top")
ggplot(mtcars) +
  geom_point(aes(x = mpg, y = disp, colour = as.factor(cyl))) +
  facet_grid(am ~ .)
theme_set(theme_light(base_size = 10))
ggplot(mtcars) +
  geom_point(aes(x = mpg, y = disp, colour = as.factor(cyl))) +
  facet_grid(. ~ am)

########################################################################################################
########################################################################################################
#Pacote tidyr
dados.originais <- data.frame(
  paciente = c("João", "Marcos", "Antonio"),
  pressao.antes = c(67, 80, 64),
  pressao.durante = c(54, 70, 60),
  pressao.depois = c(56, 90, 50)
)
dados.originais

library(tidyr)
dados.novo.formato <- dados.originais %>%
  pivot_longer(cols = pressao.antes:pressao.depois,
               names_to = "instante",
               values_to = "pressao.arterial")
dados.novo.formato

dados.novo.formato <- data.frame(
  paciente = c("João", "Marcos", "Antonio", "João", "Marcos", "Antonio", "João", "Marcos", "Antonio"),
  instante = c("pressao.antes", "pressao.antes", "pressao.antes", "pressao.durante", "pressao.durante", "pressao.durante", "pressao.depois", "pressao.depois", "pressao.depois"),
  pressao.arterial = c(67, 80, 64, 54, 70, 60, 56, 90, 50)
)
dados.novo.formato

dados.wide <- dados.novo.formato %>%
  pivot_wider(names_from = instante, values_from = pressao.arterial)
dados.wide

########################################################################################################
########################################################################################################
#misturando tidyr e ggplot2

library(tidyr)
library(ggplot2)

# Dados originais no formato "wide"
dados.originais <- data.frame(
  paciente = c("João", "Marcos", "Antonio"),
  pressao.antes = c(67, 80, 64),
  pressao.durante = c(54, 70, 60),
  pressao.depois = c(56, 90, 50)
)
dados.originais
dados.long <- dados.originais %>%
  pivot_longer(cols = pressao.antes:pressao.depois,
               names_to = "instante",
               values_to = "pressao.arterial")
dados.long

ggplot(dados.long, aes(x = factor(instante, levels = c("pressao.antes", "pressao.durante", "pressao.depois")),
                       y = pressao.arterial, group = paciente, color = paciente)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Variação da Pressão Arterial dos Pacientes",
       x = "Instante",
       y = "Pressão Arterial") +
  theme_minimal()

########################################################################################################
########################################################################################################
#pacote readr
library(readr)
url <- "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"
dados_titanic <- read_csv(url)
dados_titanic
table(dados_titanic$Survived)
write_csv(dados_titanic, "titanic_local.csv")
