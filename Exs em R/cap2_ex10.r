library(nycflights13)
library(magrittr)
library(dplyr)
library(ggplot2)
#ou library(tidyverse)
flights_nyc_2013 <- nycflights13::flights
help(flights)
head(flights_nyc_2013)
flights_nyc_2013 %>% select(arr_delay)
flights_nyc_2013 %>% filter(arr_delay/60>1)

flights_nyc_2013 %>% filter(month == 6,arr_delay/60>1) #filtrando voos do mes junho (moth ==6) e com atraso de chegada mais de uma hora(60min)

x <-flights_nyc_2013 %>% group_by(carrier) %>% summarise(atraso_medio = mean(arr_delay,na.rm = TRUE))
x
help(mean)
flights_nyc_2013
#x
#ggplot(x) +
#    geom_bar(aes(x = as.factor(carrier), fill=as.factor(atraso_medio)))+
#    labs(fill = "Tempo médio de atraso")
#  theme(legend.position = "top")

ggplot(x, aes(x=carrier,y=atraso_medio,fill=carrier)) +
    geom_bar(stat = "identity")+
    labs(x="Companhia",y="Tempo médio de atraso",fill = "Companhia")+
    theme(legend.position = "top")

########################################################################################################
########################################################################################################
#OU

ggplot(flights_nyc_2013, aes(x = carrier, y = arr_delay, fill = carrier)) + #dados eh flights_nyc_2013, eixo X companhias,eixo y o tempo de atraso (nao eh a media ainda)
  stat_summary(fun = mean, geom = "bar", na.rm = TRUE) +  #fun = mean ---> calcula a media do eixo y que é arr_delay(tempo de atraso)
  labs(x = "Companhia", y = "Tempo médio de atraso") +
  theme_light() +
  theme(legend.position = "top")

help(theme_minimal)
