library(magrittr)
library(dplyr)
mtcars

mtcars %>% select(mpg,cyl,hp) #selecionando apenas as colunas mpg cyl e hp

mtcars %>% filter(mpg>20,hp<150) #filtrando, mpg>20 e hp<150

mtcars %>% arrange(desc(mpg)) #ordenando, decrescentemente, pela coluna mpg