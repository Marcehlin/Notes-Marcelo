library(dplyr)
library(ggplot2)
mtcars
help(Geom)
help(geom_point)
help(ggplot)
ggplot(data = mtcars) +
    geom_point(aes(x=hp,y=mpg, colour = (cyl)))
