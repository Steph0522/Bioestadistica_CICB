# source("C:/mi_script.R")
# Este es mi código
data(iris)
# Aquí termina el código


library(datasets)
data(iris)
summary(iris)
boxplot(iris)

getwd()

setwd("/home/steph/Desktop/")


install.packages("tidyverse")

library(tidyverse)

devtools::install_github('rstudio/rmarkdown')

a <- 1                                                   #escalar
letra <- "a"                                             #caracter
b <- c(1,2,3)                                            #vector
c<- matrix(1:10)                                         #matriz
d<- data.frame(Especie=c("A", "B"), Longitud=c(c(1,2)))  #dataframe
e<- list(c(1:20), c(1:10))                               #lista

write.table(d, "data.txt", sep = "\t")
write.csv(d, "data.csv")
saveRDS(d, "data.RDS")

log(a)

help("log")
?log

a + a
a - 2
1 * pi
2 / 3
4 ^ a

average<- function(x){sum(x)/length(x)}
x<- 1:100
average(x)
mean(x)

colores<- c("red", "black", "blue")
un_vector <- c(1:10)

matri<-matrix(1:20, nrow = 5, ncol = 4)
dim(matri)
matri

un_vector <- 1:20
una_matriz <- matrix(1:20, nrow = 2)
una_df     <- data.frame("numeros" = 1:3, "letras" = c("a", "b", "c"))
una_lista <- list("vec" = un_vector,"mat" = una_matriz,"df" = una_df)
una_lista

data(ToothGrowth)
str(ToothGrowth)

dim(ToothGrowth)

head(ToothGrowth)

df <- data.frame(
  "entero" = 1:3, 
  "factor" = c("alto", "medio", "bajo"), 
  "letras" = as.character(c("a", "b", "c")))
df

names(df)

colnames(df)

df$factor
class(df$factor)
is.vector(df$factor)

notas_estudiantes <- list(nombres = c("Ana", "Clara", "Sofy"),
               id_estudiante = c("i1", "i2", "i3"),
               notas = c(10, 9,7))

notas_estudiantes$nombres
notas_estudiantes[["nombres"]]

evaluaciones<- as.data.frame(notas_estudiantes)

evaluaciones[c(1,2)]

evaluaciones[1:2]

evaluaciones[,-1]

evaluaciones[c("nombres","notas")]

mas_de_8<-evaluaciones[evaluaciones$notas > 8,]
mas_de_8

evaluaciones[evaluaciones$nombres == "Sofy",]

data()


download.file(
  url = "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", 
  destfile = "iris.data")


iris_dat<-readr::read_csv("https://archive.ics.uci.edu/ml/
                          machine-learning-databases/iris/iris.data")

iris_data<- read.csv("iris.data", header = F)
iris_data<- read.delim("iris.data",header = F, sep = ",")

library(readr)
iris_data<-read_csv("iris.data", 
                    col_names = c("Longitud.sepalo", "Ancho.Sepalo" ,
                    "Longitud.Petalo" ,"Ancho.Petalo" , "Especies"))

data<- read_csv("../Data/penguins_size.csv")



max(evaluaciones$notas)

which.max(evaluaciones$notas)

#Ejemplos
par(mfrow= c(1,3))
plot(x=iris$Sepal.Length, y=iris$Sepal.Width)
plot(x=iris$Sepal.Length, y = iris$Species)
plot(x=iris$Sepal.Length)

par(mfrow= c(1,3))

plot(x = iris$Species, y = iris$Sepal.Length)
plot(x=iris$Species, y=iris$Species)
plot(x=iris$Species)

hist(x = iris$Sepal.Length, main = "Histograma de longitud de sepalo", 
     xlab = "Longitud", ylab = "Frecuencia",
     col = "purple")


plot(x = iris$Petal.Length, y = iris$Petal.Width, 
     col = iris$Species, xlab = "Largo", ylab = "Ancho")

plot(x=iris$Species, y = iris$Sepal.Length, xlab = "Especie",
     ylab = "Longitud Sépalo", col = c("purple", "pink", "blue"))

boxplot(formula = Sepal.Length ~ Species, data =  iris, xlab = "Especie", 
        ylab = "Longitu Sépalo",      col = c("purple", "pink", "blue"))

#dataset de ensayo
df<- data.frame(x= c(1:5),y= c(200, 400, 600, 700, 500))
par(mfrow = c(1, 3))
plot(df$x, df$y, type = "p", main = 'type = "p"')
plot(df$x, df$y, type = "l", main = 'type = "l"')
plot(df$x, df$y, type = "b", main = 'type = "b"')


par(mfrow = c(1, 3))
plot(df$x, df$y, type = "c", main = 'type = "c"')
plot(df$x, df$y, type = "s", main = 'type = "s"')
plot(df$x, df$y, type = "h", main = 'type = "h"')

barplot(y ~ x , data = df, main = "Barplot", col = "darkred")

# png(filename="gráfica1.png", width=648, height=432)
# plot(df$x, df$y, type = "p", main = 'type = "p"')
# dev.off()


library(tidyverse)

#asignando la data a la variable "mi_data"
mi_data<-ToothGrowth

ToothGrowth %>% select(len, dose) %>% glimpse()

ToothGrowth%>%mutate(dose = case_when(
    dose == 0.5 ~ "D_0.5",
    dose == 1 ~ "D_1",
    dose == 2 ~ "D_2")) %>%  mutate(
      dose = factor(dose, levels = c("D_2", "D_1", "D_0.5"))) %>% head()

ToothGrowth %>% group_by(supp) %>% summarise(promedio = mean(len),
                                             suma = sum(len),
                                             n = n(),
                                             mediana =median(len))

ToothGrowth %>%  mutate(dose=case_when(
dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% 
  ggplot(aes(x = dose, y = len, fill=supp)) +  
  geom_boxplot()+
  ylab("Longitud diente")+  xlab("Dosis")+
  ggtitle("Longitud de dientes por Dosis aplicada") +
  scale_x_discrete(limits = c("D2", "D0.5", "D1"), position ="bottom" )+
  scale_y_continuous(breaks = c(0, 20,40))+
  scale_fill_manual(values = c("#FF00FF","#00FFFF"))
