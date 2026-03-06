htmltools::tagList(rmarkdown::html_dependency_jquery())

# library(xaringanthemer)
# style_mono_accent(
#   base_color = "#5E2129",
#   code_highlight_color = "#E3906F",
#   code_inline_color = "#0E2B54",
#   text_font_size = "1.3rem",
# 
# )

htmltools::tagList(
  xaringanExtra::use_clipboard(
    button_text = "<i class=\"fa fa-clipboard\"></i>",
    success_text = "<i class=\"fa fa-check\" style=\"color: #90BE6D\"></i>",
  ),
  rmarkdown::html_dependency_font_awesome()
)

xaringanExtra::use_logo(
  image_url = "https://www.ciisder.mx/images/logos/logo_uatx_2019.png",
  position = xaringanExtra::css_position(top = "1em", right = "1em")
)

xaringanExtra::use_tile_view()

xaringanExtra::use_share_again()



library(tidyverse)

set.seed(123)


data_normal<- rnorm(200)
hist(data_normal, col='steelblue', main='Normal')

set.seed(1254)

data_no_normal<- rexp(100, rate=3)
hist(data_no_normal, col='red', main='No normal')

set.seed(123)


plot(density(data_normal), main="Normal")

plot(density(data_no_normal), main="No Normal")

set.seed(123)


qqnorm(data_normal)
qqline(data_normal)

qqnorm(data_no_normal)
qqline(data_no_normal)

set.seed(124)


shapiro.test(data_normal)

ks.test(data_normal, "pnorm")

shapiro.test(data_no_normal)


ks.test(data_no_normal, "pnorm")

data("ToothGrowth")
data("iris")

data("ToothGrowth")
boxplot(len ~ supp, data=ToothGrowth, col=c("red", "blue"), main="Dientes")

data("iris")
boxplot(Petal.Width ~ Species, data=iris, col=c("pink", "purple", "cyan"), main="Flores")

aggregate(len ~ supp, data = ToothGrowth, var)

68.32 /  43.63

aggregate(Petal.Width ~ Species, data = iris, var)


r1<-0.03910612 / 0.01110612 #versicolor vs setosa
r2<-0.07543265 / 0.01110612 #virginca vs setosa
r3<-0.07543264 / 0.03910612 #virginica vs versicolor
cbind(r1,r2,r3)

m1<-lm(len ~ supp, data=ToothGrowth)
par(mfrow = c(1, 2))
plot(m1, which=c(1,3))

m2<-lm(Petal.Width ~ Species, data=iris)
par(mfrow = c(1, 2))
plot(m2, which=c(1,3))

var.test(len ~ supp, data = ToothGrowth) 

lmtest::bptest(m2) #sobre un modelo

library(car)
leveneTest(m2)

# ?fligner.test

datos<- data.frame(Tratamiento1=c(-3.10,0.18,-0.72,0.09,-1.66),
                  Tratamiento2=c(7.28,3.06,4.74,5.29,7.88),
                  Tratamiento3=c(0.12,5.51,5.72,5.93,6.56),
                  Tratamiento4=c(8.18,9.05,11.21,7.31,8.83))
knitr::kable(datos, align = "c")

ro1<- datos$Tratamiento1-mean(as.matrix(datos))
ro2<- datos$Tratamiento2-mean(as.matrix(datos))
ro3<- datos$Tratamiento3-mean(as.matrix(datos))
ro4<- datos$Tratamiento4-mean(as.matrix(datos))


shapiro.test(c(ro1,ro2, ro3, ro4))

library(tidyverse)
datost<- datos %>% pivot_longer(cols = everything(), names_to = "Tratamiento", values_to = "Valor")

lmtest::bptest(lm(Valor~Tratamiento, data = datost))



SCT1= 5*(mean(datos$Tratamiento1)-mean(as.matrix(datos)))^2
SCT2= 5*(mean(datos$Tratamiento2)-mean(as.matrix(datos)))^2
SCT3= 5*(mean(datos$Tratamiento3)-mean(as.matrix(datos)))^2
SCT4= 5*(mean(datos$Tratamiento4)-mean(as.matrix(datos)))^2

SCT= (SCT1+SCT2+SCT3+SCT4); SCT

SCE1= sum((datos$Tratamiento1-mean(datos$Tratamiento1))^2)
SCE2= sum((datos$Tratamiento2-mean(datos$Tratamiento2))^2)
SCE3= sum((datos$Tratamiento3-mean(datos$Tratamiento3))^2)
SCE4= sum((datos$Tratamiento4-mean(datos$Tratamiento4))^2)

SCE = (SCE1+SCE2+SCE3+SCE4); SCE

STT1= sum((datos$Tratamiento1-mean(as.matrix(datos)))^2)
STT2= sum((datos$Tratamiento2-mean(as.matrix(datos)))^2)
STT3= sum((datos$Tratamiento3-mean(as.matrix(datos)))^2)
STT4= sum((datos$Tratamiento4-mean(as.matrix(datos)))^2)

STT = (STT1+STT2+STT3+STT4); STT

CMT=SCT/(4-1);CMT

CME=SCE/(20-4);CME

valor_F= CMT/CME; valor_F

valor_p<-pf(valor_F, df1=4-1, df2=20-4, lower.tail=FALSE); valor_p


anova_R<- aov(Valor~Tratamiento, data = datost)
summary(anova_R)    


valor_F;valor_p
