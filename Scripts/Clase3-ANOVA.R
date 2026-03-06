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

knitr::include_graphics("../images/fdist.png")

knitr::include_graphics("https://statologos.com/ezoimgfmt/fourpillarfreedom.com/wp-content/uploads/2019/04/post_hoc1.jpg?ezimgfmt=rs:386x298/rscb1/ng:webp/ngcb1")

datos<- data.frame(Tratamiento1=c(-3.10,0.18,-0.72,0.09,-1.66),
                  Tratamiento2=c(7.28,3.06,4.74,5.29,7.88),
                  Tratamiento3=c(0.12,5.51,5.72,5.93,6.56),
                  Tratamiento4=c(8.18,9.05,11.21,7.31,8.83))

datost<- datos %>% pivot_longer(cols = everything(), names_to = "Tratamiento", values_to = "Valor")
head(datost)

knitr::kable(datos, align = "c")

SCT1= 5*(mean(datos$Tratamiento1)-mean(as.matrix(datos)))^2
SCT2= 5*(mean(datos$Tratamiento2)-mean(as.matrix(datos)))^2
SCT3= 5*(mean(datos$Tratamiento3)-mean(as.matrix(datos)))^2
SCT4= 5*(mean(datos$Tratamiento4)-mean(as.matrix(datos)))^2

SCT= (SCT1+SCT2+SCT3+SCT4); print(paste0("SCT: ",SCT))

SCE1= sum((datos$Tratamiento1-mean(datos$Tratamiento1))^2)
SCE2= sum((datos$Tratamiento2-mean(datos$Tratamiento2))^2)
SCE3= sum((datos$Tratamiento3-mean(datos$Tratamiento3))^2)
SCE4= sum((datos$Tratamiento4-mean(datos$Tratamiento4))^2)

SCE = (SCE1+SCE2+SCE3+SCE4); print(paste0("SCE: ",SCE))

CMT=SCT/(4-1);print(paste0("CMT: ",CMT))

CME=SCE/(20-4);print(paste0("CME: ",CME))

qcri<-qtukey(p = 0.95, nmeans = 4, df = 16)

sqme<- sqrt(CME/5)

HSD_val<- qcri*sqme


qcri; HSD_val

dif1<- mean(datos$Tratamiento3)-mean(datos$Tratamiento1)
dif2<- mean(datos$Tratamiento2)-mean(datos$Tratamiento1)
dif3<- mean(datos$Tratamiento4)-mean(datos$Tratamiento1)
dif4<- mean(datos$Tratamiento2)-mean(datos$Tratamiento3)
dif5<- mean(datos$Tratamiento4)-mean(datos$Tratamiento3)
dif6<- mean(datos$Tratamiento4)-mean(datos$Tratamiento2)


dif1>HSD_val;dif2>HSD_val;dif3>HSD_val;dif4>HSD_val;dif5>HSD_val;dif6>HSD_val

up<-dif1+(qcri*sqme)
low<-dif1-(qcri*sqme)
data.frame(up,low)

anov<- aov(Valor~Tratamiento,data = datost)
TukeyHSD(anov, "Tratamiento", ordered = T)

plot(TukeyHSD(anov, "Tratamiento", ordered = T))

library(agricolae)
hsd_agricolae<-HSD.test(anov, "Tratamiento", group = TRUE)
plot(hsd_agricolae)

knitr::include_graphics("images/new.jpg")

library(agricolae)
SNK.test(anov, "Tratamiento", console = TRUE)

library(agricolae)
snk<-SNK.test(anov, "Tratamiento", console = FALSE)
plot(snk)

library(agricolae)
duncan.test(anov, "Tratamiento", console = TRUE)

library(agricolae)
duncan<-agricolae::duncan.test(anov, "Tratamiento")
plot(duncan)

library(BHH2)
data(penicillin.data)
penicilina<- penicillin.data
str(penicilina)

plot(yield~blend+treat, data = penicilina)

modelo3<- lm(yield~blend+treat, data = penicilina)

par(mfrow = c(2, 2))
plot(modelo3)

anova(lm(yield~ blend+treat,data = penicilina))

data("ToothGrowth")
dientes<- ToothGrowth
str(dientes)

dientes$dose <- factor(dientes$dose, 
                  levels = c(500, 1000, 2000),
                  labels = c("D0.5", "D1", "D2"))
head(dientes)


library(ggpubr)
ggboxplot(data = ToothGrowth, x = "supp", y = "len", fill = "supp")+theme(text = element_text(size = 40))
ggboxplot(data = ToothGrowth, x = "dose", y = "len", fill = "dose")+theme(text = element_text(size = 40))

ggboxplot(data = ToothGrowth, x = "dose", y = "len", fill = "supp")

ggline(ToothGrowth, x = "dose", y = "len", color = "supp", add = c("mean_se", "jitter"))

anova1<- aov(len ~ supp + dose, data = dientes)
anova2<-  aov(len ~ supp * dose, data = dientes)
anova3<-  aov(len ~ supp : dose, data = dientes)


summary(anova1)

summary(anova2)

summary(anova3)

library(faraway)
data(rabbit)
conejos<- rabbit
str(conejos)

plot(gain~ block + treat, rabbit, col=456)

#Observamos que tanto el bloque como el tratamiento son significativos.
lm.rabbit <- lm(gain~ block+treat, data=rabbit)
anova(lm.rabbit)

# nos interesa analizar el efecto del factor (dieta) sobre la respuesta una vez que se haya tenido en cuenta la variabilidad atribuible al bloque (camada).
library(nlme)
lme.rabbit1 <- lme(gain~ treat, random=~1|block, data=rabbit)
anova(lme.rabbit1)

df<-data.frame(crecimiento = c (13, 16, 16, 12, 15, 16, 19, 16, 15, 15, 12, 15,
                          19, 19, 20, 22, 23, 18, 16, 18, 19, 20, 21, 21,
                          21, 23, 24, 22, 25, 20, 20, 22, 24, 22, 25, 26),
                 fertilizante = factor(c(rep(c(' A ', ' B ', ' C '), each=12 ))),
                 tech = factor(c(rep(1: 9,  each=4 ))))

str(df)

summary(aov(crecimiento ~ fertilizante/tech, data = df))

summary(aov(crecimiento ~ fertilizante %in% tech, data = df))

ratas<- readr::read_csv("https://raw.githubusercontent.com/Steph0522/Curso_Bioestadistica_2023/main/Modulo2/data_ratas.csv")
str(ratas)


anova_factorial <- lm(glycogen ~ food * prep * method, data = ratas)
anova(anova_factorial)

anova_split <- aov(glycogen ~ food * prep * method + Error(rat/food:prep), data = ratas)
summary(anova_split)

individuos<- factor(c(rep(1,5), rep(2,5), rep(3, 5),
                      rep(4, 5), rep(5,5), rep(6,5)))
tiempo<- factor(rep(1:5, 6))
rendimiento<- c(8.5, 8.2,8.9, 7.7, 7.4,
                9.8,8.9,8.9,8.8,8.1,
                9.6,9.0, 9.3, 7.5, 7.1,
                7.5, 7.8, 7.8, 4.5, 4.6,
                5.8, 5.8, 5.9, 2.6, 1.2,
                9.9, 9.8, 9.6, 8.6, 8.7)
data_rendimiento<- data.frame(individuos=individuos,
                              tiempo=tiempo, rendimiento=rendimiento)


str(data_rendimiento)

library(ez)
ezANOVA(data=data_rendimiento, dv=rendimiento, wid=individuos, within=tiempo)

boxplot(rendimiento~tiempo, xlab="tiempo", ylab="Rendimiento académico", main="rendimiento alumnos con el paso del tiempo", col="blue", data=data_rendimiento)

library(tidyverse)
library(jmv)
#cambiamos el formato de la data
data_rendimiento_notidy<-data_rendimiento %>% mutate(tiempo =case_when(
  tiempo== 1 ~ "T1",
  tiempo== 2 ~ "T2",
  tiempo== 3 ~ "T3",
  tiempo== 4 ~ "T4",
  tiempo== 5 ~ "T5")) %>% pivot_wider(names_from =tiempo, 
                                      values_from = rendimiento )

jmv::anovaRMNP(data_rendimiento_notidy, measures=vars(T1,T2,T3,T4,T5))
