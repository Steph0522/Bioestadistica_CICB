
library(tidyverse)



data <- data.frame(
  grupo = c("A","A","A",
            "B","B","B",
            "C","C","C"),
  tiempo = c(18,22,20,
             25,28,24,
             15,17,19))
data

data$rank <- rank(data$tiempo)

data

R_i <- tapply(data$rank, data$grupo, sum)
R_i

n_i <- table(data$grupo)
n_i

N <- nrow(data)
N

k <- length(unique(data$grupo))
k

H <- (12/(N*(N+1))) * sum((R_i^2)/n_i) - 3*(N+1)
H

df <- k - 1
df

qchisq(0.95, df)

Hcalculado = 6.49
Hcrítico = 5.991
Hcalculado > Hcrítico
pchisq(H, df = df, lower.tail = FALSE)

kruskal.test(tiempo ~ grupo, data = data)

set.seed(123) 
program <- factor(rep(c("Program_A", "Program_B", "Program_C"), each = 30))
mobility_scores <- c(
  sample(1:10, 30, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.15, 0.15, 0.1, 0.05, 0.05, 0, 0)),
  sample(1:10, 30, replace = TRUE, prob = c(0.1, 0.1, 0.15, 0.2, 0.2, 0.1, 0.1, 0.1, 0, 0)),
  sample(1:10, 30, replace = TRUE, prob = c(0.05, 0.05, 0.1, 0.15, 0.25, 0.2, 0.1, 0.05, 0, 0))
)

data <- data.frame(program, mobility_scores)
head(data)

kruskal_result <- kruskal.test(mobility_scores ~ program, data = data)
kruskal_result


n <- 30  
set.seed(123)  
group_A <- rexp(n, rate = 0.1) + 15  
group_B <- rexp(n, rate = 0.1) + 25  
group_C <- rexp(n, rate = 0.1) + 35 

data <- data.frame(
  mobility_scores = c(group_A, group_B, group_C),  
  program = factor(rep(c("Program A", "Program B", "Program C"), each = n)) )

head(data)

ggplot(data, aes(x = mobility_scores, fill = program)) + geom_histogram(aes(y = ..density..), bins = 15, alpha = 0.5, position = "identity") + geom_density(alpha = 0.7) + facet_wrap(~ program) + ggtitle("Distribución de los puntajes de movilidad por programa") + xlab("Puntajes de movilidad") + ylab("Densidad") + theme_minimal() + scale_fill_brewer(palette = "Set1")

kruskal_test <- kruskal.test(mobility_scores ~ program, data = data)
kruskal_test

pairwise.wilcox.test(data$mobility_scores, data$program, p.adjust.method = "holm")

library(rstatix)
dunn_res <- data %>% dunn_test(mobility_scores ~ program, p.adjust.method = "bonferroni")
dunn_res

library(FSA)
dunn_result <- dunnTest(mobility_scores ~ program, data = data, method = "bonferroni")

dunn_result

library(ggpubr)
data %>% ggplot(aes(x = program, y = mobility_scores, fill = program))+
  geom_boxplot()+ geom_pwc(method = "dunn_test",  p.adjust.method = "bonferroni",  label = "p.adj.format")+theme_classic()


pvals <- dunn_res %>%
  select(group1, group2, p.adj) %>%
  tidyr::pivot_wider(names_from = group2, values_from = p.adj)

pvals

library(multcompView)
comparisons <- dunn_res %>%
  unite(pair, group1, group2, sep = "-") %>%
  select(pair, p.adj)

pvec <- comparisons$p.adj
names(pvec) <- comparisons$pair

letras <- multcompLetters(pvec)$Letters
letras

letras_df <- data.frame(
  grupo = names(letras),
  letra = letras)%>%
  rename(program=grupo)

pos <- data %>%
  group_by(program) %>%
  summarise(y = max(mobility_scores) + 4)

letras_df <- left_join(letras_df, pos, by = "program")
letras_df



ggplot(data, aes(x = program, y = mobility_scores, fill = program)) +
  geom_boxplot() +
  geom_text(data = letras_df,
            aes(x = program, y = y, label = letra, fontface =  "bold"),
            size = 7) +
  theme_classic()
