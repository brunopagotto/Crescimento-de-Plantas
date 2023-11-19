### PROJETO CAROL ###

# Instalar pacotes e ler bibliotecas
install.packages("readxl")
library(readxl)
install.packages("reshape2")
library(reshape2)
install.packages("emmeans")
library(emmeans)
install.packages("AgroR")
library(AgroR)
library(dplyr)

# Leitura e tratamento dos dados
dados <- read_excel("Biometricas altura ok.xlsx")

# Ordenar o dataframe por quatro colunas diferentes
dados <- with(dados, dados[order(CICLO, BLOCO, EFLUENTE, INOCULO), ])

# Converter as colunas para tipo numérico e arredondar valores em duas casas
for (i in 5:7) {
  dados[, i] <- as.numeric(unlist(dados[, i]))
}
dados[5:7] =  round(dados[5:7], digits = 2)
str(dados)

# Transformar as colunas em variáveis categóricas
dados$BLOCO <- factor(dados$BLOCO)
dados$CICLO <- factor(dados$CICLO)
dados$INOCULO <- factor(dados$INOCULO)

# Separar os dados de acordo com o período de tempo da coleta
dados_tempo_1 = dados[c(1:5)] # 43, 41
dados_tempo_1$ALTURA = dados_tempo_1$`43 E 41 DAS`
dados_tempo_1 = dados_tempo_1[-5]
dados_tempo_2 = dados[c(1:4,6)] # 50, 49
dados_tempo_2$ALTURA = dados_tempo_2$`50 E 49 DAS`
dados_tempo_2 = dados_tempo_2[-5]
dados_tempo_3 = dados[c(1:4,7)] # 64, 76
dados_tempo_3$ALTURA = dados_tempo_3$`64 E 76 DAS`
dados_tempo_3 = dados_tempo_3[-5]


# Gráficos

library(ggplot2)

ggplot(dados_tempo_1, aes(x = factor(EFLUENTE), y = ALTURA)) +
  geom_boxplot() +
  facet_grid(INOCULO ~ CICLO) +
  labs(x = "EFLUENTE", y = "ALTURA") +
  ggtitle("Boxplots por combinação dos fatores")


# Calcular os limites inferiores de outliers para cada combinação de fatores
limites_outliers <- aggregate(ALTURA ~ EFLUENTE + INOCULO + CICLO, data = dados_tempo_1, FUN = function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  limite_inferior <- q[1] - 1.5 * iqr
})

# Armazenar vetor com os limites inferiores
lim_inf = limites_outliers$ALTURA

# Calcular os limites superiores de outliers para cada combinação de fatores
limites_outliers <- aggregate(ALTURA ~ EFLUENTE + INOCULO + CICLO, data = dados_tempo_1, FUN = function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  limite_superior <- q[2] + 1.5 * iqr
})

# Armazenar vetor com os limites superiores
lim_sup = limites_outliers$ALTURA

# Montar Dataframe com os limites inferior e superior
limites_outliers = limites_outliers[c(1:3)]
limites_outliers$LIM_INF = lim_inf
limites_outliers$LIM_SUP = lim_sup

# Replicar cada linha por 4 vezes
limites_outliers <- limites_outliers[rep(row.names(limites_outliers), each = 4), ]

# Redefinir os índices das linhas
rownames(limites_outliers) <- NULL

# Reordenar os dados do tempo 1 seguindo a combinação de fatores por cada bloco
blocos_dados_tempo_1 <- with(dados_tempo_1, dados_tempo_1[order(CICLO, INOCULO, EFLUENTE), ])

# Definir como NA (valor ausente) os outliers
blocos_dados_tempo_1$ALTURA[which(blocos_dados_tempo_1$ALTURA < 
                                    limites_outliers$LIM_INF)] = NA
blocos_dados_tempo_1$ALTURA[which(blocos_dados_tempo_1$ALTURA > limites_outliers$LIM_SUP)] = NA

# Calcular a média para cada grupo de 4 linhas
media_blocos_tempo_1 = aggregate(ALTURA ~ EFLUENTE + INOCULO + CICLO, 
                                 data = blocos_dados_tempo_1, FUN = mean)

# Replicar cada linha por 4 vezes
media_blocos_tempo_1 = media_blocos_tempo_1[rep(row.names(media_blocos_tempo_1), each = 4), ]

# Redefinir os índices das linhas
rownames(media_blocos_tempo_1) <- NULL

# Preencher os NA's com as médias dos 4 blocos para a combinação de fatores específica
blocos_dados_tempo_1$ALTURA[which(is.na(blocos_dados_tempo_1$ALTURA))] = 
  media_blocos_tempo_1$ALTURA[which(is.na(blocos_dados_tempo_1$ALTURA))]

# Níveis para cada fator de tratamentos
library(dae)
n.F <- 2
n.D <- 5
n.Bloco <- 4
tr <- data.frame(cbind(INOCULO = paste("I", rep(1:n.F, each = n.D, times =
                                                  n.Bloco),
                                       sep = ""),
                       EFLUENTE = paste("E", rep(1:n.D, times = n.F*n.Bloco),
                                        sep = "")))
units <- list(Bloco = n.Bloco,
              Parcela = (n.F*n.D))
nest <- list(Parcela = "Bloco")
(lay <- designRandomize(allocated = tr,
                        recipient = units,
                        nested.recipients = nest,
                        seed = 9719532))

table(lay$I)
table(lay$E)

lay$Tratamento <- factor(paste(lay$I, lay$E, sep = ":"))
print(lay$Tratamento)

# Gráfico das parcelas por blocos com cada combinação de tratamentos
ggplot(lay,
       aes(x = Parcela,
           y = Bloco,
           label = Tratamento)) +
  geom_tile(aes(fill = Tratamento),
            color="black") +
  geom_text() +
  theme_bw() +
  theme(axis.line = element_line(),
        # axis.text = element_blank(),
        # axis.title = element_blank(),
        legend.position = c("bottom"),
        legend.direction = c("horizontal"))

# Análises Descritivas
str(blocos_dados_tempo_1)
summary(blocos_dados_tempo_1)
head(blocos_dados_tempo_1)

# Número de observações
with(blocos_dados_tempo_1, tapply(ALTURA, list(EFLUENTE, INOCULO), length))
with(blocos_dados_tempo_1, tapply(ALTURA, list(EFLUENTE, INOCULO), sum))
with(blocos_dados_tempo_1, tapply(ALTURA, list(EFLUENTE, INOCULO), mean))
with(blocos_dados_tempo_1, tapply(ALTURA, list(EFLUENTE, INOCULO), var))
with(blocos_dados_tempo_1, tapply(ALTURA, list(EFLUENTE, INOCULO), sd))

# Tamanho das Médias
T.medias <- with(blocos_dados_tempo_1,
                 model.tables(aov(ALTURA ~ CICLO + BLOCO + EFLUENTE + INOCULO +
                                    CICLO:EFLUENTE:INOCULO + CICLO:EFLUENTE +
                                    CICLO:INOCULO + EFLUENTE:INOCULO),
                              "means"))
T.medias

# Dividir os dados de acordo com o ciclo
#dados_tempo_1_ciclo_1 = subset(dados_tempo_1, CICLO == 1)
#dados_tempo_1_ciclo_1 = dados_tempo_1_ciclo_1[-4]
#dados_tempo_1_ciclo_2 = subset(dados_tempo_1, CICLO == 2)
#dados_tempo_1_ciclo_2 = dados_tempo_1_ciclo_2[-4]

# PSUBDIC - função de pacote criado para análises de parcelas subdivididas
# https://agronomiar.github.io/AgroR_Tutorial/parcela-subdividida.html#psubdbc
# https://rdrr.io/cran/AgroR/src/R/PSUBDIC_function.R
#with(dados_tempo_1_ciclo_1, PSUBDIC(EFLUENTE, INOCULO, BLOCO, ALTURA))
#with(dados_tempo_1_ciclo_2, PSUBDIC(EFLUENTE, INOCULO, BLOCO, ALTURA))

# Media dos blocos para fazer os testes de Normalidade e Homocedasticidade
media_blocos_tempo_1_backup = media_blocos_tempo_1
media_blocos_tempo_1 = aggregate(ALTURA ~ EFLUENTE + INOCULO + CICLO, 
                                 data = blocos_dados_tempo_1, FUN = mean)

# Realizar o teste Shapiro-Wilk no conjunto de dados completo
resultado_shapiro <- shapiro.test(media_blocos_tempo_1$ALTURA)$p.value

# Exibir o resultado do teste Shapiro-Wilk
print(resultado_shapiro)

# Interpretação do p-valor
if (resultado_shapiro > 0.05) {
  print("Os dados parecem seguir uma distribuição normal.")
} else {
  print("Os dados não parecem seguir uma distribuição normal.")
}

# Realizar o teste de Bartlett no conjunto de dados completo
resultado_bartlett <- bartlett.test(media_blocos_tempo_1$ALTURA, 
                                    media_blocos_tempo_1$EFLUENTE, 
                                    media_blocos_tempo_1$INOCULO,
                                    media_blocos_tempo_1$CICLO)$p.value

# Exibir o resultado do teste de Bartlett
print(resultado_bartlett)

# Interpretação do p-valor
if (resultado_bartlett > 0.05) {
  print("A variância é homogênea entre os grupos.")
} else {
  print("A variância não é homogênea entre os grupos.")
}

# Ajustar o modelo linear com as interações
modelo <- lm(ALTURA ~ BLOCO + CICLO * INOCULO * EFLUENTE, data = blocos_dados_tempo_1)

# Realizar a análise de variância
anova_result <- anova(modelo)

# Visualizar a tabela de análise de variância
print(anova_result)

# Resultados do modelo
summary(modelo)
coef(modelo)
dummy.coef(modelo)

# Análise dos Resíduos
res <- residuals(modelo)
res_stud <- rstandard(modelo)
round(head(data.frame(res,res_stud)),5)
boxplot(res_stud)
max(res_stud)

# resíduo stud x EFLUENTE
ggplot(blocos_dados_tempo_1,
       aes(x = EFLUENTE,
           y = res_stud)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Dose de efluente") +
  ylab("Resíduos estudentizado")

# resíduo stud x predito
ggplot(blocos_dados_tempo_1,
       aes(x = fitted(modelo),
           y = res_stud)) +
  geom_point() +
  xlab("Valores preditos") +
  ylab("Resíduos Studentizado")

# Resíduos com valores maiores que 2
res_stud[which(res_stud > 2)]

# Gráfico comparando a distribuição dos resíduos com a linha de uma teórica
# Distribuição Normal
qqnorm(res_stud,
       xlab="Quantis da distribuição normal",
       ylab="Resíduos Studentizados",
       main="")
qqline(res_stud,
       col = 2)
abline(0,1,col = 3)

# Ajustar modelo reduzindo, otimizando a relação número de variáveis no modelo
# e os valores AIC
modelo_inicial <- lm(ALTURA ~ BLOCO + CICLO * INOCULO * EFLUENTE, data = blocos_dados_tempo_1)

# Realizar a seleção de variáveis com o método stepwise
modelo_stepwise <- step(modelo_inicial)

# Visualizar o resultado do modelo final selecionado
summary(modelo_stepwise)

# Resultados do modelo
summary(modelo_stepwise)
coef(modelo_stepwise)
dummy.coef(modelo_stepwise)

# Análise dos Resíduos
res <- residuals(modelo_stepwise)
res_stud <- rstandard(modelo_stepwise)
round(head(data.frame(res,res_stud)),5)
boxplot(res_stud)
max(res_stud)

# resíduo stud x EFLUENTE
ggplot(blocos_dados_tempo_1,
       aes(x = EFLUENTE,
           y = res_stud)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Dose de efluente") +
  ylab("Resíduos estudentizado")

# resíduo stud x predito
ggplot(blocos_dados_tempo_1,
       aes(x = fitted(modelo_stepwise),
           y = res_stud)) +
  geom_point() +
  xlab("Valores preditos") +
  ylab("Resíduos Studentizado")

# Resíduos com valores maiores que 2
res_stud[which(res_stud > 2)]

# Gráfico comparando a distribuição dos resíduos com a linha de uma teórica
# Distribuição Normal
qqnorm(res_stud,
       xlab="Quantis da distribuição normal",
       ylab="Resíduos Studentizados",
       main="")
qqline(res_stud,
       col = 2)
abline(0,1,col = 3)






##### MODELOS PARCELA SUBDIVIDIDA #####

```{r}
modelo_BRUNO = lm(ALTURA ~ BLOCO + CICLO * INOCULO * EFLUENTE,
                  data = blocos_dados_tempo_1)

modelo_PARCELASUB = aov(ALTURA ~ BLOCO + CICLO + INOCULO * EFLUENTE + 
                          Error(BLOCO*INOCULO), data = blocos_dados_tempo_1)

modelo_X = aov(ALTURA ~ BLOCO + CICLO * INOCULO * EFLUENTE + 
                 Error(CICLO/BLOCO),
               data = blocos_dados_tempo_1)
#summary(modelo_BRUNO)
summary(modelo_PARCELASUB)
#summary(modelo_X)
```