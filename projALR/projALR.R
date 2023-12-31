setwd("C:/Users/erick/OneDrive/Área de Trabalho/Programação/Big Data Analytics com R e Azure/min_projetos_R/projALR")
getwd()

dados <- read.csv("dataset.csv")
View(dados)

install.packages("sqldf")
library(sqldf)
library(Amelia)
library(dplyr)

missmap(dados, 
        main = "Titanic Training Data - Mapa de Dados Missing", 
        col = c("yellow", "black"), 
        legend = FALSE)

summary(dados)
dados <- na.omit(dados)

# Quantidade dos diferentes tipos de raça

type_race <- sqldf("SELECT COUNT(DISTINCT RACE) AS quantidade_unica from dados")
type_race

# Media de idade dos pacientes

media_idade <- sqldf("SELECT AVG(AGE) AS media_idade from dados")
media_idade

table(dados$AGE)

#Moda da idade dos pacientes

moda_idade <- sqldf("SELECT AGE AS moda_idade, COUNT(AGE) AS frequencia FROM dados GROUP BY AGE ORDER BY COUNT(AGE) DESC LIMIT 1")
moda_idade

#Variância

var_idade <- sqldf("SELECT VAR_SAMP(AGE) as var_idade FROM dados")
var_idade

#Gasto das internações por idade

gasto_p_idade <- sqldf("SELECT AGE AS idade, SUM(TOTCHG) AS gasto_por_idade 
                       FROM dados 
                       GROUP BY AGE")
gasto_p_idade

#Idade que gera maior gasto com iternações hospitalares

maior_gasto_idade <- sqldf("SELECT AGE AS idade, SUM(TOTCHG) AS gasto_por_idade 
                           FROM dados 
                           GROUP BY AGE 
                           ORDER BY gasto_por_idade DESC 
                           LIMIT 1")
maior_gasto_idade

#Gasto das internações por sexo

gasto_p_genero <- sqldf("SELECT FEMALE AS genero, SUM(TOTCHG) AS gasto_por_genero 
                       FROM dados 
                       GROUP BY FEMALE")
gasto_p_genero

#Gasto por médio por raça

gasto_p_raça <- sqldf("SELECT RACE AS raça, AVG(TOTCHG) AS gasto_medio_raça 
                       FROM dados 
                       GROUP BY RACE")
gasto_p_raça

#Media de gastos hospitalares por idade para pacientes com mais de 10 anos

gasto_p_idade <- sqldf("SELECT AGE AS idade, AVG(TOTCHG) AS media_gasto_por_idade 
                       FROM dados
                       WHERE AGE > 10
                       GROUP BY AGE")
gasto_p_idade

#Gasto por idade com crianças maiores que 10 anos e gasto superior a 3000

gasto_p_idade <- sqldf("SELECT AGE AS idade, AVG(TOTCHG) AS media_gasto_por_idade 
                       FROM dados
                       WHERE AGE > 10
                       GROUP BY AGE
                       HAVING AVG(TOTCHG) > 3000")
gasto_p_idade

#Qual a distribuição da idade dos pacientes que frequentam o hospital?


hist(dados$AGE, main= "Histograma da distribuição da idade", xlab = "idade")

#Qual a faixa etária tem o maior gasto total no hospital?

menor_idade <- sqldf("SELECT AGE AS idade,
                     SUM(TOTCHG) AS valor
                     FROM dados
                     GROUP BY AGE
                     ORDER BY valor DESC
                     LIMIT 1")
menor_idade
  
#Qual grupo baseado em diagnóstico(APRDRG) tem o maior gasto no hospital?

grupo_APRDRG_gasto <- sqldf("SELECT APRDRG AS grupo,
                            SUM(TOTCHG) AS valor
                            FROM dados
                            GROUP BY APRDRG
                            ORDER BY valor DESC
                            LIMIT 1")

#A raça do paciente tem relação com os gastos hosítalares?

# Usaremos um Teste ANOVA.
# Variável dependente no Teste ANOVA: TOTCHG
# Variável independente no Teste ANOVA: Race

# H0: Não há efeito de RACE em TOTCHG.
# H1: Há efeito de RACE em TOTCHG.

cor(dados$RACE, dados$TOTCHG, method = "pearson")
summary(dados$RACE)
summary(as.factor(dados$RACE))
modelo_anova_1 <- aov(TOTCHG ~ RACE, data = dados)
summary(modelo_anova_1)

# Resposta da Pergunta 4:
# O valor-p é maior que 0.05. Falhamos em rejeitar a H0.
# A raça do paciente não influencia no gasto total com internação no hospital.


#A combinação de idade e gênero dos pacientes influencia no gasto total em internações no hospital?

modelo_anova_2 <- aov(TOTCHG ~ AGE + FEMALE, data = dados)
summary(modelo_anova_2)

# Resposta da Pergunta 4:
# O valor-p é menor que 0.05. Reheitamos a H0.
# A idade e o sexo influencia no gasto total com internação no hospital.

#Como o tempo de permanência é o fator crucial para paciente internados, 
#desejamos descobrir se o tempo de permanência pode ser previsto a partir da idade, gênero e raça

modelo_lr <- lm(LOS ~ AGE + FEMALE + RACE, data = dados)
summary(modelo_lr)

#Quais variável tem mais impacto nos custos de internação hospitalar?

modelo_lr_v1 <- lm(TOTCHG ~ ., data = dados)
summary(modelo_lr_v1)

modelo_lr_v2 <- lm(TOTCHG ~ AGE + LOS + APRDRG, data = dados)
summary(modelo_lr_v2)

modelo_lr_v3 <- lm(TOTCHG ~ LOS + APRDRG, data = dados)
summary(modelo_lr_v3)

modelo_lr_v3 <- lm(TOTCHG ~ LOS, data = dados)
summary(modelo_lr_v3)

modelo_lr_v3 <- lm(TOTCHG ~ APRDRG, data = dados)
summary(modelo_lr_v3)

#Logo o tempo de internação é a variável com maior impacto
#No entando para um melhor modelo o melhor é analisar as variaveis tipo de diagnóstico, tempo de internação e idade
