setwd("C:/Users/erick/OneDrive/Área de Trabalho/Programação/Big Data Analytcs com R e Azure/projALR")
getwd()

dados <- read.csv("dataset.csv")
View(dados)

install.packages("sqldf")
library(sqldf)

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



