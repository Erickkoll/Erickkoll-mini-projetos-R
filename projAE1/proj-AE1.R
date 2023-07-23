setwd("C:/Users/erick/OneDrive/Área de Trabalho/Programação/Big Data Analytcs com R e Azure/projAE1")
getwd()

install.packages("readxl")
library(readxl)
library(Amelia)
library(dplyr)

dados <- read_xlsx("online-data-chapter-2-whr-2017.xlsx")
View(dados)
dim(dados)
str(dados)

missmap(dados, 
        main = "Titanic Training Data - Mapa de Dados Missing", 
        col = c("yellow", "black"), 
        legend = FALSE)

dados <- dados %>% 
  select(1:17,19)
str(dados)

dados <- na.omit(dados)
dim(dados)

missmap(dados, 
        main = "Titanic Training Data - Mapa de Dados Missing", 
        col = c("yellow", "black"), 
        legend = FALSE)
View(dados)

boxplot(dados$`Healthy life expectancy at birth`, main = "Boxplot expectativa de vida", ylab = "idade")
boxplot(dados$`Life Ladder`, main = "Boxplot escala de vida", ylab = "idade")
boxplot(dados$`Log GDP per capita`, main = "Boxplot GDP per capita", ylab = "GDP per capita")

hist(dados$`Log GDP per capita`, main = "Histograma GDP per capita", xlab = "GDP per capita")

# Usando o preço como variável dependente (y)
plot(x = dados$`Log GDP per capita`, y = dados$`Healthy life expectancy at birth`,
     main = "Expectativa de vida por GPD per Capita",
     xlab = "GPD per capita",
     ylab = "Expectativa de vida")

correlação_GPD_HL <- cor(dados$`Log GDP per capita`,dados$`Healthy life expectancy at birth`, method = "pearson")
correlação_LL_PC <- cor(dados$`Life Ladder`,dados$`Perceptions of corruption`, method = "pearson")

# Find the country with the lowest social support
country_lowest_social_support <- dados %>% 
  filter(`Social support` == min(`Social support`, na.rm = TRUE)) %>% 
  pull(country) %>% 
  .[1]

# Find the country with the highest perception of corruption
country_highest_perception_corruption <- dados %>% 
  filter(`Perceptions of corruption` == max(`Perceptions of corruption`, na.rm = TRUE)) %>% 
  pull(country) %>% 
  .[1]

# Check if the country with the lowest social support is the same as the country with the highest perception of corruption
country_lowest_social_support == country_highest_perception_corruption

plot(x = dados$`Social support`, y = dados$`Perceptions of corruption`,
     main = "Support social pela percepção da corrupção",
     xlab = "Support social",
     ylab = "Perpecção da corrupção")

correlação_SS_PC <- cor(dados$`Social support`,dados$`Perceptions of corruption`, method = "pearson")
correlação_SS_PC

plot(x = dados$Generosity, y = dados$`Life Ladder`,
     main = "média da felicidade das pessoas pela generosidade",
     xlab = "Generosidade",
     ylab = "Média da felicidade")

correlação_G_LL <- cor(dados$Generosity,dados$`Life Ladder`, method = "pearson")
correlação_G_LL