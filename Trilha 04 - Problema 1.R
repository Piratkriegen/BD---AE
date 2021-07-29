# Carregando o tidyverse
library(tidyverse)

# Um gráfico de dispersão para os dados
# Criando as variáveis
x <- c(5.2, 5.1, 4.9, 4.6, 4.7, 4.8, 4.6, 4.9)
y <- c(13, 15, 16, 20, 19, 17, 21, 16)
# Criando um dataframe para a criação do gráfico
dfa <- data_frame(x = x, y = y) 
ggplot(data = dfa) + geom_point(aes(x = x, y = y)) 

# Determine o modelo de regressão linear simples entre as variáveis x e y, sendo y a variável resposta.
lm <- lm(y ~ x, data = dfa)

# Faça uma análise do modelo de regressão utilizando a função summary:
summary(lm)

# resíduos, significância estatística dos coeficientes, percentual de variância explicada pelo modelo.
par(mfrow = c(2,2))
plot(lm)

par(mfrow = c(1,1))

# Trace, no gráfico anterior, a reta de regressão.
lmplot <- ggplot(data = dfa) + geom_point(aes(x = x, y = y))
lmplot <- lmplot + geom_abline(intercept = lm$coefficients[[1]], slope = lm$coefficients[[2]] ) #, inherit.aes = FALSE)
print(lmplot)

