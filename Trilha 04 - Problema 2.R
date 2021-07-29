# Faça a importação dos dados, verifique a estrutura e faça um sumário estatístico.

library(tidyverse)
pib_gapminder <- read.csv("c:/users/aivo2/downloads/pib_gapminder.csv")
str(pib_gapminder)
pib_gapminder %>%
  mutate_if(is.factor, as.character) -> pib_gapminder

summary(pib_gapminder)

# Classifique cada variável de acordo com seu tipo (qualitativa ordinal, nominal, quantitativa discreta, contínua, etc).

# Faça uma tabela de frequência absoluta e uma tabela de frequência relativa para verificar o número de observações por continente.
pib_gapminder %>%
  count(continente) %>%
  mutate(prop = prop.table(n)) -> Continentes
print(Continentes)

# Faça um gráfico de barras da tabela de frequência absoluta dos continentes.
ggplot(data = pib_gapminder) + 
  geom_bar(aes(x = continente, fill = continente), show.legend=FALSE)

ggplot(data = pib_gapminder) + geom_bar(mapping = aes(x = continente, y = ..prop.., group=1), show.legend = FALSE) +
  xlab("") + ylab("") + 
  ggtitle("Frequência Relativa de Observações por Continente") +
  theme_light()

# Faça um gráfico apropriado para relacionar o PIB per capta à expectativa de vida.
ggplot(data = pib_gapminder) + 
  geom_point(mapping = aes(y = pibPercap, x = expVida, color = continente)) + 
  xlab("Expectativa de Vida") + ylab( "PIB per capta")+
  ggtitle("Relacão entre PIB per capta e Expectativa de Vida")

# Crie duas novas colunas nesta base de dados com o logaritmo de PIB per capta, e o logaritmo da expectativa de vida. Estas colunas devem ter os nomes: lpibPercap e lexpVida, respectivamente.
pib_gapminder %>%
  mutate(lpibPercap = log(pibPercap), lexpVida = log(expVida)) -> pib
head(pib)

# Faça um gráfico apropriado para relacionar estas duas novas variáveis
ggplot(data = pib) + 
  geom_point(mapping = aes(y = lexpVida, x = lpibPercap, color = continente)) + 
  labs(color = "", y = "Log da Expectativa de Vida", x = "Log do PIB per capta")

# Ajuste um modelo linear aos dados, utilizando as duas novas variáveis criadas, sendo lexpVida a variável resposta
mod <- lm(lexpVida ~ lpibPercap, data=pib)
summary(mod)

par(mfrow=c(2,2))
plot(mod)

