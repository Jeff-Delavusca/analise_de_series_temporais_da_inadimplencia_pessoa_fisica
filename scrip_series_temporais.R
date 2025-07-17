rm(list = ls())

library(tidyverse)
library(lubridate)
library(ggplot2)



df <- read.csv2('inadimplencia_pf_br.csv', encoding = "latin1") %>% 
  rename(inadimplencia = X21084...Inadimplência.da.carteira.de.crédito...Pessoas.físicas...Total....
         ) %>% 
  mutate(
    
    # Substitui a vírgula por ponto e converte para numérico
    inadimplencia = as.numeric(str_replace(inadimplencia, ",", ".")),
    
    # Adiciona "01/" para representar o primeiro dia do mês
    Data = paste0("01/", Data),
    
    # Converte para data reconhecendo meses em português
    Data = dmy(Data, locale = "pt_BR") # lubridate reconhece meses como "jan", "fev", etc.
)


# paste0("01/", data) adiciona o dia 1 em todas as datas → ex: "jan/2004" vira "01/jan/2004"
# dmy(..., locale = "pt_BR") interpreta corretamente os nomes de meses em português
# inadimplencia = as.numeric(...) converte o valor de texto com vírgula para número com ponto decimal.


# Verificando classes das colunas
glimpse(df)

any(is.na(df))
sum(any(is.na(df)))
df[!complete.cases(df), ]
df <- df %>% drop_na()

ggplot2::ggplot(df, aes(x = Data, y = inadimplencia)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Inadimplência de Pessoas Físicas no Brasil",
    subtitle = "Taxa de Inadimplência (%)",
    y  = "",
    caption = "Fonte: Sistema Gerenciador de Séries Temporais (BCB)"
  )
  theme_minimal()



serie_ts <- ts(df$inadimplencia, start =c(2011, 3), frequency = 12)


plot(serie_ts, 
     main = "Série Temporal da Inadimplência",
     ylab = "Inadimplência (%)",
     xlab = "Ano")
  
  
  
decomposicao <- decompose(serie_ts)  
plot(decomposicao)  


decomposicao_stl <- stl(serie_ts, s.window = "periodic")
plot(decomposicao_stl)  
  

# Interpretação da decomposição da série

# 1. Data (série original)

## A série apresenta um comportamento oscilatório ao longo do tempo, com picos visíveis.
## Observa-se um aumento da inadimplência no início do período (2011-2013),
## uma queda gradual até aproximadamente 2018, depois uma estabilidade relativa 
## com pequenos picos em 2020 e um leve aumento no final da série.


# 2. Sazonalidade (Seasonal)

## O componente sazonal mostra um padrão que se repete todo ano, com picos e quedas regulares.
## Isso indica que a inadimplência tem uma sazonalidade clara, provavelmente influenciada por
## fatores econômicos ou comportamentais que ocorrem em determinados meses do ano.
## O padrão é bastante estável ao longo dotempo, reforçando a sazonalidade periódica.


# 3. Tendência (Trend)

## A tendência apresenta um movimento de queda do nível geral da inadimplência de 2011 até cerca
## de 2018, seguido de um período mais estável com leve alta a partir de 2022.
## Há um pico na tendência próximo a 2020, possivelmente refletindo impactos econômicos desse 
## período (ex: pandemia).
## Isso mostra que a inadimplência passou por fases distintas ao longo dos anos.


# 4. Resíduos (Remainder)

## Os resíduos mostram a parte da série que não é explicada pela sazonalidade ou pela tendência.
## Eles parecem ter variações pequenas, mas há alguns picos isolados, indicando eventos atípicos
## ou ruídos não explicados pelos componentes.
## Os resíduos são importantes para avaliar se a decomposição capturou bem os padrões da série.


# Resumo:

## A inadimplência apresenta uma tendência geral decrescente até 2018, seguida por uma estabilização
## e leve aumento recente, com sazonalidade anual bem definida e estável ao longo do tempo. Alguns 
## eventos extraordinários causam picos nos resíduos, indicando que há fatores específicos não capturados
## pela tendência e sazonalidade.
  
  
# Verificar a estacionariedade da série: Teste de Dickey-Fuller Aumentado (ADF)

if (!require("tseries")){install.packages("tseries")}
if (!require("urca")){install.packages("urca")}
library(tseries)
library(urca)

###### Testes de estacionariedade

##### Teste de Dickey-Fuller Aumentado (ADF)

## Hipótese nula (H0): A série não é estacionária (tem raiz unitária)
## Queremos Rejeitar H0 (p-valor < 0.05): estacionária

##### Teste KPSS

## Hipótese nula (H0): A série é estacionária (em torno de uma média ou tendência determinística)
## Queremos: Não rejeitar H0 (p-valor > 0.05): estacionária

##### Teste Phillips-Perron

## Similar ao ADF, mas com robustez contra autocorrelação e deterocedasticidade
## Hipótese nula (H0): Série não é estacionária
## Queremos: Rejeitar H0 (p-valor < 0.05): estacionária

adf.test(serie_ts)
kpss.test(serie_ts)
pp.test(serie_ts)


# Aplicar a primeira diferença e analisar os testes novamente

serie_diff <- diff(serie_ts)

adf.test(serie_diff)

# Interpretaçao ADF

## O p-valor = 0.0681, ou seja, acima de 5%, mas abaixo de 10%.
## A 5% de significância, não conseguimos rejeitar H0, então a série pode ser não estacionária.
## A 10% de significância, rejeita-se H0 com alguma cautela, pode-se considerar estacionária com fraca evidência.
## Logo, na primeira diferença ainda há estacionariedade fraca.

kpss.test(serie_diff)

# Interpretaçaõ KPSS

## o p-valor > 0.1 (a função só imprime até 0.1)
## Aceita-se H0 com tranquilidade. A série é estacionária segundo o KPSS

pp.test(serie_diff)

# Interpretação

## O p-valor = 0.01, forte evidência contra H0
## Portanto, rejeita-se H0, a série é estacionária
## O PP diz com clareza que a série é estacionária


# Analisar o gráfico da primeira diferença

plot(serie_diff,
     main = "Série Temporal Diferenciada (1ª ordem)",
     ylab = "Variação da Inadimplência (%)",
     xlab = "Ano")
abline(h = mean(serie_diff, na.rm = TRUE), col = "red", lty = 2)


# Aplicar a segunda diferença e analisar o teste ADF novamente

serie_diff2 <- diff(serie_diff)

adf.test(serie_diff2)
kpss.test(serie_diff2)
pp.test(serie_diff2)

# Interpretação dos testes 

## Os testes ADF e PPO rejeitaram a hipótese nula de raiz unitária.
## O KPSS não rejeita a hipótese nula de estacionariedade.
## Como os três convergem (com alta margem de segurança), não há ambiguidade:
## A série está estacionária depois da segunda diferenciação.


# Analisar o gráfico da segunda diferença

plot(serie_diff2,
     main = "Série Temporal Diferenciada (2ª ordem)",
     ylab = "Variação da Inadimplência (%)",
     xlab = "Ano")
abline(h = mean(serie_diff, na.rm = TRUE), col = "red", lty = 2)


# Analisar a auto-correlação e auto-correlação parcial da série com 2ª diferença

acf(serie_diff2, main = "ACF - Segunda Diferença", lag.max = 30)
pacf(serie_diff2, main = "PACF - Segunda Diferença", lag.max = 30)


# Rodar os modelos escolhidos

# modelo ARIMA (2,2,0) - autorregressivo de ordem e e duas diferenças, sem média móvel
modelo_arima_220 <- arima(serie_ts, order = c(2, 2, 0))
summary(modelo_arima_220)

# modelo ARIMA (0,2,2) - duas diferenças, média móvel de ordem 2 e sem componente autorregressivo
modelo_arima_022 <- arima(serie_ts, order = c(0, 2, 2))
summary(modelo_arima_022)

# modelo ARIMA (2,2,2) - autorregressivo de 2 ordegm, duas diferenças, média móvel de ordem 2
modelo_arima_222 <- arima(serie_ts, order = c(2, 2, 2))
summary(modelo_arima_222)

# Automatizar a escolha do modelo para comparação
library(forecast)
modelo_auto <- auto.arima(serie_ts, d = 2, stepwise = FALSE, approximation = FALSE)
summary(modelo_auto)

# Testei três modelos de forma manual e um automático, ou seja, deixando para o R selecionar o que melhor de adapta aos dados
# O que apresentou o melhor desempenho foi o modelo auto.

### Analisar os resíduos desse modelo

# teste Ljung-Box: autocorrelação dos resíduos
Box.test(resid(modelo_auto), type = "Ljung-Box")

# Esse teste verifica autocorrelação nos resíduos. A hipótese nula (H0) é que não há correlação, 
# ou seja, os resíduos são independentes (ruído branco)

## Resultados:
# Estatística do teste: 0.4261
# Graus de liberdade (df): 1
# Valor-p (p-value): 0.5139

## Interpretação:
# Como o valor-p é alto (0.51 > 0.05), não rejeitamos H0. Isso significa que não há evidências
# de autocorrelação nos resíduos do modelo.
# O modelo ARIMA escolhido capturou bem a estrutura da série.
# Os resíduos se comportam como ruído branco.
# isso valida a qualidade estatística do ajuste.

#Plot ACF dos resíduos
acf(resid(modelo_auto), main = "ACF dos resíduos")

# Histograma e Q-Q plot dos resíduos
hist(resid(modelo_auto), main = "Histograma dos resíduos", xlab = "Resíduos")
qqnorm(resid(modelo_auto))
qqline(resid(modelo_auto))
