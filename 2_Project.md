# 0. Descrição do projeto
Descrição: Neste projeto analisamos dados diários da concentração de monóxido de carbono de estações de monitoramento da qualidade do ar, obtidos através do site [Data.Rio](http://www.data.rio). Foi realizada uma análise exploratória (EDA), além da previsão de quatro modelos distintos, sendo eles o primeiro o de suavização exponencial Holt-Winters, o segundo Prophet (desenvolvido pelo Facebook), o terceiro Redes Neurais (Hyndman) e o modelo ARIMA. Utilizou-se validação cruzada (cross-validation) para cálculo dos *mapes* (mean absolute percentage error), métrica para avaliação da acurácia dos modelos.

# 1. Análise Exploratória 

## 1.1 Pacotes utilizados na análise
```{r, message = F}
##Leitura dos pacotes
library(anomalize) #0.1.1
#Anomalize é um pacote para detecção de anomalias.
library(tidyverse) #1.2.1
#Pacote para manipulação de dados de uma forma mais fácil
library(data.table) #1.12.0
#Pacote para leitura de dados mais rápido
library(lubridate) #1.7.4
#Pacote para tratamento de datas
library(magrittr) #1.5
#Pacote para uso do pipe que torna a programação mais fluida
library(mice) #3.5.0
#Pacote para imputação de dados
library(fpp2) #2.3
#Pacote para modelagem estatística
library(mFilter) #0.1.4
#Pacote para decomposição da série temporal
library(prophet) #0.5
#Pacote para o modelo de previsão desenvolvido pelo Facebook
library(DT) #0.5
#Pacote para tabelas mais reproduzíveis
library(urca) #1.3.0
#Pacote para testes estatísticos
library(BETS) #0.4.9
#Pacote para modelagem estatística, sobretudo de séries temporais
library(gridExtra) #2.3
#Pacote para agregar diversos gráficos numa única imagem
library(FinTS) #0.4.6
#Pacote para uso de teste estatístico
library(forecast) #8.7
#Pacote para modelagem de séries temporais e demais modelos
library(stats) #3.5.3
#Pacote nativo do R com diversas funções estatísticas, incluindo modelos simples como Holt-Winters
```

## 1.2 Leitura dos dados
```{r, warning=FALSE}
dados <- data.table::fread("C:/Users/guilh/Desktop/cry1/Dados_horarios_do_monitoramento_da_qualidade_do_ar__MonitorAr.csv",
                           header = T, sep = ",", encoding = 'UTF-8')
df <- dados %>% dplyr::select(Data, CO, Estação)
df$Data %<>% as_datetime()
```

## 1.3 Boxplot da concentração de CO por Estação meteorológica
```{r, fig.align='center'}
df %>% 
  na.omit() %>% 
  ggplot(aes(x = as.factor(Estação), y = CO)) + geom_boxplot() + theme_bw() +
  labs(x = 'Stations', y = 'CO concentration (ppm)', title = 'CO concentration Box-Plot by fixed station') +
  theme_bw()
```
![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/boxplot_fixeds_station.png)

## 1.4 Mediana das concentrações de CO por Estação
```{r, fig.align='center'}
df %>% 
  dplyr::filter(!Estação  == 'PG') %>% 
  group_by(Estação) %>% 
  summarise(Concentracao_CO = median(CO, na.rm = TRUE)) %>% 
  arrange(desc(Concentracao_CO)) %>% 
  DT::datatable(rownames = FALSE, options = list(
    columnDefs = list(list(className = 'dt-center', targets = 0:1))
  ))
```
![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/concentracao_co_estacao.png)

## 1.41 Resumo de cada Estação - Base Crua

```{r}
estations <- unique(df$Estação)
lista <- list()

for(i in 1:length(estations)) {
  lista[[i]] <- df %>% 
    dplyr::filter(Estação == estations[i]) %>% 
    dplyr::select(CO) %>% 
    summary()
  print(paste('Estação', estations[i], unlist(lista[[i]]),"  "))
}
```

## 1.5 Esboço de uma série temporal escolhendo a estação IR, SP e CA
Foram selecionadas as estações IR, SP e CA por terem medianas semelhantes.

```{r}
IR_plot <- df %>% 
  na.omit() %>% 
  dplyr::filter(Estação == 'IR') %>% 
  dplyr::select(CO) %>% 
  ts()
```

## 1.61 Imputando a mediana nos missing values de cada estação

```{r}
IR_mediana_plot <- df %>% 
  dplyr::filter(Estação == 'IR') %>% 
  mutate(CO = ifelse(is.na(CO), 0.39, CO)) %>% 
  dplyr::select(CO) %>% 
  ts()
```

* Após a imputação pela mediana, a média teve uma pequena queda de 0.4481 para 0.4463.
```{r}
cbind(IR_plot, IR_mediana_plot) %>% 
  summary()
```

## 1.7 Gráfico das séries temporais das três estaçoes selecionadas com imputação da mediana nos missing values
```{r, fig.align='center'}
par(mfrow = c(3,1))
plot(IR_plot, xlab = '', main = 'Concentração de CO na Estação de IR - Missing values excluídos')
plot(IR_mediana_plot, xlab = '', main = 'Concentração de CO na Estação de SP - Missing values imputados pela mediana')
```
![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/missing_values_estacao.png)

```{r, fig.align='center', warning = FALSE, message = FALSE}
##1.81 Detecção de anomalia - Parte 1
#Utilizando diferença interquartílica IQR.
#df %>% 
#  dplyr::filter(Estação == 'IR') %>% 
#  mutate(CO = ifelse(is.na(CO), 0.39, CO)) %>% 
#  dplyr::mutate(Data = as_datetime(Data)) %>% 
#  rename(date = Data, count = CO) %>% 
#  dplyr::select(-Estação) %>%
#  as.tibble() %>% 
#  time_decompose(count, method = "stl") %>%
#  anomalize(remainder, method = "iqr") %>%
#  time_recompose() %>% 
#  #dplyr::filter(anomaly == 'Yes') %>% 
#  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.25) +
#  labs(title = "Anomalias detectadas 1020 de 54067 observações - Aprox. 1.89%", subtitle = "STL + IQR Methods") 
```

## 1.8 Detecção de anomalia
Utilizando a detecção de anomalia do pacote AnomalyDetection (Twitter)
A detecção de anomalia foi feita na base hora a hora.

```{r, fig.align='center', warning = FALSE, message = FALSE}
#Tabela de detecção de anomalia
tabela_anomalo <- df %>% 
  dplyr::filter(Estação == 'IR') %>% 
  mutate(CO = ifelse(is.na(CO), 0.39, CO)) %>% 
  dplyr::mutate(Data = as_datetime(Data)) %>% 
  rename(date = Data, count = CO) %>% 
  dplyr::select(-Estação) %>%
  as.tibble() %>% 
  # Twitter + GESD
  time_decompose(count, method = "twitter", trend = "2 months") %>%
  anomalize(remainder, method = "gesd") %>%
  time_recompose()
```

```{r, fig.align='center'}
#Gráfico das anomalias
tabela_anomalo %>% 
  plot_anomalies(time_recomposed = TRUE) +
  labs(title = "Anomalias detectadas 1061 de 55785 observações - Aprox. 1.9%", subtitle = "Twitter + GESD Methods")
```
![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/deteccao_anomalia.png)

## 1.81 **Transformo a base df já com as imputações nos valores anômalos**
```{r}
df <- df %>% 
  dplyr::filter(Estação == 'IR') %>% 
  bind_cols(tabela_anomalo %>% dplyr::select(anomaly, observed)) %>% 
  dplyr::mutate(CO = ifelse(anomaly == 'No', CO, 0.39),
                CO = ifelse(is.na(CO), 0.39, CO)) %>% 
  dplyr::select(-anomaly, -observed)
```

# 1.811 Tabela de valores anômalos por Ano
```{r}
df %>% 
  dplyr::filter(Estação == 'IR') %>% 
  bind_cols(tabela_anomalo %>% 
              dplyr::select(anomaly, observed)) %>% 
  mutate(Ano = year(Data),
         Anomalia = ifelse(anomaly == 'No', 'Nao', 'Sim')) %>% 
  group_by(Ano) %>% 
  count(Anomalia) %>% 
  rename(Contagem = n) %>% 
  DT::datatable( rownames = FALSE, options = list(
    columnDefs = list(list(className = 'dt-center', targets = 0:2))
  ))
```
![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/tabela_anomalia.png)

## 1.82 Estatística descritiva depois da imputação dos dados da Estação IR
```{r}
estations <- unique(df$Estação)
lista <- list()

for(i in 1:length(estations)) {
  lista[[i]] <- df %>% 
    dplyr::filter(Estação == estations[i]) %>% 
    dplyr::select(CO) %>% 
    summary()
  print(paste('Estação', estations[i], unlist(lista[[i]]),"  "))
}
```

## 1.9 Utilizando CO da Estação IR para modelar
```{r}
#dados_IR <- df %>% 
#  dplyr::filter(Estação == 'IR') %>% 
#  mutate(CO = ifelse(is.na(CO), 0.39, CO),
#         Data_novo = lubridate::ymd_hms(Data)) %>% 
#  dplyr::select(-Estação)
```

------------------------------

## 2.0 Agregando os dados para diário
Os dados foram imputados na Estação Irajá
```{r}
dados_diarios <- df %>% 
  dplyr::filter(Estação == 'IR') %>% 
  mutate(CO = ifelse(is.na(CO), 0.39, CO),
         Data = as.character(Data),
         Data = substring(Data, 1, 10),
         Data = lubridate::ymd(Data)) %>% 
  group_by(Data) %>% 
  summarise(CO_dia_media = mean(CO),
            CO_dia_mediana = median(CO)) %>% 
  ungroup()

#Data.frame utilizado para a construção de todos os modelos
dados_filtro <- dados_diarios %>% 
  mutate(dia_da_semana = lubridate::wday(Data, label = T),
         ano = lubridate::year(Data)) %>% 
  dplyr::filter(ano >= 2015) %>% 
  arrange(Data)
```

## 2.1 Concentração **média** de CO na estação Irajá (dados agregados por dia)
Há uma queda na concentração de monóxido de carbono de 2015 a 2018.

```{r, fig.align='center'}
dados_filtro %>% 
  mutate(Ano = as.factor(ano)) %>% 
  ggplot(aes(x = Data, y = CO_dia_media, color = dia_da_semana)) + geom_line() +
  facet_wrap(~dia_da_semana) +
  theme_bw() + labs(x = 'Tempo', y = 'CO', 
                    title = 'Concentração média de CO na estação Irajá', subtitle = 'Dados diários por dia da semana') +
  theme(legend.position = "bottom")
```
![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/concentracao_co_iraja.png)

## 2.2 Separando os dados em base de treino e teste
```{r}

treino <- dados_filtro[1:1200,]
teste <- dados_filtro[1201:1263,]

#serie_treino <- ts(data = treino$CO_dia_media, start = as.Date('2012-01-01'), frequency = 365)
#serie_teste <- ts(data = teste$CO_dia_media, start = as.Date('2018-05-16'), frequency = 365)
inds <- treino$Data
inds_teste <- teste$Data

serie_treino <- ts(treino$CO_dia_media ,start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365)
serie_teste <- ts(teste$CO_dia_media ,start = c(2018, as.numeric(format(inds_teste[1], "%j"))), frequency = 365)
```

## 2.3 Decomposição da série
```{r, fig.align='center', echo = FALSE}
decomp <- decompose(serie_treino, type = 'additive')
plot(decomp)
```
![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/decomp_timeseries.png)

## 2.4 Ajuste sazonal
#Aqui é uma tentativa de retirar a variação sazonal, restando apenas as componentes de tendência e erro.
```{r, fig.align='center', echo = FALSE}
ajuste_sazonal = serie_treino - decomp$seasonal
plot(ajuste_sazonal, xlab = ' ', ylab = 'Ajuste sazonal', main = 'Ajuste sazonal da série')
```

![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/remocao_sazonalidade.png)

------------------------------

## 3.0 Modelos
Serão 4 os modelos de previsão:
  1. Holt Winters (Suavização Exponencial de Holt Winters)
2. Prophet (Facebook)
3. ARIMA
4. Modelos de redes neurais

## 3.10 Suavização exponencial sazonal de Holt Winters
Este modelo é apropriado para séries que apresentam comportamento sazonal.
Os parâmetros alpha, beta e gamma representam respectivamente os parâmetros de suavização para o nível, tendência e sazonalidade.

```{r, fig.align='center'}
ajuste_com_sazonalidade <- HoltWinters(serie_treino)

plot(serie_treino, xlab = 'Tempo', ylab = 'Concentração de CO', main = 'Ajuste do Modelo de Suavização Exponencial - Holt-Winters')
lines(fitted(ajuste_com_sazonalidade)[,1], lwd = 0.5, col = 'red')
legend(15344, 2, c('Concentração de CO', 'Ajuste Holt Winters'), lwd = c(1,2),
       col = c('black', 'red'), bty = 'n')
```
![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/ajuste_suavizacao_exponencial.png)

## 3.11 Parâmetros do modelo Holt-Winters
Lembrando que os parâmetros alpha, beta e gamma representam respectivamente os parâmetros de suavização para o nível, tendência e sazonalidade.
Alpha = 0.1852959
Beta = 0
Gamma = 0.4565081

```{r}
ajuste_com_sazonalidade$alpha
ajuste_com_sazonalidade$beta
ajuste_com_sazonalidade$gamma
```

##3.12 Previsão do modelo de Holt Winters 7 passos à frente com intervalo de predição de 95%
```{r, fig.align='center'}
prev_hw = forecast(ajuste_com_sazonalidade, h = 7, level = 95)
plot(prev_hw, xlab = 'Tempo', ylab = 'Concentração de CO', main = 'Previsão do modelo Holt Winters')
```
![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/previsao_holtwinters_grafico.png)

## 3.13 Tabela de previsão e MAPE

```{r, warning = FALSE}
serie_teste_top7 <- serie_teste %>% as.tibble() %>% top_n(7) %>% rename(Serie_original = x)

previsao_hw <- prev_hw %>% 
  as.tibble() %>% 
  dplyr::rename(Previsao = `Point Forecast`) %>% 
  mutate(Valor_Real = serie_teste[1:7],
         Erro = round(abs((Previsao - Valor_Real)/Valor_Real),4),
         Valor_Real = round(Valor_Real,3),
         Previsao = round(Previsao, 4)) %>% 
  dplyr::select(Valor_Real,Previsao,  Erro)

mape <- mean(previsao_hw$Erro)

previsao_hw %>% 
  DT::datatable( rownames = FALSE, options = list(
    columnDefs = list(list(className = 'dt-center', targets = 0:2))
  ))
```

![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/previsao_holtwinters.png)

# 3.14 Cross-validation Holt-Winters
Mape de 31%.

```{r}
lista_mape <- list()

for (i in 1:10) {
  train_series <- ts(dados_filtro$CO_dia_media[1:(1199+i)] ,start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365)
  #Ajuste do modelo
  fit <- HoltWinters(train_series)
  #Previsão 7 passos à frente
  previsao_fit <- forecast(fit, h = 7, level = 95)
  
  test_series <-  dados_filtro$CO_dia_media[(1200+i):(1206+i)]
  
  MAPE_da_serie <- previsao_fit %>% 
    data.frame() %>% 
    rename(Previsao = Point.Forecast) %>% 
    mutate(Valor_observado = test_series,
           Erro = abs((Valor_observado - Previsao)/Valor_observado)) %>% 
    summarise(MAPE = mean(Erro))
  
  #Armazena o Mape da série
  lista_mape[[i]] <- MAPE_da_serie$MAPE
  
}
unlist(lista_mape)
mean(unlist(lista_mape))
```

## 3.15 Modelagem com Prophet (Facebook)
*Utilizando a sazonalidade da série
```{r}
stats <- treino %>% 
  rename(ds = Data, y = CO_dia_media) %>% 
  dplyr::select(-CO_dia_mediana, -ano, -dia_da_semana)

m <- prophet(stats, daily.seasonality = TRUE)
future <- make_future_dataframe(m, periods = 7)
previsao_prophet <- stats::predict(m, future)
```

## 3.16 Dados ajustados utilizando o pacote Prophet (Facebook)
```{r, fig.align='center'}
plot(m, previsao_prophet) + theme_bw() +
  labs(x = 'Anos', y = 'Concentração de CO', title = 'Previsão Prophet - Concentração de CO na Estação Irajá',
       subtitle = 'Dados da concentração média diária')
```

![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/previsao_prophet.png)

## 3.17 Análise das componentes do modelo
* Mais uma vez há a confirmação da queda da concentração de CO de 2015 a 2018.
* Entre maio e agosto estão as maiores concentrações de CO.
* A concentração de CO alcança os maiores níveis na sexta e no sábado.
* Entre 4 e 5 da manhã e depois entre 19 e 20h registram-se as maiores concentrações de CO.

```{r, fig.align='center'}
prophet_plot_components(m, previsao_prophet)
```

## 3.18 Tabela Previsão e MAPE
Abaixo a tabela com os dados
```{r}
tabela <- previsao_prophet %>% 
  tail(7) %>% 
  cbind(teste[1:7,]) %>% 
  dplyr::select(ds, yhat, CO_dia_media) %>% 
  mutate(Erro = abs((yhat - CO_dia_media)/yhat),
         yhat = round(yhat,4),
         CO_dia_media = round(CO_dia_media, 4), 
         Erro = round(Erro, 4)) %>% 
  rename(Previsao = yhat, Valor_Real = CO_dia_media) %>% 
  dplyr::select(ds, Valor_Real, Previsao, Erro)
tabela %>% DT::datatable( rownames = FALSE, options = list(
  columnDefs = list(list(className = 'dt-center', targets = 0:2))
))
```

![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/tabela_prophet.png)

## 3.19 Cross-Validation Prophet
MAPE de 21%

```{r}
lista_mape <- list()

for (i in 1:10) {
  train_series <- treino %>% 
    rename(ds = Data, y = CO_dia_media) %>% 
    dplyr::select(-CO_dia_mediana, -ano, -dia_da_semana)
  train_series <- train_series[1:(1199+i),]
  
  m <- prophet(train_series, daily.seasonality = TRUE)
  
  #Previsão 7 passos á frente
  future <- make_future_dataframe(m, periods = 7)
  previsao_fit <- stats::predict(m, future)
  
  MAPE_da_serie <- data.frame(
    Valor_observado = dados_filtro$CO_dia_media[(1200+i):(1206+i)],
    Previsao =  previsao_fit$yhat %>% tail(7)
  ) %>% 
    mutate(Erro = abs((Valor_observado - Previsao)/Valor_observado)) %>% 
    summarise(MAPE = mean(Erro))
  
  #Armazena o Mape da série
  lista_mape[[i]] <- MAPE_da_serie$MAPE
  
}
unlist(lista_mape)
mean(unlist(lista_mape))
```

------------------------------

## 4 Modelos de Redes Neurais
```{r}
inds <- treino$Data

ts_treino <- treino$CO_dia_media %>% 
  ts(start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365)

modelo_nnet <- forecast::nnetar(ts_treino, lambda = 0, , decay = 0, maxit = 0)
```

## 4.1 Gráfico de previsão da Rede Neural
```{r, fig.align='center'}
previsao_nnet <- forecast(modelo_nnet, h = 7)

previsao_nnet %>% autoplot() + theme_bw() + 
  labs(x = 'Anos', y = 'Concentração média de CO', 
       title = 'Previsão do modelo de rede neural',
       subtitle = 'Modelo NNAR(24,1,13)[365]') 
```

![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/previsao_redeneural.png)

## 4.2 Previsão do Modelo de Rede Neural e o Erro associado
```{r}
tabela_nnet <- previsao_nnet %>% 
  as.tibble() %>% 
  rename(Previsao = `Point Forecast`) %>% 
  bind_cols(teste[1:7,]) %>%
  mutate(CO = CO_dia_media,
         Erro = abs((CO - Previsao)/Previsao),
         CO = round(CO,4),
         Previsao = round(Previsao,4),
         Erro = round(Erro,4)) %>% 
  dplyr::select(Data, CO, Previsao, Erro) %>% 
  rename(Valor_Real = CO)
tabela_nnet %>% 
  DT::datatable( rownames = FALSE, options = list(
    columnDefs = list(list(className = 'dt-center', targets = 0:3))
  ))
```

![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/tabela_redeneural.png)

## 4.3 Cross-Validation Rede Neural
Mape 28%

```{r}
lista_mape <- list()

for (i in 1:10) {
  
  train_series <- ts(dados_filtro$CO_dia_media[1:(1199+i)] ,start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365)
  #Ajuste do modelo
  fit <- forecast::nnetar(train_series, lambda = 0, , decay = 0, maxit = 0)
  #Previsão 7 passos à frente
  previsao_fit <- forecast(fit, h = 7)
  
  test_series <-  dados_filtro$CO_dia_media[(1200+i):(1206+i)]
  
  MAPE_da_serie <- previsao_fit %>% 
    data.frame() %>% 
    rename(Previsao = Point.Forecast) %>% 
    mutate(Valor_observado = test_series,
           Erro = abs((Valor_observado - Previsao)/Valor_observado)) %>% 
    summarise(MAPE = mean(Erro))
  
  #Armazena o Mape da série
  lista_mape[[i]] <- MAPE_da_serie$MAPE
  
}
unlist(lista_mape)
mean(unlist(lista_mape))
```

------------------------------

## 5 Modelo ARIMA
#### Etapas da modelagem

5.2: Verificação de diferenças significativas entre os meses através do monthplot.
5.3: O gráfico de autocorrelação dá indícios se há estacionariedade (quando a autocorrelação é constante ao longo do tempo) ou não.
Quando há o descrescimento rápido, há indício de estacionariedade.
No entanto, nem sempre é possível verificar através do gráfico.

Para isso, é feito um teste estatístico de raiz unitária (5.4).
(H0: Série é estacionária)
(H1: série não é estacionária)
Após feito o teste, percebeu-se evidências de que a série é estacionária.
Portanto, não será necessária a diferenciação. Com isso, inicia-se a etapa de modelagem (5.5).

5.5: Dois modelos são testados (nesta etapa nós podemos analisar a FAC (gráfico de autocorrelação (5.3)),
ou apenas sugerir modelos, verificando se os parâmetros são significativos.

Foram testados:
1. ARIMA(1,0,1)
2. ARIMA(2,0,1)

Nesse mesmo 5.5 verifica-se a rejeição de um parâmetro do modelo ARIMA(1,0,1).
Com isso, descartamos esse modelo e seguimos para o ARIMA(2,0,1).
O ARIMA (2,0,1) já apresenta todos os seus parâmetros significativos.

5.6: Seguimos e fazemos o teste para verificar se os resíduos são homocedásticos (variância constante) e seguem distribuição normal.
O gráfico mostra que a função de autocorrelação está dentro do intervalo, mostrando homocedasticidade.
O último mostra gráfico analisa se os resíduos estão acima do P-valor de 0.05, o que mostra a não rejeição da normalidade dos resíduos.

5.7: O teste para verificar se a variância é constante foi rejeitado, o que acaba sendo um problema.
No entanto, optamos por continuar e fazer a previsão de 7 passos à frente.

5.8: É feita a previsão do modelo 7 passos à frente.

5.9 É feita a validação cruzada (cross-validation).

```{r}
inds <- treino$Data

ts_treino <- treino$CO_dia_media %>% 
  ts(start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365)
```

## 5.1 Visualização da série
Série original e série com transformação logarítmica
```{r, fig.align='center'}
p1 <- ts_treino %>% 
  autoplot() + labs(x = 'Anos', y = 'Concentração média de CO', 
                    title = 'Concentração média de CO na Estação Irajá') + theme_bw()

p2 <- log(ts_treino) %>% 
  autoplot() + labs(x = 'Anos', y = 'Concentração média de CO', 
                    title = 'Log da Concentração média de CO na Estação Irajá') + theme_bw()

gridExtra::grid.arrange(p1, p2)
```

![](https://github.com/guilhermeagsouza/Projetos/blob/master/concentracao_co_estacao_log.png)

## 5.2 Análise de sazonalidade nos dias
Há uma concentração média maior entre os meses de maio e agosto.

```{r, fig.align='center'}
#ts_treino <- log(ts_treino)
monthplot(ts_treino, xlab = 'Dias', ylab = 'Concentração média de CO na estação Irajá',
          main = 'Monthplot da série temporal - Dias decorridos')
```

![](https://github.com/guilhermeagsouza/Projetos/blob/master/monthplot.png)

## 5.3 Função de autocorrelação da série
Não há como cravar se a série é estacionária ou não, somente visualizando a série temporal nesse caso.
Isso será confirmado com testes de raiz unitária.

PS: Se a série temporal mostrasse um rápido decrescimento na função de autocorrelação, poderíamos ter evidência de estacionariedade.
```{r, fig.align='center'}
forecast::Acf(ts_treino, lag.max = 30, main = 'Gráfico da função de autocorrelação da série')
forecast::Pacf(ts_treino, lag.max = 30, main = 'Gráfico da função de autocorrelação parcial da série')
```

![](https://github.com/guilhermeagsouza/Projetos/blob/master/autocorrelacao_serie.png)
![](https://github.com/guilhermeagsouza/Projetos/blob/master/funcao_autocorrelacao_parcial_serie.png)

## 5.4 Teste de raiz unitária na parte não sazonal
Ho: A série temporal possui uma raiz unitária <-> a série é estacionária
H1: A série temporal não possui raiz unitária <-> a série não é estacionária

A estatística de teste (tau2 = -5.25) é inferior ao valor crítico associado ao nível de confiança de 95% (-2.86).
Dessa forma, conclui-se que a série temporal é estacionária, ou seja, não há rejeição da hipótese nula.

**Não necessidade de diferenciar a parte sazonal**
  
  ```{r}
adf.drift <- ur.df(y = ts_treino, type = c('drift'), lags = 24, selectlags = "AIC")
```

```{r}
adf.drift@teststat
adf.drift@cval
```

## 5.5 Modelando a série temporal
Foram testados os modelos ARIMA(1,0,1) e ARIMA(2,0,1), pois ambos os modelos tiveram parâmetros significativos.
```{r}
fit.air <- Arima(ts_treino, order = c(1,0,1), seasonal = c(0,0,0))
fit.air2 <- Arima(ts_treino, order = c(2,0,1), seasonal = c(0,0,0))

BETS::t_test(fit.air) %>% DT::datatable()
BETS::t_test(fit.air2) %>% DT::datatable()
```

![](https://github.com/guilhermeagsouza/Projetos/blob/master/estimacao_arima(1%2C0%2C1).png)
![](https://github.com/guilhermeagsouza/Projetos/blob/master/estimacao_arima(2%2C0%2C1).png)

## 5.6 Diagnóstico

## 5.61 Diagnóstico do modelo SARIMA (1,0,1)(0,0,0)
Analisando os resíduos padronizados, a função de autocorrelação e os p-valores da estatística de Ljung-Box.
Já no gráfico dos resíduos podemos identificar que há rejeição da hipótese de normalidade.
```{r, fig.align='center'}
diag1 <- tsdiag(fit.air, gof.lag = 20)
```

![](https://github.com/guilhermeagsouza/Projetos/blob/master/diagnostico_modelo1.png)

## 5.62 Diagnóstico do modelo SARIMA (2,0,1)(0,0,0)
No gráfico dos resíduos nota-se que há evidências da não rejeição da hipótese da normalidade.
Portanto, este modelo foi escolhido para seguir com a modelagem.

```{r, fig.align='center'}
diag <- tsdiag(fit.air2, gof.lag = 20)
Box.test(x = fit.air2$residuals, lag = 24, type = 'Ljung-Box', fitdf = 3)
forecast::accuracy(fit.air2) #MAPE = 23.23
```
![](https://github.com/guilhermeagsouza/Projetos/blob/master/diagnostico_modelo2.png)

## 5.7 Teste para Heterocedasticidade condicional autorregressiva
H0: A variãncia da série é estacionária

H1: A variância da série não é estacionária

Pelo teste do Multiplicador de Lagrange, nota-se que a variância não é estacionária.
PS: o teste foi realizado com a série natural e com transformação logarítmica.

```{r}
FinTS::ArchTest(fit.air2$residuals, lags = 12)
```

## 5.8 Previsão
```{r, fig.align='center'}
previsao_sarima <- forecast(object = fit.air2, h = 7, level = 0.95)

previsao_sarima %>% autoplot() + theme_bw() +
  labs(x = 'Anos', y = 'Concentração de CO', 
       title = 'Previsão 7 passos à frente - Concentração média de CO',
       subtitle = 'Modelo SARIMA (2,0,1) com média zero')
```

![](https://github.com/guilhermeagsouza/Projetos/blob/master/previsao_sarima.png)

## 5.9 Tabela de Previsão 7 passos à frente
```{r}
tabela_sarima <- previsao_sarima %>% 
  as.tibble() %>% 
  rename(Previsao = `Point Forecast`) %>% 
  bind_cols(teste[1:7,]) %>% 
  mutate(Previsao = Previsao,
         CO = CO_dia_media,
         Erro = abs((CO- Previsao)/CO)) %>% 
  dplyr::select(Data, Previsao, CO_dia_media, Erro)

tabela_sarima %>%
  rename(CO = CO_dia_media) %>% 
  mutate(Previsao = round(Previsao,4),
         CO = round(CO, 4),
         Erro = round(Erro,4)) %>% 
  rename(Valor_Real = CO) %>% 
  dplyr::select(Data, Valor_Real, Previsao, Erro) %>% 
  DT::datatable( rownames = FALSE, options = list(
    columnDefs = list(list(className = 'dt-center', targets = 0:3))
  ))

```

![](https://github.com/guilhermeagsouza/Projetos/blob/master/tabela_sarima.png)

## 5.91 - Cross-Validation SARIMA
MAPE 24%

```{r}
lista_mape <- list()

for (i in 1:10) {
  
  train_series <- ts(dados_filtro$CO_dia_media[1:(1199+i)] ,start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365)
  #Ajuste do modelo
  fit <- Arima(train_series, order = c(2,0,1), seasonal = c(0,0,0))
  #Previsão 7 passos à frente
  previsao_fit <- forecast(object = fit, h = 7, level = 0.95)
  
  test_series <-  dados_filtro$CO_dia_media[(1200+i):(1206+i)]
  
  MAPE_da_serie <- previsao_fit %>% 
    data.frame() %>% 
    rename(Previsao = Point.Forecast) %>% 
    mutate(Valor_observado = test_series,
           Erro = abs((Valor_observado - Previsao)/Valor_observado)) %>% 
    summarise(MAPE = mean(Erro))
  
  #Armazena o Mape da série
  lista_mape[[i]] <- MAPE_da_serie$MAPE
  
}
unlist(lista_mape)
mean(unlist(lista_mape))

```

------------------------------

## 6 Conclusão dos modelos
Dois modelos univariados de séries temporais foram utilizados: o modelo de suavização exponencial sazonal, proposto por Holt & Winters (1960) e o modelo SARIMA, proposto por Box & Jenkins (1970). A abordagem de Box & Jenkins utiliza os valores passados e presentes da série, permitindo fazer previsões de valores futuros. Os modelos autorregressivos integrados de médias móveis recebem o nome de ARIMA. Quando há adição de sazonalidade, o modelo chama-se SARIMA. Tanto o modelo de suavização exponencial, quanto o modelo SARIMA consideram os padrões de tendência, sazonalidade e ciclo.

Os dois modelos posteriores são Redes Neurais e o algoritmo Prophet, desenvolvido pelo Facebook. (a adicionar)

Para análise da performance dos modelos, foi utilizado a métrica MAPE, que representa o erro médio ponderado. O modelo Holt-Winters apresentou o menor MAPE com 19% de acurácia e o com maior erro agregado foi o modelo SARIMA com 31%.

------------------------------

## 7 Bibliografia

<p>Cochrane, C.: Time Series Nested Cross-Validation. Towards Data Science.
https://towardsdatascience.com/time-series-nested-cross-validation-76adba623eb9 (2018).
Accessed 10 May 2019.</p>
<p>Dancho, M., Vaughan, D.: anomalize: Tidy Anomaly Detection. R package version 0.1.1.
https://CRAN.R-project.org/package=anomalize (2018). Accessed 30 June 2019.</p>
<p>Data.Rio: Dados horários do monitoramento da qualidade do ar - MonitorAr. .
http://www.data.rio/datasets/5b1bf5c3e5114564bbf9b7a372b85e17_2 (2019). Accessed 12
January 2019.</p>
<p>Ferreira, P. C., Speranza, T., Costa, J., Teixeira, F., Marcolino, D.: BETS: Brazilian Economic
Time Series. R package version 0.4.9. https://CRAN.R-project.org/package=BETS (2018).</p>
<p>Hyndman, R., Athanasopoulos, G., Bergmeir, C., Caceres, G., Chhay, L., O'Hara-Wild, M.,
Petropoulos, F., Razbash, S., Wang, E., Yasmeen, F.: forecast: Forecasting functions for time series and linear models. R package version 8.7. http://pkg.robjhyndman.com/forecast (2019). Accessed 30 June 2019.</p>
<p>Hyndman, R.J., & Athanasopoulos, G: Forecasting principles and Practice. OTexts.
https://otexts.com/fpp2/stl.html (2018). Accessed 18 March 2019.</p>
<p>R Core Team: R: A language and environment for statistical computing. R Foundation.
https://www.R-project.org/ (2018). Accessed 30 June 2019.</p>
<p>Taylor, S., Letham, B.: prophet: Automatic Forecasting Procedure. R package version 0.5.
https://CRAN.R-project.org/package=prophet (2019). Accessed 30 June 2019.</p>
<p>Zhang. G.P., Qi, M.: Neural network forecasting for seasonal and trend time series. Eur. J. Oper.
160, 501-514 (2005)</p>
