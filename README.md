# Projetos
Repositório para expor os meus projetos de análise e visualização de dados, machine learning e deep learning

### Descrição dos projetos

------------------------------
**<p>Projeto: Previsão de 3 meses de vendas de diferentes lojas (competição)</p>**
Descrição: Competição no Kaggle para prever 3 meses de vendas de itens em diferentes lojas. Foram utilizados os modelos Neural Network Autoregression, LightGBM e o modelo univariado de suavização exponencial.

O modelo com a utilização do LightGBM alcançou a 10a colocação na competição, dentre 460 participantes.
![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/kaggle_score.PNG)

Neste [link](https://www.kaggle.com/guilhermeagsouza/neural-network-autoregression-15-73290) você consegue ver a implementação do modelo de redes neurais com regressores (código).

------------------------------
 **<p>Projeto: Análise da concentração de monóxido de carbono nas estações de tratamento do estado do Rio de Janeiro</p>**
 Descrição: Neste projeto analisamos dados diários da concentração de monóxido de carbono de estações de monitoramento da qualidade do ar, obtidos através do site [Data.Rio](http://www.data.rio). Foi realizada uma análise exploratória (EDA) e detecção de anomalias, além da previsão de quatro modelos distintos, sendo eles o primeiro o de suavização exponencial Holt-Winters, o segundo Prophet (desenvolvido pelo Facebook), o terceiro Redes Neurais (Hyndman) e o modelo ARIMA. Utilizou-se validação cruzada (cross-validation) para cálculo dos *mapes* (mean absolute percentage error), métrica para avaliação da acurácia dos modelos. 
 
 O código está disponível neste [link](https://github.com/guilhermeagsouza/Projetos/blob/master/2_Project.md) (código + gráficos).
 
 ------------------------------
 **<p>Estudo para redução do número de óbitos em estradas</p>**
Descrição: Neste projeto estudo indicadores econômicos, de educação, população e infraestrutura que mais impactam em medidas que possam auxiliar municípios de São Paulo em metas de redução de óbito e acidentes em estradas, para o ano de 2017. Foram utilizadas técnicas de imputação de dados com Random Forest, modelo de regressão para avaliar quais variáveis eram significativas e por final a técnica de clusterização para identificar quais variáveis são mais associadas a um grupo de municípios. 

 O código está disponível neste [link](https://github.com/guilhermeagsouza/Projetos/blob/master/3_Project.md) (código + gráficos).
 
 ------------------------------
 **<p>Projeto: Análise dos vendedores de uma empresa do segmento varejo</p>**
 Descrição: Análise do perfil dos vendedores da varejista Ricardo Eletro. Foram análisadas as variáveis de recência, frequência de vendas e valor monetário das vendas. Foram propostas duas metodologias: uma de clusterização k-means e outra considerando os quartis de um boxplot, recebendo cada critério 0,2,3,4 pontos. Também foi verificado quais os vendedores migravam semanalmente entre 3 perfis diferentes (high, medium, low). 
 
 Os códigos estão invisíveis, porém os resultados podem ser vistos neste [link](https://github.com/guilhermeagsouza/Projetos/blob/master/An%C3%A1lise%20Vendedores%20da%20Ricardo%20Eletro.pdf) (relatório).

------------------------------
 **<p>Projeto: Previsão da Audiência das marcas do Grupo Abril</p>**
 Descrição: Prever a audiência de cada marca (Veja, Superinteressante, Guia do Estudante, Quatro Rodas, Capricho, Boaforma, Saúde e Cláudia) do Grupo Abril através da métrica de pageviews. Foram realizadas análises iniciais para monitoramento de possíveis quebras estruturais nas séries. Posteriormente quatro modelos foram propostos: ETS (modelo de suavização exponencial), Prophet (Facebook), SARIMA (com variáveis regressoras) e um Ensemble ponderado dos modelos. A consolidação dos resultados alimentava um banco de dados no Big Query (Google). A partir desses dados um dashboard apresentava os erros percentuais dos modelos.
 
 [Códigos](https://github.com/guilhermeagsouza/previsao-abril)
 
 [Dashboard com dados fictícios](https://guilhermeagsouza.shinyapps.io/dashboard_audiencia/)


------------------------------
**<p>Projeto: Utilizando XGBoost para prever se a entrada em um trade foi acertiva ou não (Notebook mais bem avaliado da competição desenvolvido em R) </p>**
Descrição: Baseado em diversas operações (trades) em diversas classes de ativos, a empresa Jane Street patrocinou essa competição no Kaggle para ter um modelo com a maior acurácia e que avaliasse quando os trades realizados pelos profissionais do mercado foram no momento correto ou não. A ideia aqui foi desenvolver um modelo XGBoost inicialmente para ver a aderência aos dados.

Códigos: [Kaggle](https://www.kaggle.com/code/guilhermeagsouza/eda-tidymodels-approach-with-xgboost)

------------------------------

**<p>Projeto: Dashboard Análise Cripto (em andamento)</p>** 
Descrição: A ideia do dashboard é trazer além da cotação histórica e rentabilidade de uma seleção de criptomoedas, outros indicadores correlacionados ou não como NASDAQ, SP500, Ouro e a relação entre Euro/Dólar.

[Dashboard do projeto](https://guilhermeagsouza.shinyapps.io/analise_cripto)

------------------------------

[![Linkedin Badge](https://img.shields.io/badge/-Guilherme-blue?style=flat-square&logo=Linkedin&logoColor=white&link=https://www.linkedin.com/in/guilhermeagsouza/)](https://www.linkedin.com/in/guilhermeagsouza/) 
