# 0: Descrição do projeto
Comparar munícipios para estabelecer metas de redução de óbito e acidentes em estradas, para o ano de 2017.
A técnica de clusterização foi utilizada para separar grupos com variáveis mais relevantes umas com as outras.


# Variáveis do dataset
* cidade: nome do município.
* ano: ano referência dos dados.
* pib: PIB do município.
* mat1517: percentual da população entre 15 e 17 anos, matriculada no ensino médio.
* veiculos: quantidade de veículos registrados no município.
* motos: quantidade de motos registradas no município.
* populacao: população do município.
* pop1519: população entre 15 e 19 anos.
* pop2024: população entre 20 e 24 anos.
* pop2529: população entre 25 e 29 anos.
* pop60p: percentual da população acima de 60 anos.
* ibge: código IBGE do município.
* jovem: pop1519 + pop2024 + pop2529
* pjovem: jovem/populacao
* pmotos: 100*motos/veiculos.
* pmat: 100*mat1517/populacao.
* rodovia: extensão de rodovias, em km, do município.

### Carregando os pacotes
```{r, message=FALSE, warning=FALSE}
require(abjutils)
require(brazilmaps)
require(corrgram)
require(DataExplorer)
require(dplyr)
require(DT)
require(ggplot2)
require(magrittr)
require(maps)
require(mice)
require(plotly)
require(randomForest)
require(readr)
require(readxl)
require(stringr)
require(viridis)
```

### Leitura dos dados
```{r}
df_dataset <- read_xlsx("city_dataset.xlsx")
df_municipios <- read_xls("MunicipiosBrasil.xls")
```

### Tabela com as variáveis do dataset
```{r}
df_table <- df_dataset %>% dplyr::select(-ibge)

sketch = htmltools::withTags(table(
 tableHeader(c('Município', 'Ano','PIB', 'Mat1517', 'Veículos', 'Motos',
               'População', 'Pop1519', 'Pop2024', 'Pop2529', 'Pop60p',
               'jovem', 'pjovem', 'pmotos', 'pmat', 'rodovia'))
))

DT::datatable(container = sketch, df_table, rownames = FALSE,
                        options = list(searching = TRUE, pageLength = 10,
                        lengthMenu = c(20,50,100), dom = 'Bfrtip',
                         
                          initComplete = JS("
function(settings, json) {
$(this.api().table().header()).css({
'background-color': '#000',
'color': '#fff'
});
}")))
```

![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/tabela_municipios.png)

* Cabe mencionar que as variáveis com proporção maiores que 1 foram excluídas da análise.

### Leitura de base externa
Dados do IBGE foram utilizados para obtenção das variáveis latitude e longitude para a elaboração de um mapa exploratório simples.

```{r, warning = message}
df_dataset %<>% rename(municipio = cidade)
df_municipios %<>% rename(municipio = MUNICIPIO)

#Colocando os dados em formato minúsculo
df_municipios$municipio %<>% str_to_title()
#Removendo acentos
df_dataset$municipio %<>% 
    str_to_lower() %>% #transformando em letras minúsculas para facilitar a remoção dos acentos
    str_replace_all("á", "a") %>% 
    str_replace_all("é", "e") %>% 
    str_replace_all("í", "i") %>% 
    str_replace_all("ó", "o") %>% 
    str_replace_all("ú", "u") %>% 
    str_replace_all("ã", "a") %>% 
    str_replace_all("â", "a") %>% 
    str_replace_all("ê", "e") %>% 
    str_replace_all("ô", "o") %>% 
    str_replace_all("õ", "o") %>% 
    str_replace_all("ç", "c") %>% 
    str_to_title() #voltando com as variáveis iniciando com letra maiúscula

df_map <- df_dataset %>% 
  dplyr::select(municipio, populacao, ano) %>% 
  dplyr::distinct() %>% 
  dplyr::left_join(df_municipios, by = c("municipio"))

df_map %<>% rename(long = LONGITUDE, lat = LATITUDE, pop = populacao)
df_map %<>% dplyr::filter(ano == 2014)
```


# Criação de um pequeno mapa com as coordenadas de Latitude e Longitude
```{r, warning = FALSE}
p2 <-  df_map %>%
  dplyr::filter(municipio != "Sao Paulo") %>% 
  arrange(pop) %>%
  mutate( name=factor(municipio, unique(municipio))) %>%
  mutate( mytext=paste("Município: ", municipio, "\n", "População: ", pop, sep="")) %>% 
  ggplot() +
    geom_polygon(aes(x=long, y = lat), fill="grey", alpha=0.3) +
    geom_point(aes(x=long, y=lat, size=pop, color = pop, text=mytext, alpha=pop)) +
    scale_size_continuous(range=c(1,15)) +
    scale_color_viridis(option="inferno", trans="log") +
    scale_alpha_continuous(trans="log") +
    theme_void() +
    coord_map() +
    labs(colour = "População")

p2 <- ggplotly(p2, tooltip = "text", width = 600, height = 800)
htmlwidgets::saveWidget(p2, "index.html")
```

# Imputação por Random Forest
Na técnica de imputação por Random Forest são combinados resultados de diversos classificadores, sendo o resultado um emsemble de várias árvores de decisão. Cada classificador funciona como um preditor.

* Foram utilizadas 5 imputações simultâneas e 50 iterações.

PS: Para que o documento não ficasse com mais de 1500 linhas de imputação, o output não foi mostrado.

```{r, cache = TRUE, results=FALSE}
set.seed(2709) #configurando uma semente
df_dataset %<>%  filter(pmat <= 1)
imp <- mice(df_dataset, m = 5, maxit = 50, method = "rf", seed = 2709)

df_dados_imputados <- mice::complete(imp, 1)
df_dados_imputados %<>% dplyr::filter(ano == 2016)
#Salvando a base para que não seja rodada novamente pelo alto custo computacional
df_dados_imputados %>% write.csv2("dados_imputados.csv", row.names = FALSE)
df_dados_imputados <- read.csv2("dados_imputados.csv")
df_dados_imputados %<>%  filter(pmat <= 1)
```


# Correlação entre as variáveis
Conforme o esperado, as variáveis de população são perfeitamente correlacionadas entre si.

* A relação entre a extensão de rodovias em km de cada município e a quantidade de motos registradas no município possui correlação fraca, ou seja, conforme uma variável aumenta, a outra também aumenta.

* Além disso, o percentual da população acima de 60 anos e o percentual da população entre 15 e 19 anos, matriculadas no ensino médio possui correlação fraca.

```{r}
df_dados_imputados %>% 
  dplyr::select(-municipio, -ano, -ibge) %>% 
  corrgram(order=TRUE, upper.panel=panel.cor, main="Gráfico de correlação")
```

![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/correlacao.png)

# Técnica de Análise de Componentes Principais (ACP)
A técnica de ACP constiste em transformar um conjunto de variáveis em diversas componentes principais. Cada componente é uma combinação linear das variáveis originais, sendo independentes entre si. A ideia é reduzir o máximo de informações, com uma perda ínfima de informação.

Na análise foram selecionadas 5 componentes que representam 98.7% da variabilidade dos dados, o que pode ser visto no gráfico abaixo.

```{r}
label <- as.factor(df_dados_imputados[[1]])

covtrain <- df_dados_imputados %>% 
            dplyr::select(-municipio, -ano, -ibge) %>% 
            scale() %>% 
            cov()

train_pc <- prcomp(covtrain)
varex <- train_pc$sdev^2/sum(train_pc$sdev^2)
varcum <- cumsum(varex)
result <- data.frame(num=1:length(train_pc$sdev),
                         ex=varex,
                         cum=varcum)

plot(result$num,result$cum,type="b",xlim=c(0,14),
     main="Variância explicada pelas 14 componentes",
     xlab="Número de Componentes",ylab="Variância explicada")
abline(v = 5, lty = 2)
```

![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/variancia_explicada.png)

Considerando o município de São Paulo na análise.
* No gráfico abaixo são plotadas as duas primeiras componentes principais que representam 88% dos dados.

* Podemos perceber um alto grau de agrupamento de municípios como São Bernardo do Campo, Ribeirão Preto, Guarulhos, São José dos Campos e São Paulo destacados na primeira componente principal e municípios como Águas de São Pedro, Nova Canaã Paulista, Santa Salete, Floreal e Pedranópolis na segunda componente principal.

* Pelo gráfico identificamos que a variável São Paulo distorce muito a análise.

```{r}
train_score <-  df_dados_imputados %>%
                dplyr::select(-municipio, -ano, -ibge) %>% 
                as.matrix() %>% 
                scale() %*% train_pc$rotation[,1:2]
            
train <- cbind(label,as.data.frame(train_score))

colors <- rainbow(length(unique(train$label)))
names(colors) <- unique(train$label)
plot(train$PC1,train$PC2,type="n",main="Duas primeiras componentes principais",
     xlab = "Segunda componente principal", ylab = "Primeira componente principal")
text(train$PC1,train$PC2,label=train$label, col=colors[train$label])
```

![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/primeira_componente.png)

```{r, eval = TRUE}
rm(label)
rm(covtrain)
rm(train_pc)
rm(varex)
rm(varcum)
rm(result)
```

# Outlier: São Paulo

* Já no gráfico sem o município de SP, as duas primeiras componentes principais representam aproximadamente 89% da variabilidade dos dados, ganho de 1% em relação ao PCA anterior considerando o município.

Com 6 componentes explicamos aproximadamente 99.5% da variabilidade dos dados.

```{r}
df_save_data <- df_dados_imputados
df_dados_imputados %<>% dplyr::filter(municipio != "Sao Paulo")
label <- as.factor(df_dados_imputados[[1]])

covtrain <- df_dados_imputados %>% 
            dplyr::select(-municipio, -ano, -ibge) %>% 
            scale() %>% 
            cov()

train_pc <- prcomp(covtrain)
varex <- train_pc$sdev^2/sum(train_pc$sdev^2)
varcum <- cumsum(varex)
result <- data.frame(num=1:length(train_pc$sdev),
                         ex=varex,
                         cum=varcum)

plot(result$num,result$cum,type="b",xlim=c(0,14),
     main="Variância explicada pelas 14 componentes",
     xlab="Número de Componentes",ylab="Variância explicada")
abline(v = 6, lty = 2)
```

![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/numero_clusters.png)

No gráfico de componentes abaixo, sem a presença do município de SP, verificamos um melhor espalhamento dos demais municípíos. 

Municípios com maior PIB e maior população como Campinas, Guarulhos, Ribeirão Preto e São Bernardo do Campo tendem a se concentrar mais na primeira componente principal.

Enquanto municípios menores em população e PIB como Pracinha, Reginópolis e Lavínia se concentram na segunda componente principal 


```{r}
train_score <-  df_dados_imputados %>%
                dplyr::select(-municipio, -ano, -ibge) %>% 
                as.matrix() %>% 
                scale() %*% train_pc$rotation[,1:2]
            
train <- cbind(label,as.data.frame(train_score))

colors <- rainbow(length(unique(train$label)))
names(colors) <- unique(train$label)
plot(train$PC1,train$PC2,type="n",main="Duas primeiras componentes principais",
     xlab = "Segunda componente principal", ylab = "Primeira componente principal")
text(train$PC1,train$PC2,label=train$label, col=colors[train$label])
```

![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/duas_componentes.png)

# Modelo de regressão - Variáveis relevantes

* Para uma análise de variáveis relevantes, foi rodado um critério de seleção Stepwise que minimizasse o AIC para estimação do PIB. 

* Após a checagem do modelo, verificou-se que as seguintes variáveis foram significativas: mat1517, veículos, pop60p, pjovem, pmotos, pmat e rodovia. 

* Vale lembrar que as variáveis tiveram transformação logarítmica, a fim de se fazer as conclusões sobre o modelo.

* PS: Foi feita uma análise global contando com todos os municípios, o que pode ser visto na ordem de grandeza das unidades na interpretação dos parâmetros do modelo.

* Pontos interessantes:
À medida que são adicionados 100 veículos, o PIB aumenta em aproximadamente 45 unidades.

```{r}
set.seed(2809)
df_lm <- df_dados_imputados %>% 
  na.omit() %>% 
  dplyr::select(-municipio, -ano, -ibge) %>% 
  mutate(pib = log(pib), mat1517 = log(mat1517), veiculos = log(veiculos), 
         motos = log(motos), populacao = log(populacao), pop1519 = log(pop1519),
         pop2024 = log(pop2024), pop2529 = log(pop2529), jovem = log(jovem))

regressao <- lm(log(pib) ~ ., data = df_lm %>% 
                       dplyr::select(-populacao,-pop1519, -jovem, -pop2529,-pop2024, -motos, -pmotos))
summary(regressao)

Call:
lm(formula = log(pib) ~ ., data = df_lm %>% dplyr::select(-populacao, 
    -pop1519, -jovem, -pop2529, -pop2024, -motos, -pmotos))

Residuals:
      Min        1Q    Median        3Q       Max 
-0.215678 -0.028185 -0.001953  0.028722  0.217919 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.117e+00  8.911e-02  23.751  < 2e-16 ***
mat1517      4.673e-02  1.699e-02   2.750 0.006201 ** 
veiculos     4.467e-02  3.677e-03  12.150  < 2e-16 ***
pop60p      -4.617e-03  1.206e-03  -3.827 0.000148 ***
pjovem      -2.857e-03  1.415e-03  -2.019 0.044069 *  
pmat        -8.617e-02  1.615e-02  -5.337 1.51e-07 ***
rodovia      2.552e-04  7.602e-05   3.357 0.000856 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.04675 on 443 degrees of freedom
Multiple R-squared:  0.8007,	Adjusted R-squared:  0.798 
F-statistic: 296.7 on 6 and 443 DF,  p-value: < 2.2e-16
```



### Multicolinearidade
* A variável jovem que representa a população de 15 a 19, a de 20 e 24 e a 25 e 29 anos, foi retirada do modelo, por apresentar multicolinearidade. O que é confirmada com a análise do VIF logo abaixo.

* O VIF (fator de variância generalizada) avalia se há multicolinearidade entre as variáveis,
isto é, se as variáveis são altamente correlacionadas, o que aumenta a variabilidade (desvio padrão) do modelo.

* Se o VIF < 5, a variável permanece no modelo. 

* Logo as variáveis selecionadas explicam bem o modelo.eu 

```{r}
car::vif(regressao)

mat1517 veiculos   pop60p   pjovem     pmat  rodovia 
1.499978 4.844512 2.441433 2.265945 3.992041 1.416054 
```

# Clusterização - Kmeans clustering
A ideia da técnica é encontrar agrupamentos nos dados. O algoritmo trabalha iterativamente para 'encaixar' cada observação em um cluster baseado nas características que são providenciadas no dataset.

A distância entre os agrupamentos é a distância entre os centróides que são os valores médios das observações. Toda vez que um agrupamento é formado, um novo método é calculado.

Cada ponto é assinalado ao seu centróide mais próximo, baseado no quadrado da distância Euclideana. 

Para que a análise não seja distorcida, as variáveis foram padronizadas, a fim de tirar a medida em grandezas diferentes.

Logo após a padronização, foi rodado o algoritmo K-Means.

### Número de clusters
Abaixo é feita um código para obter a soma dos quadrados dentro dos clusteres. 
Uma forma de se escolher o número ideal de clusters é a partir que a a soma dos começa a estabilizar, o que acontece com 7 ou 8 clusters.

```{r}
set.seed(2709) #configurando a semente
#Voltando com os dados originais, incluindo São Paulo
df_dados_imputados <- df_save_data

# Padronizando os dados
df_scale <- df_dados_imputados %>%
                dplyr::select(-municipio, -ano, -ibge) %>% 
                scale() #Distância Euclideana

wss <- (nrow(df_scale)-1)*sum(apply(df_scale,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(df_scale,
   centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Número de clusters",
  ylab="Soma dos quadrados intra cluster", main = "Escolha do número clusters")
abline(v = 6, col = "red", lty = c(2), lwd = c(3))
```

![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/numero_clusters.png)

```{r}
cluster_km <- kmeans(df_scale, centers = 6)$cluster

```

### Número de municípios/Proporção de municípios por cluster

```{r}
df_dados_imputados %>%
  dplyr::select(municipio, -ano, -ibge) %>%
  cbind(cluster_km) %>%
  rename(Cluster = cluster_km) %>% 
  arrange(Cluster) %>%
  mutate(N = 1) %>% 
  group_by(Cluster) %>% 
  dplyr::summarise(Total_Municipios = sum(N)) %>%
  mutate(Proporcao_Municipios = round(100*Total_Municipios/sum(Total_Municipios),2)) %>% 
  DT::datatable(rownames = F)
```

![](https://github.com/guilhermeagsouza/ImagensTabelasDosProjetos/blob/master/numero_clusters.png)

### Tabela com a média de cada variável para cada cluster

```{r}
df_dados_imputados %>%
  dplyr::select(-municipio, -ano, -ibge) %>%
  cbind(cluster_km) %>% 
  rename(Cluster = cluster_km) %>% 
  group_by(Cluster) %>% 
  dplyr::summarise_all(mean) %>% 
  DT::datatable(rownames = F)
```


### Análise dos clusters

* Cluster 1: São Paulo.

O cluster 1 tem apenas São Paulo como município. Por apresentar as maiores médias do PIB e do número de veículos, maior número de rodovias em KM, a menor proporção de matriculados no Ensino Médio na faixa de 15 a 19 anos e a menor proporção da população de 60 anos ou mais e a maior proporção da população na faixa de idade entre 25 a 29 anos, aparece sozinho no agrupamento.

* Cluster 2: Boituva, Cabreúva, Cubatão, Hortolândia, Taboão da Serra.

Tem um percentual baixo da população abaixo de 60 anos e um percentual alto da população jovem.

* Cluster 3: Catanduva, Mirassol, Mogi Mirim, Ourinhos, Ubatuba.

Tem um PIB médio pequeno e um percentual de população entre 15 e 17 anos alto.

* Cluster 4: Balbinos, Lavínia, Reginópolis

Tem um número baixo tanto de veículos como motos, uma população pequena comparada aos demais municípios e a que tem a maior proporção jovens na população.

* Cluster 5: Araraquara, Guarulhos, Barretos, Itapeva, Franca, Jundiaí.

O segundo cluster que tem o maior número de jovens, perdendo apenas para o cluster 1 (município de São Paulo); a menor proporção de jovens matriculados no ensino médio, só perdendo para São Paulo; 

* Cluster 6: Cardoso, Dourado, Divinolândia, Nova Granada, Vista Alegre do Alto.

São municípios pequenos em população, PIB médio baixo, grande parte da população na faixa dos 60 anos de idade ou mais.

