## Doença de Alzheimer
  
  ![](https://hypescience.com/wp-content/uploads/2015/04/proteina-tau-alzheimer-838x629.jpg)
  
  *Fonte: Hypescience*
  
  ### Objetivo
  O objetivo desta análise é verificar quais variáveis são significativas na identificação de um paciente com a doença de Alzheimer. A variável resposta é do tipo binária, 1 para cérebro debilitado ou 0 caso contrário. Dentre as mais de 130 variáveis preditoras estão diversos tipos de proteínas, sendo as mais significantes de acordo com a modelagem as proteínas tau e amiloide.

![](http://www.cslimites.com/wp-content/uploads/2015/05/Alzheimer-Brain-Scans.jpg)

* Carregando os pacotes
```{r warning = FALSE}
suppressMessages(library(caret))
suppressMessages(library(AppliedPredictiveModeling))
suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))
suppressMessages(library(ggcorrplot))
suppressMessages(library(factoextra))
```

* Separando os dados em dados de treinamento e teste.
```{r}
set.seed(18022018) ##Configurando uma semente para guardar o resultado
data(AlzheimerDisease)
#preditores <- predictors[,57:68]
adData = data.frame(diagnosis,predictors)
#adData = data.frame(diagnosis,preditores)
```
* Selecionando 75% da amostra para os dados de treinamento e 25% da amostra para os dados de teste
```{r}
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]
```

* Quais as variáveis são altamente correlacionadas
* São excluídas as variáveis com correlação igual ou maior que 0.75
```{r}
corr <- adData %>% select_if(is.numeric) %>% cor()
rem_corr <- caret::findCorrelation(x = corr, cutoff = 0.75)
t_train <- training[,-c(rem_corr)]
t_test <- testing[,-c(rem_corr)]
```
### Análise de componentes principais
* Para redução de dimensionalidade, utiliza-se Análise de Componentes principais
* Para captar 90% da variabilidade dos dados, são necessárias 54 variáveis.
```{r}
PCA <- preProcess(adData, method = c("center","scale", "pca"), thresh = 0.9)
Created from 333 samples and 131 variables

Pre-processing:
  - centered (129)
- ignored (2)
- principal component signal extraction (129)
- scaled (129)

PCA needed 54 components to capture 90 percent of the variance
```

### Validação cruzada (Cross-validation)
```{r}
#Validação cruzada
trControl1 = caret::trainControl(method = "cv", number = 20, savePredictions = TRUE)
$method
[1] "cv"

$number
[1] 20

$repeats
[1] NA

$search
[1] "grid"

$p
[1] 0.75
```

## Machine Learning
### Cart Model
```{r}
grid <- expand.grid(cp = seq(0, 1, by = 0.01))
arvore_modelo <- caret::train(diagnosis ~., data = t_train, method = "rpart",
                              preProcess = c("center","scale", "pca"), 
                              trControl = trControl,
                              tuneGrid = grid)
arvore_modelo
CART 

251 samples
105 predictors
2 classes: 'Impaired', 'Control' 

Pre-processing: centered (109), scaled (109), principal
component signal extraction (109) 
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 227, 225, 226, 226, 226, 226, ... 
Resampling results across tuning parameters:
  
  cp    Accuracy   Kappa     
0.00  0.6852436  0.23780561
0.01  0.6852436  0.23173142
0.02  0.7283333  0.28901316
0.03  0.7243333  0.27031877
0.04  0.7243333  0.27031877
0.05  0.7608333  0.33667137
0.06  0.7608333  0.33667137
0.07  0.7608333  0.34894820
0.08  0.7566667  0.34990514
0.09  0.7645128  0.36412286
0.10  0.7645128  0.35774871
0.11  0.7645128  0.35774871
0.12  0.7728462  0.33047599
0.13  0.7728462  0.33047599
0.14  0.7648462  0.29392776
0.15  0.7490000  0.26602302
0.16  0.7250000  0.14918382
0.17  0.7250000  0.14918382
0.18  0.7330000  0.13473515
0.19  0.7330000  0.10127508
0.20  0.7330000  0.10127508
0.21  0.7251538  0.02086957
0.22  0.7251538  0.02086957
0.23  0.7251538  0.02086957
0.24  0.7251538  0.02086957
0.25  0.7251538  0.00000000
0.26  0.7251538  0.00000000
0.27  0.7251538  0.00000000
0.28  0.7251538  0.00000000
0.29  0.7251538  0.00000000
0.30  0.7251538  0.00000000
0.31  0.7251538  0.00000000
0.32  0.7251538  0.00000000
0.33  0.7251538  0.00000000
0.34  0.7251538  0.00000000
0.35  0.7251538  0.00000000
0.36  0.7251538  0.00000000
0.37  0.7251538  0.00000000
0.38  0.7251538  0.00000000
0.39  0.7251538  0.00000000
0.40  0.7251538  0.00000000
0.41  0.7251538  0.00000000
0.42  0.7251538  0.00000000
0.43  0.7251538  0.00000000
0.44  0.7251538  0.00000000
0.45  0.7251538  0.00000000
0.46  0.7251538  0.00000000
0.47  0.7251538  0.00000000
0.48  0.7251538  0.00000000
0.49  0.7251538  0.00000000
0.50  0.7251538  0.00000000
0.51  0.7251538  0.00000000
0.52  0.7251538  0.00000000
0.53  0.7251538  0.00000000
0.54  0.7251538  0.00000000
0.55  0.7251538  0.00000000
0.56  0.7251538  0.00000000
0.57  0.7251538  0.00000000
0.58  0.7251538  0.00000000
0.59  0.7251538  0.00000000
0.60  0.7251538  0.00000000
0.61  0.7251538  0.00000000
0.62  0.7251538  0.00000000
0.63  0.7251538  0.00000000
0.64  0.7251538  0.00000000
0.65  0.7251538  0.00000000
0.66  0.7251538  0.00000000
0.67  0.7251538  0.00000000
0.68  0.7251538  0.00000000
0.69  0.7251538  0.00000000
0.70  0.7251538  0.00000000
0.71  0.7251538  0.00000000
0.72  0.7251538  0.00000000
0.73  0.7251538  0.00000000
0.74  0.7251538  0.00000000
0.75  0.7251538  0.00000000
0.76  0.7251538  0.00000000
0.77  0.7251538  0.00000000
0.78  0.7251538  0.00000000
0.79  0.7251538  0.00000000
0.80  0.7251538  0.00000000
0.81  0.7251538  0.00000000
0.82  0.7251538  0.00000000
0.83  0.7251538  0.00000000
0.84  0.7251538  0.00000000
0.85  0.7251538  0.00000000
0.86  0.7251538  0.00000000
0.87  0.7251538  0.00000000
0.88  0.7251538  0.00000000
0.89  0.7251538  0.00000000
0.90  0.7251538  0.00000000
0.91  0.7251538  0.00000000
0.92  0.7251538  0.00000000
0.93  0.7251538  0.00000000
0.94  0.7251538  0.00000000
0.95  0.7251538  0.00000000
0.96  0.7251538  0.00000000
0.97  0.7251538  0.00000000
0.98  0.7251538  0.00000000
0.99  0.7251538  0.00000000
1.00  0.7251538  0.00000000

Accuracy was used to select the optimal model using the
largest value.
The final value used for the model was cp = 0.13.
```

```{r}
arvore_pred <- predict.train(arvore_modelo, newdata = t_test)
confusionMatrix(t_test$diagnosis, arvore_pred)
Confusion Matrix and Statistics

Reference
Prediction Impaired Control
Impaired        8      14
Control         4      56

Accuracy : 0.7805          
95% CI : (0.6754, 0.8644)
No Information Rate : 0.8537          
P-Value [Acc > NIR] : 0.97385         

Kappa : 0.3469          
Mcnemar's Test P-Value : 0.03389         

Sensitivity : 0.66667         
Specificity : 0.80000         
Pos Pred Value : 0.36364         
Neg Pred Value : 0.93333         
Prevalence : 0.14634         
Detection Rate : 0.09756         
Detection Prevalence : 0.26829         
Balanced Accuracy : 0.73333         

'Positive' Class : Impaired
```

### K-Nearest-Neighbor
```{r}
grid_knn <- expand.grid(k = seq(2, 50, by = 1))
modelo_knn <- caret::train(diagnosis ~., data = t_train, method = "knn", trControl = trControl, metric = "Accuracy",
preProcess = c("center","scale", "pca"), tuneGrid = grid_knn)
modelo_knn
k-Nearest Neighbors 

251 samples
105 predictors
2 classes: 'Impaired', 'Control' 

Pre-processing: centered (109), scaled (109), principal
component signal extraction (109) 
Resampling: Cross-Validated (20 fold) 
Summary of sample sizes: 239, 238, 238, 239, 239, 239, ... 
Resampling results across tuning parameters:

k   Accuracy   Kappa     
2  0.6848443  0.24884500
3  0.7254579  0.27431028
4  0.7091117  0.23931035
5  0.7575092  0.31924898
6  0.7491758  0.28350496
7  0.7934066  0.39398147
8  0.7767399  0.35433550
9  0.7857143  0.36723201
10  0.7891941  0.36083307
11  0.7857143  0.34007822
12  0.7866758  0.36575800
13  0.7866300  0.34231596
14  0.7825092  0.33859990
15  0.7786172  0.31931323
16  0.7776557  0.30565866
17  0.7661172  0.26506452
18  0.7629121  0.24952163
19  0.7622711  0.24732865
20  0.7539377  0.20625722
21  0.7504121  0.19844583
22  0.7462454  0.17880298
23  0.7462454  0.17880298
24  0.7423993  0.16301350
25  0.7497711  0.17340682
26  0.7536630  0.18558527
27  0.7536630  0.18226526
28  0.7575092  0.19805473
29  0.7536630  0.18226526
30  0.7578297  0.18920971
31  0.7578297  0.19278113
32  0.7664835  0.23865774
33  0.7619963  0.18920971
34  0.7539835  0.15199166
35  0.7543040  0.16215398
36  0.7501374  0.14327210
37  0.7267399  0.05237550
38  0.7424451  0.11169315
39  0.7347527  0.08082162
40  0.7466117  0.11077149
41  0.7424451  0.08934292
42  0.7347527  0.07903590
43  0.7427656  0.09498202
44  0.7305861  0.05760733
45  0.7302656  0.04498202
46  0.7299451  0.03007519
47  0.7341117  0.05150376
48  0.7305861  0.03633540
49  0.7382784  0.05864662
50  0.7382784  0.05864662

Accuracy was used to select the optimal model using the
largest value.
The final value used for the model was k = 7
```

```{r}
plot(modelo_knn)
knn_pred <- predict.train(modelo_knn, newdata = t_test)
confusionMatrix(t_test$diagnosis, knn_pred)
Confusion Matrix and Statistics

Reference
Prediction Impaired Control
Impaired       10      12
Control         3      57

Accuracy : 0.8171          
95% CI : (0.7163, 0.8938)
No Information Rate : 0.8415          
P-Value [Acc > NIR] : 0.77962         

Kappa : 0.4648          
Mcnemar's Test P-Value : 0.03887         

Sensitivity : 0.7692          
Specificity : 0.8261          
Pos Pred Value : 0.4545          
Neg Pred Value : 0.9500          
Prevalence : 0.1585          
Detection Rate : 0.1220          
Detection Prevalence : 0.2683          
Balanced Accuracy : 0.7977          

'Positive' Class : Impaired
```

```{r}
modelo_lda <- caret::train(diagnosis ~., data = t_train, method = "lda", trControl = trControl, metric = "Accuracy",
                           preProcess = c("center","scale", "pca"))
modelo_lda
Linear Discriminant Analysis 

251 samples
105 predictors
2 classes: 'Impaired', 'Control' 

Pre-processing: centered (109), scaled (109), principal
component signal extraction (109) 
Resampling: Cross-Validated (20 fold) 
Summary of sample sizes: 239, 239, 239, 238, 238, 238, ... 
Resampling results:
  
  Accuracy   Kappa    
0.8451465  0.5845768
```

```{r}
plot(modelo_lda)
lda_pred <- predict.train(modelo_lda, newdata = t_test)
confusionMatrix(t_test$diagnosis, lda_pred) #79%
Confusion Matrix and Statistics

Reference
Prediction Impaired Control
Impaired       12      10
Control         7      53

Accuracy : 0.7927          
95% CI : (0.6889, 0.8743)
No Information Rate : 0.7683          
P-Value [Acc > NIR] : 0.3549          

Kappa : 0.4481          
Mcnemar's Test P-Value : 0.6276          

Sensitivity : 0.6316          
Specificity : 0.8413          
Pos Pred Value : 0.5455          
Neg Pred Value : 0.8833          
Prevalence : 0.2317          
Detection Rate : 0.1463          
Detection Prevalence : 0.2683          
Balanced Accuracy : 0.7364          

'Positive' Class : Impaired
```

### Gradient Boosting Machine
```{r}
#interaction.depth = número de folhas
#n.trees = número de árvores
#shrinkage = taxa de aprendizado 0.001
#n.minobsinnode = Número mínimo de amostras nos nós terminais da árvore

grid_gbm <- expand.grid(interaction.depth = c(1:3), n.trees = #seq(50,150,by = 50), 
shrinkage = seq(0.001,0.01,0.001), n.minobsinnode = 10)

modelo_gbm <- caret::train(diagnosis ~., data = t_train, method = "gbm", trControl = trControl, metric = "Accuracy",
preProcess = c("center","scale", "pca"))
modelo_gbm
Stochastic Gradient Boosting 

251 samples
105 predictors
2 classes: 'Impaired', 'Control' 

Pre-processing: centered (109), scaled (109), principal
component signal extraction (109) 
Resampling: Cross-Validated (20 fold) 
Summary of sample sizes: 239, 238, 239, 238, 239, 239, ... 
Resampling results across tuning parameters:

interaction.depth  n.trees  Accuracy   Kappa    
1                   50      0.7806777  0.3452106
1                  100      0.7697344  0.3322015
1                  150      0.7655678  0.3224793
2                   50      0.7649267  0.2883870
2                  100      0.7899725  0.3871851
2                  150      0.7655678  0.2962973
3                   50      0.7774267  0.3312722
3                  100      0.7726648  0.3169200
3                  150      0.7733059  0.3173811

Tuning parameter 'shrinkage' was held constant at a value of
0.1
Tuning parameter 'n.minobsinnode' was held constant at
a value of 10
Accuracy was used to select the optimal model using the
largest value.
The final values used for the model were n.trees =
100, interaction.depth = 2, shrinkage = 0.1 and n.minobsinnode
= 10.
```


```{r}
pred_gbm <- predict(modelo_gbm, newdata = t_test)
confusionMatrix(pred_gbm, t_test$diagnosis)
Confusion Matrix and Statistics

Reference
Prediction Impaired Control
Impaired       11       3
Control        11      57

Accuracy : 0.8293          
95% CI : (0.7302, 0.9034)
No Information Rate : 0.7317          
P-Value [Acc > NIR] : 0.02682         

Kappa : 0.5086          
Mcnemar's Test P-Value : 0.06137         

Sensitivity : 0.5000          
Specificity : 0.9500          
Pos Pred Value : 0.7857          
Neg Pred Value : 0.8382          
Prevalence : 0.2683          
Detection Rate : 0.1341          
Detection Prevalence : 0.1707          
Balanced Accuracy : 0.7250          

'Positive' Class : Impaired 
```
### Modelo LogitBoost
```{r}
grid_logit <- expand.grid(nIter = seq(2,51, by = 1))
modelo_logit <- caret::train(diagnosis ~., data = t_train, method = "LogitBoost", trControl = trControl, metric = "Accuracy",
                             preProcess = c("center","scale", "pca"), tuneGrid = grid_logit)
modelo_logit
Boosted Logistic Regression 

251 samples
105 predictors
2 classes: 'Impaired', 'Control' 

Pre-processing: centered (109), scaled (109), principal
component signal extraction (109) 
Resampling: Cross-Validated (20 fold) 
Summary of sample sizes: 238, 239, 238, 238, 238, 239, ... 
Resampling results across tuning parameters:
  
  nIter  Accuracy   Kappa    
2     0.8433279  0.2030592
3     0.7644231  0.2935305
4     0.8500703  0.3431761
5     0.7894231  0.3772218
6     0.8414996  0.3910798
7     0.7846154  0.3901489
8     0.8423864  0.4616305
9     0.7923077  0.4396309
10     0.8276335  0.4312976
11     0.7724359  0.3672550
12     0.8267298  0.4005283
13     0.7849359  0.3979065
14     0.8194949  0.3551853
15     0.7759615  0.4027455
16     0.8018182  0.3861682
17     0.7413462  0.3116209
18     0.7883189  0.3557499
19     0.7532051  0.3599214
20     0.7833081  0.3279420
21     0.7256410  0.2801900
22     0.7693784  0.2489702
23     0.7326923  0.3028753
24     0.7715321  0.3148476
25     0.7137821  0.2589941
26     0.7618143  0.3295874
27     0.7451923  0.3434580
28     0.8026603  0.4387691
29     0.7503205  0.3495232
30     0.7893726  0.3961743
31     0.7701923  0.3829169
32     0.8102564  0.4786065
33     0.7698718  0.3900801
34     0.7942502  0.3994026
35     0.7650641  0.3691558
36     0.7908848  0.3988697
37     0.7618590  0.3778579
38     0.7982964  0.4427180
39     0.7615385  0.3712296
40     0.7901437  0.4052901
41     0.7653846  0.4000707
42     0.8127914  0.4870231
43     0.7932692  0.4621830
44     0.8155905  0.4819642
45     0.7650641  0.3934564
46     0.7895824  0.4215102
47     0.7647436  0.3874805
48     0.7995319  0.4412904
49     0.7685897  0.3815686
50     0.8107692  0.4785691
51     0.7647436  0.3765641

Accuracy was used to select the optimal model using the
largest value.
The final value used for the model was nIter = 4.
```

```{r}
plot(modelo_logit)
pred_logit <- predict(modelo_logit, t_test)
confusionMatrix(pred_logit, t_test$diagnosis)
Confusion Matrix and Statistics

Reference
Prediction Impaired Control
Impaired       10       1
Control         6      52

Accuracy : 0.8986          
95% CI : (0.8021, 0.9582)
No Information Rate : 0.7681          
P-Value [Acc > NIR] : 0.004653        

Kappa : 0.6803          
Mcnemar's Test P-Value : 0.130570        

Sensitivity : 0.6250          
Specificity : 0.9811          
Pos Pred Value : 0.9091          
Neg Pred Value : 0.8966          
Prevalence : 0.2319          
Detection Rate : 0.1449          
Detection Prevalence : 0.1594          
Balanced Accuracy : 0.8031          

'Positive' Class : Impaired
```

### Extreme Gradient Boosting
* No XGB não tunei os parâmetros por exigir elevado recurso computacional.

```{r}
modelo_xgb <- caret::train(diagnosis ~., data = t_train, method = "xgbTree", trControl = trControl, metric = "Accuracy",
preProcess = c("center","scale", "pca"))
modelo_xgb
eXtreme Gradient Boosting 

251 samples
105 predictors
2 classes: 'Impaired', 'Control' 

Pre-processing: centered (109), scaled (109), principal
component signal extraction (109) 
Resampling: Cross-Validated (20 fold) 
Summary of sample sizes: 238, 239, 239, 238, 238, 239, ... 
Resampling results across tuning parameters:

eta  max_depth  colsample_bytree  subsample  nrounds  Accuracy 
0.3  1          0.6               0.50        50      0.7646520
0.3  1          0.6               0.50       100      0.7857601
0.3  1          0.6               0.50       150      0.7652473
0.3  1          0.6               0.75        50      0.7771062
0.3  1          0.6               0.75       100      0.7880495
0.3  1          0.6               0.75       150      0.7765110
0.3  1          0.6               1.00        50      0.7765110
0.3  1          0.6               1.00       100      0.7697344
0.3  1          0.6               1.00       150      0.7816392
0.3  1          0.8               0.50        50      0.7890568
0.3  1          0.8               0.50       100      0.7819139
0.3  1          0.8               0.50       150      0.8047619
0.3  1          0.8               0.75        50      0.7847985
0.3  1          0.8               0.75       100      0.7812729
0.3  1          0.8               0.75       150      0.7928114
0.3  1          0.8               1.00        50      0.7883700
0.3  1          0.8               1.00       100      0.7771520
0.3  1          0.8               1.00       150      0.7777473
0.3  2          0.6               0.50        50      0.8011447
0.3  2          0.6               0.50       100      0.8049908
0.3  2          0.6               0.50       150      0.8162546
0.3  2          0.6               0.75        50      0.7924908
0.3  2          0.6               0.75       100      0.7999084
0.3  2          0.6               0.75       150      0.8043956
0.3  2          0.6               1.00        50      0.8195055
0.3  2          0.6               1.00       100      0.8037546
0.3  2          0.6               1.00       150      0.8117674
0.3  2          0.8               0.50        50      0.7957875
0.3  2          0.8               0.50       100      0.8130037
0.3  2          0.8               0.50       150      0.8130037
0.3  2          0.8               0.75        50      0.7886447
0.3  2          0.8               0.75       100      0.7957418
0.3  2          0.8               0.75       150      0.8002289
0.3  2          0.8               1.00        50      0.7765110
0.3  2          0.8               1.00       100      0.7889194
0.3  2          0.8               1.00       150      0.7960623
0.3  3          0.6               0.50        50      0.7800366
0.3  3          0.6               0.50       100      0.8079212
0.3  3          0.6               0.50       150      0.7963828
0.3  3          0.6               0.75        50      0.8168040
0.3  3          0.6               0.75       100      0.8129579
0.3  3          0.6               0.75       150      0.8046245
0.3  3          0.6               1.00        50      0.7892857
0.3  3          0.6               1.00       100      0.8005495
0.3  3          0.6               1.00       150      0.8008242
0.3  3          0.8               0.50        50      0.7931319
0.3  3          0.8               0.50       100      0.8085623
0.3  3          0.8               0.50       150      0.8082418
0.3  3          0.8               0.75        50      0.8120879
0.3  3          0.8               0.75       100      0.8274725
0.3  3          0.8               0.75       150      0.8194597
0.3  3          0.8               1.00        50      0.8088370
0.3  3          0.8               1.00       100      0.8120879
0.3  3          0.8               1.00       150      0.8120879
0.4  1          0.6               0.50        50      0.7874542
0.4  1          0.6               0.50       100      0.7967033
0.4  1          0.6               0.50       150      0.7928571
0.4  1          0.6               0.75        50      0.7688645
0.4  1          0.6               0.75       100      0.7729853
0.4  1          0.6               0.75       150      0.7691392
0.4  1          0.6               1.00        50      0.7800824
0.4  1          0.6               1.00       100      0.7768315
0.4  1          0.6               1.00       150      0.7765568
0.4  1          0.8               0.50        50      0.7637363
0.4  1          0.8               0.50       100      0.7723443
0.4  1          0.8               0.50       150      0.7595696
0.4  1          0.8               0.75        50      0.7924908
0.4  1          0.8               0.75       100      0.7931777
0.4  1          0.8               0.75       150      0.7765110
0.4  1          0.8               1.00        50      0.7726190
0.4  1          0.8               1.00       100      0.7774725
0.4  1          0.8               1.00       150      0.7572802
0.4  2          0.6               0.50        50      0.7803114
0.4  2          0.6               0.50       100      0.7854396
0.4  2          0.6               0.50       150      0.7735348
0.4  2          0.6               0.75        50      0.7922619
0.4  2          0.6               0.75       100      0.7851648
0.4  2          0.6               0.75       150      0.7848443
0.4  2          0.6               1.00        50      0.8133242
0.4  2          0.6               1.00       100      0.8002289
0.4  2          0.6               1.00       150      0.7960623
0.4  2          0.8               0.50        50      0.7921703
0.4  2          0.8               0.50       100      0.7853938
0.4  2          0.8               0.50       150      0.7809524
0.4  2          0.8               0.75        50      0.7759158
0.4  2          0.8               0.75       100      0.7803571
0.4  2          0.8               0.75       150      0.7806777
0.4  2          0.8               1.00        50      0.7764652
0.4  2          0.8               1.00       100      0.7886905
0.4  2          0.8               1.00       150      0.7925366
0.4  3          0.6               0.50        50      0.7774267
0.4  3          0.6               0.50       100      0.7886905
0.4  3          0.6               0.50       150      0.7963828
0.4  3          0.6               0.75        50      0.7889652
0.4  3          0.6               0.75       100      0.8011447
0.4  3          0.6               0.75       150      0.7963370
0.4  3          0.6               1.00        50      0.7957418
0.4  3          0.6               1.00       100      0.7963828
0.4  3          0.6               1.00       150      0.8040751
0.4  3          0.8               0.50        50      0.7765568
0.4  3          0.8               0.50       100      0.7733059
0.4  3          0.8               0.50       150      0.7658883
0.4  3          0.8               0.75        50      0.7919414
0.4  3          0.8               0.75       100      0.7922619
0.4  3          0.8               0.75       150      0.7884158
0.4  3          0.8               1.00        50      0.8053571
0.4  3          0.8               1.00       100      0.8213370
0.4  3          0.8               1.00       150      0.8133242
Kappa    
0.3443478
0.3850412
0.3430075
0.3199398
0.3846673
0.3696209
0.3252863
0.3434776
0.3796663
0.3992108
0.3801448
0.4255461
0.3576470
0.3693931
0.4051160
0.3548520
0.3665588
0.3681932
0.4326557
0.4381707
0.4629211
0.3591274
0.3878981
0.3967545
0.4549454
0.4011611
0.4246248
0.4023670
0.4531440
0.4531440
0.3703724
0.3959939
0.4254330
0.3430130
0.3586018
0.3880136
0.3474459
0.4489750
0.4483974
0.4215045
0.4086735
0.3892290
0.3777427
0.4104284
0.4028049
0.3879849
0.4334829
0.4345035
0.4414200
0.4644913
0.4437440
0.4413489
0.4541454
0.4592515
0.3830128
0.4312307
0.4301394
0.3348832
0.3472322
0.3483377
0.3546742
0.3577565
0.3724509
0.3302008
0.3609736
0.3326461
0.3893258
0.3964340
0.3531701
0.3218205
0.3664966
0.3308279
0.3654257
0.3738105
0.3597743
0.4136063
0.3863304
0.3921849
0.4500459
0.4119511
0.4095347
0.4141847
0.3836177
0.3664662
0.3490144
0.3532783
0.3659714
0.3170860
0.3800639
0.3802642
0.3952369
0.3938273
0.4385472
0.3827855
0.4246245
0.4152648
0.3829902
0.3986423
0.4321109
0.3377395
0.3327452
0.3273358
0.3751191
0.3743763
0.3678043
0.4507238
0.4866902
0.4624820

Tuning parameter 'gamma' was held constant at a value of 0

Tuning parameter 'min_child_weight' was held constant at a
value of 1
Accuracy was used to select the optimal model using the
largest value.
The final values used for the model were nrounds = 100,
max_depth = 3, eta = 0.3, gamma = 0, colsample_bytree =
0.8, min_child_weight = 1 and subsample = 0.75.
```

```{r}
pred_xgb <- predict(modelo_xgb, t_test)
confusionMatrix(pred_xgb, t_test$diagnosis)
Confusion Matrix and Statistics

Reference
Prediction Impaired Control
Impaired       10       4
Control        12      56

Accuracy : 0.8049          
95% CI : (0.7026, 0.8842)
No Information Rate : 0.7317          
P-Value [Acc > NIR] : 0.08208         

Kappa : 0.4384          
Mcnemar's Test P-Value : 0.08012         

Sensitivity : 0.4545          
Specificity : 0.9333          
Pos Pred Value : 0.7143          
Neg Pred Value : 0.8235          
Prevalence : 0.2683          
Detection Rate : 0.1220          
Detection Prevalence : 0.1707          
Balanced Accuracy : 0.6939          

'Positive' Class : Impaired 
```

Future analysis is coming.. [Linkedin](https://www.linkedin.com/in/guilhermeagsouza/).