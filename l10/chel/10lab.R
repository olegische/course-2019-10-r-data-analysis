#keras

library(keras)
library(tensorflow)
library(dummies)
library(caret)


#Путь к файлам с данными
setwd("D:/data")

# Описание данных
#trustLevel - Индивидуальный уровень доверия клиента. 6: Высочайшая надежность
#totalScanTimeInSeconds - Общее время в секундах между первым и последним отсканированным продуктом
#grandTotal - Общая стоимость отсканированных продуктов	
#lineItemVoids - Количество аннулированных сканирований	
#scansWithoutRegistration - Количество попыток сканирования без какого-либо сканирования (неудачное сканирование)	
#quantityModification - Число изменений количества товаров для одного из сканируемых продуктов	
#scannedLineItemsPerSecond - Среднее количество отсканированных продуктов в секунду	
#valuePerSecond - Средняя общая стоимость отсканированных продуктов в секунду	
#lineItemVoidsPerPosition - Отношение числа аннулированных сканирований к общему числу аннулированных и не аннулированных сканирований	
#fraud - Классификатор как мошенничество (1) или не мошенничество (0)


#Читаем файл данных 
trainData<- read.csv("train.csv", header =T, sep = "|")
testData<- read.csv("test.csv", header =T, sep = "|") 
realData <- read.csv("realclass.csv", header =T, sep = "|") 


#install_tensorflow()
#install_keras()


if (max(trainData$trustLevel) >1) {trainData$trustLevel<-trainData$trustLevel/max(trainData$trustLevel)}
if (max(trainData$totalScanTimeInSeconds) >1){trainData$totalScanTimeInSeconds<-trainData$totalScanTimeInSeconds/max(trainData$totalScanTimeInSeconds)}
if (max(trainData$grandTotal) >1) {trainData$grandTotal<-trainData$grandTotal/max(trainData$grandTotal)}
if (max(trainData$lineItemVoids) >1) {trainData$lineItemVoids<-trainData$lineItemVoids/max(trainData$lineItemVoids)}
if (max(trainData$scansWithoutRegistration) >1) {trainData$scansWithoutRegistration<-trainData$scansWithoutRegistration/max(trainData$scansWithoutRegistration)}
if (max(trainData$quantityModifications) > 1) {trainData$quantityModifications<-trainData$quantityModifications/max(trainData$quantityModifications)}
if (max(trainData$scannedLineItemsPerSecond) > 1) {trainData$scannedLineItemsPerSecond<-trainData$scannedLineItemsPerSecond/max(trainData$scannedLineItemsPerSecond)}
if (max(trainData$valuePerSecond) >1) {trainData$valuePerSecond<-trainData$valuePerSecond/max(trainData$valuePerSecond)}
if (max(trainData$lineItemVoidsPerPosition) >1) {trainData$lineItemVoidsPerPosition<-trainData$lineItemVoidsPerPosition/max(trainData$lineItemVoidsPerPosition)}


if (max(testData$trustLevel) >1) {testData$trustLevel<-testData$trustLevel/max(testData$trustLevel)}
if (max(testData$totalScanTimeInSeconds) >1){testData$totalScanTimeInSeconds<-testData$totalScanTimeInSeconds/max(testData$totalScanTimeInSeconds)}
if (max(testData$grandTotal) >1) {testData$grandTotal<-testData$grandTotal/max(testData$grandTotal)}
if (max(testData$lineItemVoids) >1) {testData$lineItemVoids<-testData$lineItemVoids/max(testData$lineItemVoids)}
if (max(testData$scansWithoutRegistration) >1) {testData$scansWithoutRegistration<-testData$scansWithoutRegistration/max(testData$scansWithoutRegistration)}
if (max(testData$quantityModifications) > 1) {testData$quantityModifications<-testData$quantityModifications/max(testData$quantityModifications)}
if (max(testData$scannedLineItemsPerSecond) > 1) {testData$scannedLineItemsPerSecond<-testData$scannedLineItemsPerSecond/max(testData$scannedLineItemsPerSecond)}
if (max(testData$valuePerSecond) >1) {testData$valuePerSecond<-testData$valuePerSecond/max(testData$valuePerSecond)}
if (max(testData$lineItemVoidsPerPosition) >1) {testData$lineItemVoidsPerPosition<-testData$lineItemVoidsPerPosition/max(testData$lineItemVoidsPerPosition)}

#library(reticulate)
#use_python("C:/Users/user/AppData/Local/Programs/Python/Python38")
#use_python("D:/data/Python38")
#library(PythonInR)

model <- keras_model_sequential() 



#Activation
#activation_relu(x, alpha = 0, max_value = NULL, threshold = 0)
#activation_elu(x, alpha = 1)
#activation_selu(x)
#activation_hard_sigmoid(x)
#activation_linear(x)
#activation_sigmoid(x)
#activation_softmax(x, axis = -1)
#activation_softplus(x)
#activation_softsign(x)
#activation_tanh(x)
#activation_exponential(x)

#kernel_initializer
#initializer_constant
#initializer_glorot_normal
#initializer_he_normal
#initializer_he_uniform
#initializer_identity
#initializer_lecun_normal
#initializer_lecun_uniform
#initializer_ones
#initializer_orthogonal
#initializer_random_normal
#initializer_random_uniform
#initializer_truncated_normal
#initializer_variance_scaling
#initializer_zeros



#Типы слоев
#Рекурентные
#RNN
#LSTM

#Встроенные
#Embedding
#Dense -обычный



model %>% 
  
  # First hidden layer
  # layer_dense(
  #   units              = 9, 
  #   kernel_initializer = "uniform", 
  #   activation         = "relu", 
  #   input_shape        = ncol(trainData)-1) %>% 
  # 
  # # Dropout to prevent overfitting
  # layer_dropout(rate = 0.1) %>%
  
layer_embedding(
  input_dim =  9,
  output_dim = 256) %>%
  
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  
  #  # Second hidden layer
  #  layer_dense(
  #    units              = 128, 
  #    kernel_initializer = "uniform", 
  #    activation         = "relu") %>% 
  
  #  # Dropout to prevent overfitting
  #  layer_dropout(rate = 0.1) %>%
  
  
# # Second hidden layer
# layer_dense(
#   units              = 128, 
#   kernel_initializer = "uniform", 
#   activation         = "relu") %>% 
# 
# # Dropout to prevent overfitting
# layer_dropout(rate = 0.1) %>%

# Second hidden layer
layer_lstm(
  units              = 128,
  return_sequences = TRUE) %>%
  
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  
  # Second hidden layer
  layer_lstm(
    units              = 128,
    return_sequences = TRUE) %>%
  
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%  
  
  # Output layer
  layer_dense(
    units              = 1, 
    kernel_initializer = "uniform", 
    activation         = "sigmoid")

#Optimizer
#optimizer_adam - стохастический
#optimizer_adadelta - адаптивный
#optimizer_adagrad - адаптивный разновидность градиентного 
#optimizer_adamax разновидность Adam 
#optimizer_nadam - Adam type
#optimizer_rmsprop
#optimizer_sgd - градиентный

#summary(model)

#metric
#metric_binary_accuracy(y_true, y_pred)
#metric_binary_crossentropy(y_true, y_pred)
#metric_categorical_accuracy(y_true, y_pred)
#metric_categorical_crossentropy(y_true, y_pred)
#metric_cosine_proximity(y_true, y_pred)
#metric_hinge(y_true, y_pred)
#metric_kullback_leibler_divergence(y_true, y_pred)
#metric_mean_absolute_error(y_true, y_pred)
#metric_mean_absolute_percentage_error(y_true, y_pred)
#metric_mean_squared_error(y_true, y_pred)
#metric_mean_squared_logarithmic_error(y_true, y_pred)
#metric_poisson(y_true, y_pred)
#metric_sparse_categorical_crossentropy(y_true, y_pred)
#metric_squared_hinge(y_true, y_pred)
#metric_top_k_categorical_accuracy(y_true, y_pred, k = 5)
#metric_sparse_top_k_categorical_accuracy(y_true, y_pred, k = 5)
#custom_metric(name, metric_fn)

#Loss function
#loss_mean_squared_error(y_true, y_pred)
#loss_mean_absolute_error(y_true, y_pred)
#loss_mean_absolute_percentage_error(y_true, y_pred)
#loss_mean_squared_logarithmic_error(y_true, y_pred)
#loss_squared_hinge(y_true, y_pred)
#loss_hinge(y_true, y_pred)
#loss_categorical_hinge(y_true, y_pred)
#loss_logcosh(y_true, y_pred)
#loss_categorical_crossentropy(y_true, y_pred)
#loss_sparse_categorical_crossentropy(y_true, y_pred)
#loss_binary_crossentropy(y_true, y_pred)
#loss_kullback_leibler_divergence(y_true, y_pred)
#loss_poisson(y_true, y_pred)
#loss_cosine_proximity(y_true, y_pred)


model %>% compile(
  loss="binary_crossentropy",
  optimizer = "rmsprop",
  metrics = "accuracy"
)


#names(trainData)

history <- model %>% fit(as.matrix(trainData[,-10]), 
                         as.matrix(trainData$fraud),
                         epochs = 200, 
                         batch_size = dim(trainData)[1],# 128, 
                         validation_split = 0.2,
                         verbose = TRUE)


plot(history)+theme_bw()

model %>% evaluate(as.matrix(testData)[1:10000,],as.matrix(realData$fraud)[1:10000])

# Predicted Class
res<-predict_classes(object = model, x = as.matrix(testData)[1:10000,]) %>% as.vector()

# Predicted Class Probability
res_prob <- predict_proba(object = model, x = as.matrix(testData)[1:10000,]) %>% as.vector()

library(ModelMetrics)
# AUC
auc(realData$fraud[1:10000], res_prob)

library(verification)
roc.plot(realData$fraud[1:10000], res_prob)
