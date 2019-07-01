############# DNN
library(AmesHousing)
data(Ameshousing)
data_onehot <- model.matrix(~ ., AmesHousing::make_ames())[, -1] %>% as.data.frame()
str(data_onehot)
## split

set.seed(123)
split <- rsample::initial_split(data_onehot, prop = .7, strata = "Sale_Price")
train <- rsample::training(split)
test  <- rsample::testing(split)

# Create & standardize feature sets
# training features
train_x <- train %>% dplyr::select(-Sale_Price)

train_x<-apply(train_x,2,scale)









convert data to h2o object --> here we use our original data as h2o will
# perform one-hot encoding and standardization for us.
ames_h2o <- train %>%
  mutate(Sale_Price_log = log(Sale_Price)) %>%
  as.h2o()
##
test_x <- test %>% dplyr::select(-Sale_Price)
mean=colMeans(test_x)
std=apply(test_x,2,sd)
test_x <- scale(test_x, center = mean, scale = std)


# Create & transform response sets
train_y <- log(train$Sale_Price)
test_y  <- log(test$Sale_Price)

##
colSums(is.na(train_x
              ))


AmesHousing::make_ames()


##
# zero variance variables (after one hot encoded) cause NaN so we need to remove
zv <- which(colSums(is.na(train_x)) > 0, useNames = FALSE)
train_x <- train_x[, -zv]
test_x  <- test_x[, -zv]


##, we initiate our sequential feedforward DNN architecture 

library(keras)
library(dplyr)


model <- keras_model_sequential() %>%
  layer_dense(units = 10, input_shape = ncol(train_x)) %>%
  layer_dense(units = 5) %>%
  layer_dense(units = 1)

### adding activation function
model <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = "relu", input_shape = ncol(train_x)) %>%
  layer_dense(units = 5, activation = "relu") %>%
  layer_dense(units = 1)

model <- keras_model_sequential() %>%
  
  # network architecture
  layer_dense(units = 10, activation = "relu", input_shape = ncol(train_x)) %>%
  layer_dense(units = 5, activation = "relu") %>%
  layer_dense(units = 1) %>%
  
  # backpropagation
  compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )



####
model <- keras_model_sequential() %>%
  
  # network architecture
  layer_dense(units = 10, activation = "relu", input_shape = ncol(train_x)) %>%
  layer_dense(units = 5, activation = "relu") %>%
  layer_dense(units = 1) %>%
  
  # backpropagation
  compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )

# train our model
learn <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 25,
  batch_size = 32,
  validation_split = .2,
  verbose = FALSE
)

learn
## Trained on 1,643 samples, validated on 411 samples (batch_size=32, epochs=25)
## Final epoch (plot to see history):
## val_mean_absolute_error: 1.034


#Model tuning
#Now that we have an understanding of producing and running our DNN model, the next task is to find an optimal model by tuning different parameters. There are many ways to tune a DNN. Typically, the tuning process follows these general steps; however, there is often a lot of iteration among these:
  
                #Adjust model capacity (layers & nodes)
#Increase epochs if you do not see a flatlined loss function
         #Add batch normalization
  #Add dropout
            #Add weight regularization
##Adjust learning rate



#Adjust model capacity (layers & nodes)

# first start with high capacity model and then see deviation for the cross validation set 

model <- keras_model_sequential() %>%
  
  # network architecture
  layer_dense(units = 500, activation = "relu", input_shape = ncol(train_x)) %>%
  layer_dense(units = 250, activation = "relu", input_shape = ncol(train_x)) %>%
  layer_dense(units = 125, activation = "relu") %>%
  layer_dense(units = 1) %>%
  
  # backpropagation
  compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )

# train our model
learn <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 25,
  batch_size = 32,
  validation_split = .2,
  verbose = FALSE
)

plot(learn)


##   
################# BATCH NORMALIZATION
 
model <- keras_model_sequential() %>%
  
  # network architecture
  layer_dense(units = 100, activation = "relu", input_shape = ncol(train_x)) %>%
  layer_batch_normalization() %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dense(units = 1) %>%
  
  # backpropagation
  compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )

# train our model
learn <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 25,
  batch_size = 32,
  validation_split = .2,
  verbose = FALSE,
  callbacks = list(
    callback_early_stopping(patience = 2)
  )
)

learn
## Trained on 1,643 samples, validated on 411 samples (batch_size=32, epochs=25)
## Final epoch (plot to see history):
## val_mean_absolute_error: 0.2016
##                val_loss: 0.07756
##     mean_absolute_error: 0.1786
##                    loss: 0.05482

plot(learn)

##                val_loss: 4.078
##     mean_absolute_error: 0.4967
##                    loss: 0.5555
plot(learn)




############ using h20

# install.packages('h2o')
library(h2o)
h2o.init(nthreads = -1)


#convert data to h2o object --> here we use our original data as h2o will
# perform one-hot encoding and standardization for us.
ames_h2o <- train %>%
  mutate(Sale_Price_log = log(Sale_Price)) %>%
  as.h2o()

response <- "Sale_Price_log"
predictors<-setdiff(colnames(train),"Sale_Price")



# train a model with defined parameters
model <- h2o.deeplearning(
  x = predictors, 
  y = response, 
  training_frame = ames_h2o,
  nfolds = 10,                                 # 10-fold CV
  keep_cross_validation_predictions = TRUE,    # retain CV prediction values
  distribution = "gaussian",                   # output is continuous 
  activation = "Rectifier",                    # use ReLU in hidden layers
  hidden = c(100, 50),                         # two hidden layers
  epochs = 25,                                 # number of epochs to perform
  stopping_metric = "MSE",                     # loss function is MSE
  stopping_rounds = 5,                         # allows early stopping
  l2 = 0.001,                                  # L2 norm weight regularization
  mini_batch_size = 32                         # batch sizes
)

plot(model, timestep = "epochs", metric = "rmse")
print(h2o.mse(model, xval = TRUE))
print(h2o.rmse(model, xval = TRUE))

# variable importance
h2o.varimp_plot(model, 20)
h2o.partialPlot(model, ames_h2o, cols = "Gr_Liv_Area")


# create hyperparameter grid search
hyper_params <- list(
  activation = c("Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout"), 
  l1 = c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1),
  l2 = c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1)
)

search_criteria <- list(strategy = "RandomDiscrete", max_runtime_secs = 120)
splits <- h2o.splitFrame(ames_h2o, ratios = 0.8, seed = 1)

# train the grid
dl_grid <- h2o.grid(
  algorithm = "deeplearning",
  x = predictors, 
  y = response,
  grid_id = "dl_grid",
  training_frame = splits[[1]],
  validation_frame = splits[[2]],
  seed = 1,
  hidden = c(100, 50),
  hyper_params = hyper_params,
  search_criteria = search_criteria
)


dl_gridperf <- h2o.getGrid(
  grid_id = "dl_grid", 
  sort_by = "mse", 
  decreasing = TRUE
)
print(dl_gridperf)
best_dl_model_id <- dl_gridperf@model_ids[[1]]
best_dl <- h2o.getModel(best_dl_model_id)
