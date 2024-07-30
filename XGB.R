library(dplyr)
library(e1071)
library(caret)
library(UBL)
library(xgboost)
library(Matrix)

crime_cleaned<-read.csv("data/cleaned_v2.csv",stringsAsFactors = FALSE)
dim(crime_cleaned)
colnames(crime_cleaned)


cat_col_names<- c("State","Agency_type","Month","Crime_type","Crime_status","Victim_sex",
                  "Victim_race","Offender_sex","Offender_race","Weapon","Relationship",
                  "Crime_cause","County","Offender_demo") 

crime_cleaned <- crime_cleaned %>%
  mutate(across(all_of(cat_col_names), as.factor))

#...............................................................................................

table(crime_cleaned$Offender_demo)


#Recode target variable so that labels to be in [0,5] range
crime_cleaned$Offender_demo <- as.numeric(crime_cleaned$Offender_demo)
crime_cleaned$Offender_demo <- crime_cleaned$Offender_demo -1

table(crime_cleaned$Offender_demo)

# Define the training dataset
df_training<- subset(crime_cleaned,select = -c(Offender_sex,Offender_age))
dim(df_training)


#create a 70/30 training/test set split
set.seed(100)
training_idx <- sample(nrow(df_training), 0.7*nrow(df_training), replace = FALSE)
train<-df_training[training_idx,]
test<-df_training[-training_idx,]

X_train = data.matrix(train[,-length(train)])                  
y_train = train[,length(train)]                               

X_test = data.matrix(test[,-length(test)])                    
y_test = test[,length(test)]      


#one hot encode the categorical variables
sp_mat_train <- sparse.model.matrix( Offender_demo~ ., data = train)[, -1]   # -1 - remove intercept
sp_mat_test <- sparse.model.matrix( Offender_demo~ ., data = test)[, -1]


# convert the train and test data into xgboost matrix type.
xgboost_train = xgb.DMatrix(data=sp_mat_train, label=y_train)
xgboost_test = xgb.DMatrix(data=sp_mat_test, label=y_test)


# Define the parameters for the xgboost model
xgb_params <- list(
  booster = "gbtree",
  eta = 1,
  max_depth = 10,
  gamma = 10,
  subsample = 0.7,
  min_child_weight = 7,
  objective = "multi:softmax",
  eval_metric = "merror",
  
  num_class = 6
)


start_time <- Sys.time()
xgb_model <- xgb.train(params = xgb_params,
                       data = xgboost_train,
                       nrounds=1500,
                       verbose = 2)  
end_time <- Sys.time()
due <- round(end_time - start_time,2)
print(due)

#Generate model summary
print(xgb_model)

# make predictions
xgb_preds <- predict(xgb_model, xgboost_test, reshape = TRUE)

#generate confusion matrix
conf_mat<- confusionMatrix( factor(xgb_preds, levels = 0:5),
                            factor(y_test, levels = 0:5))
print(conf_mat)


# Generate the Feature importance plot
mat <- xgboost::xgb.importance (model = xgb_model)
xgb.plot.importance (importance_matrix = mat[1:30]) 

# get the top 30 feature names to increase the readability of the plot
top_30<- mat$Feature[1:30]





#.......................................tuning...........................#

#Hyperparamter tuning
#Step 1 : Tune the learning rate

params_grid <-  expand.grid(
                            eta = 0.01,
                            nrounds = 1500,
                            max_depth = c(5,7,8),
                            gamma = c(0,2,4),
                            subsample = c(0.5,0.7),
                            min_child_weight = c(2,5,8),
                            colsample_bytree = c(0.6,0.8))

# Train model with grid search
train_control <- trainControl(method = "cv", number = 3, search = "grid",verboseIter = TRUE)
tuned_xgb <- train(Offender_demo~., data = train, method = "xgbTree", trControl = train_control, tuneGrid = params_grid)

# summarising the results
print(tuned_xgb)


#get the best eta
tuned_xgb$bestTune
#nrounds  max_depth  eta  gamma colsample_bytree min_child_weight subsample
#    1500         8  1     0              0.6                1         1





#.............................undersampling.................................#

undersampled<- RandUnderClassif( Offender_demo ~ ., dat = train, C.perc = "balance", repl = FALSE)

table(undersampled$Offender_demo)

X_train_un = data.matrix(undersampled[,-length(undersampled)])                  
y_train_un = undersampled[,length(undersampled)]                               

X_test_un = data.matrix(test[,-length(test)])                    
y_test_un = test[,length(test)]      


#one hot encode the categorical variables
sp_mat_train <- sparse.model.matrix( Offender_demo~ ., data = undersampled)[, -1]   # -1 - remove intercept
sp_mat_test <- sparse.model.matrix( Offender_demo~ ., data = test)[, -1]


# convert the train and test data into xgboost matrix type.
xgboost_train = xgb.DMatrix(data=sp_mat_train, label=y_train_un)
xgboost_test = xgb.DMatrix(data=sp_mat_test, label=y_test_un)

# Define the parameters for the xgboost model
xgb_params <- list(
  booster = "gbtree",
  eta = 1,
  max_depth = 10,
  gamma = 10,
  subsample = 0.7,
  min_child_weight = 7,
  objective = "multi:softmax",
  eval_metric = "merror",

  num_class = 6
)

start_time <- Sys.time()
xgb_model <- xgb.train(params = xgb_params,
                       data = xgboost_train,
                       nrounds=1500,
                       verbose = 2)  
end_time <- Sys.time()
due <- round(end_time - start_time,2)
print(due)

#Generate model summary
print(xgb_model)


# make predictions
xgb_preds <- predict(xgb_model, xgboost_test, reshape = TRUE)


#generate confusion matrix
conf_mat<- confusionMatrix(factor(xgb_preds, levels =0:5),
                            factor(y_test, levels =):5)
print(conf_mat)



