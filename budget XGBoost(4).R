######## Modeling 2 : XG boosting
library("xgboost")  # the main algorithm
library("archdata") # for the sample dataset
library("caret")    # for the confusionmatrix() function (also needs e1071 package)
library("dplyr")    # for some data preperation
library("Ckmeans.1d.dp") # for xgb.ggplot.importance



tour <- read.csv('C:\\Users\\dpfk1\\Desktop\\SEIN\\P-sat\\주제분석\\한국여행 컨설팅\\tour data.csv', header = T)

####################### make total_pay_except_shop ###############3
tour %<>% mutate(total_pay_except_shop = total_pay - pay_shopping)
tour %<>% filter(total_pay_except_shop>=0)


###################### make total_pay categorical variable ##################

pay_cate <- vector('numeric', length = nrow(tour))

pay_cate[tour$total_pay_except_shop >=0 & tour$total_pay_except_shop <= 350] <- 0
pay_cate[tour$total_pay_except_shop >350 & tour$total_pay_except_shop <= 600] <- 1
pay_cate[tour$total_pay_except_shop >600 & tour$total_pay_except_shop <= 950 ] <- 2
pay_cate[tour$total_pay_except_shop > 950] <- 3

table(pay_cate)

tour$pay_cate <- pay_cate

################# make visit_place dummy variable ##############
tours <- tour
tours %<>% gather(x, visit_place, visit_place1:visit_place48, na.rm = T)
tours %<>% arrange(X)
tours$visit_place <- as.factor(tours$visit_place)

library(fastDummies) 
tours <- dummy_cols(tours, select_columns = "visit_place") 



tours %<>% select(-visit_place) 
visitplace <- tours %>% group_by(X) %>% summarise_at(vars(starts_with('visit_place')), sum)

#################### choose 1:135 visit place ################

visitplace_final <- data.frame(x = 1:12746)

for(i in 1:135){
  visitplace1 <- visitplace %>% select(sprintf('visit_place_%d', i))
  visitplace_final <- cbind(visitplace_final, visitplace1)
}

visitplace_final %<>% select(-x)




################## select tour data ###################

tour_pay <- tour %>% select(revisit_code, decision_time, purpose, companion, num_companion, activity1,
                            visit_seoul, period, type, accomodations_hotel, accomodations_guesthouse, accomodations_condo,
                            accomodations_friend, accomodations_official, accomodations_temple, accomodations_homestay,
                            nation, gender, job, age, area_total, site_total, spring, summer, fall, winter, pay_cate)

################# unite tour_pay visitplace ##################

tour_pay <- cbind(tour_pay, visitplace_final)


str(tour_pay)



################ make factor variable to dummy variable ################ 

library(dummies)

revisit_code <- dummy(tour_pay$revisit_code)
revisit_code <- as.data.frame(revisit_code)

purpose <- dummy(tour_pay$purpose)
purpose <- as.data.frame(purpose)

companion <- dummy(tour_pay$companion)
companion <- as.data.frame(companion)

activity1 <- dummy(tour_pay$activity1)
activity1 <- as.data.frame(activity1)

type <- dummy(tour_pay$type)
type <- as.data.frame(type)

nation <- dummy(tour_pay$nation)
nation <- as.data.frame(nation)

gender <- dummy(tour_pay$gender)
gender <- as.data.frame(gender)

job <- dummy(tour_pay$job)
job <- as.data.frame(job)

age <- dummy(tour_pay$age)
age <- as.data.frame(age)

tour_pay %<>% select(-revisit_code, -purpose, -companion, -activity1, -type, -nation, -gender, -job, -age)
tour_pay <- cbind(tour_pay, revisit_code, purpose, companion, activity1, type, nation, gender, job, age)

str(tour_pay)



################## devide train test #################

library(caret)

set.seed(0)
# Make split index
train_index <- sample(1:nrow(tour_pay), nrow(tour_pay)*0.7)
# Full data set
data_variables <- as.matrix(tour_pay[,-18])
data_label <- tour_pay[,'pay_cate']
data_matrix <- xgb.DMatrix(data = as.matrix(tour_pay), label = data_label)
# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)



##################### grid search for the greatest param #################
library(caret)
tune_grid <- expand.grid(
  nrounds = 200,
  eta = 0.05,
  max_depth = c(4, 6, 8),
  colsample_bytree = c(0.5, 0.6, 0.7),
  min_child_weight = c(1, 3, 5, 7),
  subsample = c(0.5, 0.6, 0.7),
  gamma = 1
)


tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 5, # with n folds 
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)


xgb_tune <- caret::train(
  x = train_data,
  y = as.factor(train_label),
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)

result <- xgb_tune$results %>% arrange(desc(Accuracy))

result




############### param ##################

numberOfClasses <- length(unique(tour_pay$pay_cate))
xgb_params <- list(objective = "multi:softmax",
                   subsample = 0.7,
                   colsample_bytree = 0.7,
                   eta = 0.05,
                   max_depth = 8,
                   min_child_weight = 1,
                   num_class = numberOfClasses)
nround    <- 1000 # number of XGBoost rounds
cv.nfold  <- 5



# Fit cv.nfold * cv.nround XGB models and save OOF predictions

cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   early_stopping_rounds = 200,
                   verbose = T,
                   prediction = TRUE)



################ modeling #################

watchlist <- list(train=train_matrix, test=test_matrix)


xg_model <- xgb.train(params = xgb_params,
                      data = train_matrix, 
                      nrounds = nround,
                      early_stopping_rounds = 200,
                      watchlist=watchlist, ##test list를 보면서 early stopping을 진해
                      verbose = T,
                      prediction = TRUE)

############### evaluation ###################

# Predict hold-out test set
test_pred <- predict(xg_model, newdata = test_matrix)

confusionMatrix(factor(test_pred) , factor(test_label), mode = 'everything')

confusionMatrix(factor(test_pred) , factor(test_label), mode = 'everything')$byClass


################# F1 score ######################

8/ (1/0.6174377 + 1/0.4514731  + 1/0.4432624 + 1/0.5896130 + 1/0.6666667 + 1/0.4943074 + 1/0.6632302 + 1/ 0.2920561)
