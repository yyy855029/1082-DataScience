# import library
library(tidyverse)
library(randomForest)
library(rpart)


# calculate accuracy function
accuracy<-function(data){
  resultframe<-data.frame(truth=data$label,
                          pred=predict(model,data,type='class'))
  rtab<-table(resultframe) 
  acc<-sum(diag(rtab))/sum(rtab)
  # print(rtab)
  return(acc)
}


# set random seed
set.seed(80)

# read raw data
setwd('C:/Users/黃子瑋/Desktop/測試/datascience-hw7')
train_data<-read.csv('hw7_train.csv',header=T)
raw_test_data<-read.csv('hw7_test.csv',header=T)
test_ids<-raw_test_data[,'id']


# preprocess raw data
train_data[,'label']<-as.factor(train_data[,'label'])
as.data.frame(table(train_data[,'label']))


# shuffle data
train_data_size<-dim(train_data)[1]
random_rows<-sample(train_data_size)
random_train_data<-train_data[random_rows,]
row.names(random_train_data)<-NULL


# split index vector
k_fold<-5
test_percent<-1/k_fold
valid_percent<-test_percent

test_number<-round(test_percent*train_data_size)
valid_number<-test_number
train_number<-train_data_size-test_number-valid_number

total_index<-c(1:train_data_size)
total_index_list<-split(total_index,ceiling(seq_along(total_index)/test_number))


# process extra data situation
if(length(total_index_list)>k_fold){
  
  total_index_list[[k_fold]]<-c(total_index_list[[k_fold]],total_index_list[[k_fold+1]])
  total_index_list[[k_fold+1]]<-NULL
}


# calculate cross validation accuracy
sets<-c()
trainings<-c()
validations<-c()
tests<-c()


for(i in 1:length(total_index_list)){
  test_index<-total_index_list[[i]]
  valid_index<-total_index_list[[i%%k_fold+1]]
  train_index<-setdiff(setdiff(total_index,valid_index),test_index)
  
  train_data<-random_train_data[train_index,]
  valid_data<-random_train_data[valid_index,]
  test_data<-random_train_data[test_index,]
  
  # create model
  model<-randomForest(label~.,data=subset(train_data,select=-c(id,feature9)),ntree=500)
  
  sets[i]=paste0('fold',as.character(i))
  trainings[i]=round(accuracy(train_data),2)
  validations[i]=round(accuracy(valid_data),2)
  tests[i]=round(accuracy(test_data),2)
}

sets<-c(sets,'ave.')
trainings<-c(trainings,round(mean(trainings),2))
validations<-c(validations,round(mean(validations),2))
tests<-c(tests,round(mean(tests),2))

# print(tests)
# varImpPlot(model)


# save predict result
model<-randomForest(label~.,data=subset(random_train_data,select=-c(id,feature9)),ntree=500)
test_predict<-predict(model,raw_test_data)
predict_result<-data.frame(id=test_ids,label=test_predict)
write.csv(predict_result,'submission.csv',row.names=FALSE,quote=FALSE)
