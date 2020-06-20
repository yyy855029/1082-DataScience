# import package
if(!require('optparse')){install.packages('optparse')}
if(!require('randomForest')){install.packages('randomForest')}
if(!require('mltools')){install.packages('mltools')}
if(!require('data.table')){install.packages('data.table')}

library(optparse)
library(randomForest)
library(mltools)
library(data.table)


# calculate accuracy function
accuracy<-function(data){
  resultframe<-data.frame(truth=data$Survived,
                          pred=predict(model,data,type='class'))
  rtab<-table(resultframe) 
  acc<-sum(diag(rtab))/sum(rtab)
  # print(rtab)
  return(acc)
}


# Age value function
Age_value<-function(x){
  range_list<-list(c(0:11),c(12:18),c(19:22),c(23:27),c(28:33),c(34:40),c(41:100))
  value_list<-c('Seventh','Sixth','Fifth','Fourth','Third','Second','First')
  for (i in 1:length(range_list)) {
    if (round(x) %in% unlist(range_list[i])) {
      return(value_list[i])
    }
  }
}


# process data function
preprocess_data<-function(train_data,test_data){
  # PassengerId/Ticket column
  train_data<-train_data[-c(1,9)]
  test_data<-test_data[-c(1,8)]
  
  # Pclass column
  train_data[,'Pclass']<-as.factor(train_data[,'Pclass'])
  test_data[,'Pclass']<-as.factor(test_data[,'Pclass'])
  
  # Survived column
  train_data[,'Survived']<-as.factor(train_data[,'Survived'])
  
  # Name column
  rare_title<-c('Dona','the Countess','Capt','Col','Don','Dr','Major','Rev','Jonkheer','Sir','Lady')
  train_data[,'Name']=gsub('(.*, )|(\\..*)','',train_data[,'Name'])
  test_data[,'Name']=gsub('(.*, )|(\\..*)','',test_data[,'Name'])
  train_data[train_data[,'Name']=='Mlle','Name']=rep(c('Miss'),times=sum(train_data[,'Name']=='Mlle'))
  test_data[test_data[,'Name']=='Mlle','Name']=rep(c('Miss'),times=sum(test_data[,'Name']=='Mlle'))
  train_data[train_data[,'Name']=='Ms','Name']=rep(c('Mrs'),times=sum(train_data[,'Name']=='Ms'))
  test_data[test_data[,'Name']=='Ms','Name']=rep(c('Mrs'),times=sum(test_data[,'Name']=='Ms'))
  train_data[train_data[,'Name']=='Mme','Name']=rep(c('Mrs'),times=sum(train_data[,'Name']=='Mme'))
  test_data[test_data[,'Name']=='Mme','Name']=rep(c('Mrs'),times=sum(test_data[,'Name']=='Mme'))
  train_data[train_data[,'Name'] %in% rare_title,'Name']=rep(c('Rare'),times=sum(train_data[,'Name'] %in% rare_title))
  test_data[test_data[,'Name'] %in% rare_title,'Name']=rep(c('Rare'),times=sum(test_data[,'Name'] %in% rare_title))
  train_data[,'Name']<-as.factor(train_data[,'Name'])
  test_data[,'Name']<-as.factor(test_data[,'Name'])
  
  # Sex column
  train_data[,'Sex']=sapply(train_data[,'Sex'],function(x){ifelse(x=='male',0,1)})
  test_data[,'Sex']=sapply(test_data[,'Sex'],function(x){ifelse(x=='male',0,1)})
  
  # Age column
  age.mean<-mean(train_data[,'Age'],na.rm=TRUE)
  age.std<-sd(train_data[,'Age'],na.rm=TRUE)
  train_data[is.na(train_data[,'Age']),'Age']=round(sample((age.mean-age.std):(age.mean+age.std),sum(is.na(train_data[,'Age'])),replace=T))
  test_data[is.na(test_data[,'Age']),'Age']=round(sample((age.mean-age.std):(age.mean+age.std),sum(is.na(test_data[,'Age'])),replace=T))
  train_data[,'Age_Range']=sapply(train_data[,'Age'],Age_value)
  test_data[,'Age_Range']=sapply(test_data[,'Age'],Age_value)
  train_data[,'Age_Range']<-as.factor(train_data[,'Age_Range'])
  test_data[,'Age_Range']<-as.factor(test_data[,'Age_Range'])
  
  # Fsize column
  train_data[,'Fsize_n']=train_data[,'SibSp']+train_data[,'Parch']+1
  test_data[,'Fsize_n']=test_data[,'SibSp']+test_data[,'Parch']+1
  
  # Fare column
  test_data[is.na(test_data[,'Fare']),'Fare']=median(train_data[,'Fare'],na.rm=TRUE)
  train_data[,'Log_Fare']=log(train_data[,'Fare']+1)
  test_data[,'Log_Fare']=log(test_data[,'Fare']+1)
  
  # Cabin column
  train_data[,'Cabin']=sapply(train_data[,'Cabin'],function(x){substr(x,1,1)})
  test_data[,'Cabin']=sapply(test_data[,'Cabin'],function(x){substr(x,1,1)})
  train_data[train_data[,'Cabin']=='','Cabin']=rep(c('U'),times=sum(train_data[,'Cabin']==''))
  test_data[test_data[,'Cabin']=='','Cabin']=rep(c('U'),times=sum(test_data[,'Cabin']==''))
  train_data[,'Cabin']<-as.factor(train_data[,'Cabin'])
  test_data[,'Cabin']<-as.factor(test_data[,'Cabin'])
  
  # Embarked column
  train_data[train_data[,'Embarked']=='','Embarked']=rep(c('S'),times=sum(train_data[,'Embarked']==''))
  train_data[,'Embarked']<-as.factor(train_data[,'Embarked'])
  test_data[,'Embarked']<-as.factor(test_data[,'Embarked'])
  
  train_data<-one_hot(data.table(train_data),c('Pclass','Name','Embarked','Cabin','Age_Range'))
  test_data<-one_hot(data.table(test_data),c('Pclass','Name','Embarked','Cabin','Age_Range'))
  output<-list(train=train_data,test=test_data)
  return(output)
  
}


# set random seed
set.seed(80)


# read Rscript elements
option_list<-list(
  make_option(c('-f','--fold'),type='integer',default=FALSE,
              action='store',help='fold numbers of cross validation'
  ),
  make_option('--train',type='character',default=FALSE,
              action='store',help='train data'
  ),
  make_option('--test',type='character',default=FALSE,
              action='store',help='test data'
  ),
  make_option(c('-r','--report'),type='character',default=FALSE,
              action='store',help='report performance'
  ),
  make_option(c('-p','--predict'),type='character',default=FALSE,
              action='store',help='predict result'
  )
)

opt=parse_args(OptionParser(option_list=option_list))

k_fold<-opt$fold
train_data_name<-opt$train
test_data_name<-opt$test
report_performance_name<-opt$report
predict_result_name<-opt$predict


# print(k_fold)
# print(train_data_name)
# print(test_data_name)
# print(report_performance_name)
# print(predict_result_name)


# read raw data
train_data<-read.csv(train_data_name,header=T)
test_data<-read.csv(test_data_name,header=T)
test_ids<-test_data[,'PassengerId']

# preprocess raw data
output<-preprocess_data(train_data,test_data)
new_train_data<-output$train
new_test_data<-output$test

# shuffle data
train_data_size<-dim(new_train_data)[1]
random_rows<-sample(train_data_size)
random_train_data<-new_train_data[random_rows,]
row.names(random_train_data)<-NULL

# View(random_train_data)

# split index vector
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
  model<-randomForest(Survived~.,data=subset(train_data,select=-c(Embarked_,Cabin_T)),ntree=1000)
  
  sets[i]=paste0('fold',as.character(i))
  trainings[i]=round(accuracy(train_data),2)
  validations[i]=round(accuracy(valid_data),2)
  tests[i]=round(accuracy(test_data),2)
}


sets<-c(sets,'ave.')
trainings<-c(trainings,round(mean(trainings),2))
validations<-c(validations,round(mean(validations),2))
tests<-c(tests,round(mean(tests),2))


# save report performance
report_performance<-data.frame(set=sets,training=trainings,validation=validations,test=tests)
write.csv(report_performance,report_performance_name,row.names=FALSE,quote=FALSE)

# print(trainings)
# print(validations)
# print(tests)


# save predict result
model<-randomForest(Survived~.,data=subset(random_train_data,select=-c(Embarked_,Cabin_T)),ntree=1000)
test_predict<-predict(model,new_test_data)
predict_result<-data.frame(PassengerId=test_ids,Survived=test_predict)
write.csv(predict_result,predict_result_name,row.names=FALSE,quote=FALSE)


