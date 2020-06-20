# import package
if(!require('optparse')){install.packages('optparse')}

library(optparse)

# calculate accuracy function
accuracy<-function(data){
  resultframe<-data.frame(truth=data$SeriousDlqin2yrs,
                          pred=ifelse(predict(model,data,type='response')>0.1,1,0))
  rtab<-table(resultframe) 
  acc<-sum(diag(rtab))/sum(rtab)
  #print(rtab)
  return(acc)
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
test_ids<-test_data[,'X']


# process data function
preprocess_data<-function(train_data,test_data){
  # SeriousDlqin2yrs column
  train_data<-train_data[-c(1)]
  train_data[,'SeriousDlqin2yrs']=as.factor(train_data[,'SeriousDlqin2yrs'])
  test_data<-test_data[-c(1,2)]
  
  # RevolvingUtilizationOfUnsecuredLines column
  train_data[,'Log_RevolvingUtilizationOfUnsecuredLines']=log(train_data[,'RevolvingUtilizationOfUnsecuredLines']+1)
  test_data[,'Log_RevolvingUtilizationOfUnsecuredLines']=log(test_data[,'RevolvingUtilizationOfUnsecuredLines']+1)
  
  # MonthlyIncome column
  train_data[is.na(train_data[,'MonthlyIncome']),'MonthlyIncome']=mean(train_data[,'MonthlyIncome'],na.rm=TRUE)
  test_data[is.na(test_data[,'MonthlyIncome']),'MonthlyIncome']=mean(test_data[,'MonthlyIncome'],na.rm=TRUE)
  train_data[,'Log_MonthlyIncome']=log(train_data[,'MonthlyIncome']+1)
  test_data[,'Log_MonthlyIncome']=log(test_data[,'MonthlyIncome']+1)
  
  # NumberOfDependents column
  train_data[is.na(train_data[,'NumberOfDependents']),'NumberOfDependents']=mean(train_data[,'NumberOfDependents'],na.rm=TRUE)
  test_data[is.na(test_data[,'NumberOfDependents']),'NumberOfDependents']=mean(test_data[,'NumberOfDependents'],na.rm=TRUE)
  train_data[,'Log_NumberOfDependents']=log(train_data[,'NumberOfDependents']+1)
  test_data[,'Log_NumberOfDependents']=log(test_data[,'NumberOfDependents']+1)
  
  # DebtRatio column
  train_data[,'Log_DebtRatio']=log(train_data[,'DebtRatio']+1)
  test_data[,'Log_DebtRatio']=log(test_data[,'DebtRatio']+1)
  
  # NumberOfOpenCreditLinesAndLoans column
  train_data[,'Log_NumberOfOpenCreditLinesAndLoans']=log(train_data[,'NumberOfOpenCreditLinesAndLoans']+1)
  test_data[,'Log_NumberOfOpenCreditLinesAndLoans']=log(test_data[,'NumberOfOpenCreditLinesAndLoans']+1)
  
  # NumberRealEstateLoansOrLines column
  train_data[,'Log_NumberRealEstateLoansOrLines']=log(train_data[,'NumberRealEstateLoansOrLines']+1)
  test_data[,'Log_NumberRealEstateLoansOrLines']=log(test_data[,'NumberRealEstateLoansOrLines']+1)
  
  output<-list(train=train_data,test=test_data)
  
  return(output)
}


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
  model<-glm(SeriousDlqin2yrs~.,data=train_data,family=binomial(link='probit'))
  
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
model<-glm(SeriousDlqin2yrs~.,data=random_train_data,family=binomial(link='probit'))
test_predict<-predict(model,new_test_data,type='response')
predict_result<-data.frame(Id=test_ids,Probability=test_predict)
write.csv(predict_result,predict_result_name,row.names=FALSE,quote=FALSE)



