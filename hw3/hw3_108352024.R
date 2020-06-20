# import package
if(!require('optparse')){install.packages('optparse')}
if(!require('rpart')){install.packages('rpart')}
if(!require('randomForest')){install.packages('randomForest')}

library(optparse)
library(rpart)
library(randomForest)


# calculate accuracy function
accuracy<-function(data){
  resultframe<-data.frame(truth=data$V2,
                          pred=predict(model,data,type='class'))
  rtab<-table(resultframe) 
  acc<-sum(diag(rtab))/sum(rtab)
  # print(rtab)
  return(acc)
}


# set random seed
set.seed(80)


# read Rscript elements
option_list<-list(
  make_option(c('-f','--fold'),type='integer',default=FALSE,
              action='store',help='fold numbers of cross validation'
  ),
  make_option(c('-i','--input'),type='character',default=FALSE,
              action='store',help='input file'
  ),
  make_option(c('-o','--output'),type='character',default=FALSE,
              action='store',help='output file'
  )
)

opt=parse_args(OptionParser(option_list=option_list))

k_fold<-opt$fold
input_name<-opt$input
output_name<-opt$output


# print(k_fold)
# print(input_name)
# print(output_name)


# read data
data<-read.csv(input_name,header=F)
data_size<-dim(data)[1]
random_rows<-sample(data_size)
random_data<-data[random_rows,]
row.names(random_data)<-NULL

# View(random_data)


# split index vector
test_percent<-1/k_fold
valid_percent<-test_percent

test_number<-round(test_percent*data_size)
valid_number<-test_number
train_number<-data_size-test_number-valid_number

total_index<-c(1:data_size)
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
  
  train_data<-random_data[train_index,]
  valid_data<-random_data[valid_index,]
  test_data<-random_data[test_index,]
  
  # create model
  model<-randomForest(V2~V3+V4+V5+V6+V7+V8,data=train_data)
  
  sets[i]=paste0('fold',as.character(i))
  trainings[i]=round(accuracy(train_data),2)
  validations[i]=round(accuracy(valid_data),2)
  tests[i]=round(accuracy(test_data),2)
}


sets<-c(sets,'ave.')
trainings<-c(trainings,round(mean(trainings),2))
validations<-c(validations,round(mean(validations),2))
tests<-c(tests,round(mean(tests),2))


# save output
output<-data.frame(set=sets,training=trainings,validation=validations,test=tests)
write.csv(output,output_name,row.names=FALSE,quote=FALSE)

# print(trainings)
# print(validations)
# print(tests)



