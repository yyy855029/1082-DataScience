# import library
if (!require('optparse')) { install.packages('optparse') }

library(optparse)


# read Rscript elements
option_list<-list(
  make_option(c('-t','--target'),type='character',default=FALSE,
              action='store',help='male or female'
  ),
  make_option(c('-i','--input'),type='character',default=FALSE,
              action='store',help='input file'
  ),
  make_option(c('-o','--output'),type='character',default=FALSE,
              action='store',help='output file'
  )
)

opt=parse_args(OptionParser(option_list=option_list),positional_arguments=TRUE)

target_set<-c('female','male')
positive_type<-opt$options$target
negative_type<-target_set[target_set!=positive_type]
input_name<-c(opt$options$input,opt$args)
output_name<-opt$options$output
methods<-unlist(strsplit(unlist(lapply(strsplit(input_name,'/'),function(x)(tail(x,n=1)))),'.csv'))


# print(positive_type)
# print(negative_type)
# print(input_name)
# print(output_name)
# print(methods)


sensitivities<-c()
specificities<-c()
F1s<-c()
AUCs<-c()


# calculate score
for(i in 1:length(input_name)){
  # print(input_name[i])
  input<-read.table(input_name[i],sep=',', header=T)
  TP<-sum(input$reference==positive_type & input$prediction==positive_type)
  FP<-sum(input$reference==negative_type & input$prediction==positive_type)
  FN<-sum(input$reference==positive_type & input$prediction==negative_type)
  TN<-sum(input$reference==negative_type & input$prediction==negative_type)
  pos_scores<-input$pred.score[input$reference==positive_type]
  neg_scores<-input$pred.score[input$reference==negative_type]
  
  sensitivity<-round(TP/(TP+FN),2)
  specificity<-round(TN/(TN+FP),2)
  F1<-round(2*TP/(2*TP+FP+FN),2)
  # use simulation method
  AUC<-round(mean(sample(pos_scores,10000000,replace=T)>sample(neg_scores,10000000,replace=T)),2)
  
  sensitivities[i]=sensitivity
  specificities[i]=specificity
  F1s[i]=F1
  AUCs[i]=AUC
  
}

# because pred.score is the predicted probability of "Male"
if (positive_type=='female'){
  AUCs<-1-AUCs
}

sensitivities<-c(sensitivities,methods[which.max(sensitivities)])
specificities<-c(specificities,methods[which.max(specificities)])
F1s<-c(F1s,methods[which.max(F1s)])
AUCs<-c(AUCs,methods[which.max(AUCs)])
methods<-c(methods,'highest')


# print(methods)
# print(sensitivities)
# print(specificities)
# print(F1s)
# print(AUCs)


# save output
output<-data.frame(method=methods,sensitivity=sensitivities,specificity=specificities,F1=F1s,AUC=AUCs)
write.csv(output,output_name,row.names=FALSE,quote=FALSE)



