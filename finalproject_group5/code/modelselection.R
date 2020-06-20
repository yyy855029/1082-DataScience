library(ggplot2)
library(dplyr)
library(rpart)
library(randomForest)
library(argparser)
library(party)
library(xgboost)
library(pROC)
library(DiagrammeR)
library(rpart)
library(plyr)
library(dplyr)
library(gridExtra)
library(lattice)
library(caret)
library(e1071)

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  


p <- arg_parser("target, input and output")
p <- add_argument(p,"--fold", help = "assign how many fold k to do the cross validation")
p <- add_argument(p, "--pokemon", help="train file")
p <- add_argument(p, "--combat", help="train file")
p <- add_argument(p, "--report", help="report file", default="performance.csv")
p <- add_argument(p, "--predict", help="predict file", default="predict.csv")
p <- add_argument(p, "--methods", help="use which model")
argv <- parse_args(p, commandArgs(trailingOnly = TRUE))
k_fold <-  argv$fold
k_fold <- as.numeric(k_fold)
report <- argv$report
prediction <- argv$predict
methods <- argv$methods
performance <- data.frame(list(set="", training="", validation="",testing="",method = ""), stringsAsFactors=FALSE,row.names = 1)



#load data
#pokemon <-  read.csv("pokemon.csv",header = TRUE,sep = ",", stringsAsFactors = F,row.names = NULL)
pokemon <-  read.csv(argv$pokemon,header = TRUE,sep = ",", stringsAsFactors = F,row.names = NULL)
#combat <- read.csv("combats.csv",header = TRUE, sep=",", stringsAsFactors = F,row.names = NULL)
combat <- read.csv(argv$combat,header = TRUE, sep=",", stringsAsFactors = F,row.names = NULL)

names(pokemon)[1] <- "number"

#join to create train and test data
#library(dplyr)
firstpokemon <-  data.frame(combat$First_pokemon)
secondpokemon <-  data.frame(combat$Second_pokemon)
colnames(firstpokemon) <-  "number"
colnames(secondpokemon) <- "number"

#這段是勝率，不是預測誰贏
#merge data together
#library(plyr)
merge1 <- join(firstpokemon,pokemon,by="number")
merge2 <- join(secondpokemon,pokemon,by="number")
newdata <- cbind(merge1,merge2,combat$Winner)
colnames(newdata)[colnames(newdata)=="combat$Winner"] = "Winner" 
colnames(newdata) <- c("number_a","name_a","type1_a","type2_a","hp_a","attack_a","defense_a","spatk_a","spdef_a","speed_a","generation_a","legendary_a","number_b","name_b","type1_b","type2_b","hp_b","attack_b","defense_b","spatk_b","spdef_b","speed_b","generation_b","legendary_b","Winner")

newdata2 <- newdata
#data transition
newdata2$type1_a <- as.numeric(factor(newdata2$type1_a))
newdata2$type2_a <- as.numeric(factor(newdata2$type2_a))
newdata2$legendary_a <- as.numeric(factor(newdata2$legendary_a))

newdata2$type1_b <- as.numeric(factor(newdata2$type1_b))
newdata2$type2_b <- as.numeric(factor(newdata2$type2_b))
newdata2$legendary_b <- as.numeric(factor(newdata2$legendary_b))

#new columns
binarywinner = rep(1, 50000)
binarywinner[newdata2$Winner == newdata2$number_b] = 0
newdata2$binary_winner <- binarywinner
newdata3 <- newdata2[,-c(1,2,13,14,25)]


# to draw plot
temp <- 0
score <- function(word,tb){
  if (word>temp){
    temp <- word
    write.csv("aaa.csv",file=tb)
  }
}
#cross validate k=10
k=k_fold
#k = 10
folds = cut(seq(1,nrow(newdata)),breaks = k, labels= FALSE)
for (i in 1:k){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  validIndexes <- which(folds==(k-i+1),arr.ind=TRUE)
  d_test <- newdata3[testIndexes, ]
  d_valid <- newdata3[validIndexes,]
  d_train <- newdata3[-c(testIndexes,validIndexes), ]
  
  #model training
  #y_train <-  d_train$binary_winner
  y_train <- d_train[,21]
  x_train <- d_train[,-21]
  y_valid <-  d_valid[,21]
  x_valid <- d_valid[,-21]
  y_test <- d_test[,21]
  x_test <- d_test[,-21]
  fmla = paste("binary_winner",paste(names(x_train),collapse = "+"),sep="~")
  if (methods == "logistic"){
    #train
    #model1 <- glm(fmla,data=d_train,family="binomial")
    model1 <- readRDS("./logistic.rds")
    result_train <- predict(model1,x_train,type="response")
    rt1_train <- table(result_train > .5, y_train)
    acc_train <- (rt1_train[1]+rt1_train[4])/sum(rt1_train)
    #test
    result_valid <- predict(model1,x_valid,type="response")
    rt1_valid <- table(result_valid > .5, d_valid$binary_winner)
    acc_valid <- (rt1_valid[1]+rt1_valid[4])/sum(rt1_valid)
    
    result_test <- predict(model1,x_test,type="response")
    rt1_test <- table(result_test > .5, d_test$binary_winner)
    acc_test <- (rt1_test[1]+rt1_test[4])/sum(rt1_test)
    
    sen <- (rt1_test[4])/(rt1_test[2]+rt1_test[4])
    spe <- (rt1_test[1])/(rt1_test[1]+rt1_test[3])
    pre <- (rt1_test[4])/(rt1_test[3]+rt1_test[4])
    F1 <- 2*pre*sen/(pre+sen)
    result_test <- c(result_test)
    result_test[which(result_test>=0.5)] =1
    result_test[which(result_test<0.5)] =0
    result_test <- as.numeric(result_test)
    cm <- confusionMatrix(as.factor(result_test),reference = as.factor(y_test), mode = "prec_recall", positive="1")
    pdf("matrix_log.pdf")
    draw_confusion_matrix(cm)
    dev.off()
    #score(acc_test,rt1_test)
    #myroc = roc(response= y_valid, predictor = x_valid)
    #pdf("roc_curve.pdf")
    #plot1 <- plot.roc(myroc)
    #text <- paste("AUC: ", as.character(round(auc(myroc),2)))
    #mtext(text)
    #dev.off()
  }
  else if(methods =="randomforest"){
    
    #model2 <- randomForest(formula = binary_winner~., data = d_train, ntree=500)

    #rf_tree <- randomForest(binary_winner ~. , data = d_train, type = 'classification', ntree = 500 )
    model2<- readRDS("./rf_tree.rds")
    pred <- predict(model2, x_train)
    rt2_train <- table(pred >0.5, y_train)
    acc_train <- (rt2_train[1]+rt2_train[4])/sum(rt2_train)
    pdf("varimportance.pdf")
    plot2 <- randomForest::varImpPlot(model2)
    dev.off()
    pred <- predict(model2, x_valid)
    rt2_valid <- table(pred >0.5, y_valid)
    acc_valid <- (rt2_valid[1]+rt2_valid[4])/sum(rt2_valid)
    
    
    pred <- predict(model2, x_test)
    rt2_test <- table(pred >0.5, y_test)
    acc_test <- (rt2_test[1]+rt2_test[4])/sum(rt2_test)
    
    sen <- (rt2_test[4])/(rt2_test[2]+rt2_test[4])
    spe <- (rt2_test[1])/(rt2_test[1]+rt2_test[3])
    pre <- (rt2_test[4])/(rt2_test[3]+rt2_test[4])
    F1 <- 2*pre*sen/(pre+sen)
    
    result_test <- c(pred)
    result_test[which(result_test>=0.5)] =1
    result_test[which(result_test<0.5)] =0
    result_test <- as.numeric(result_test)
    cm <- confusionMatrix(as.factor(result_test),reference = as.factor(y_test), mode = "prec_recall", positive="1")
    pdf("matrix_rf.pdf")
    draw_confusion_matrix(cm)
    dev.off()
    #score(acc_test,rt2_test)
    #tree_func(model,tree_num = 500)
    #rownames(d_train) = make.names(rownames(d_train), unique=TRUE)
    #plot4 <- reprtree:::plot.getTree(model)
    #pt <- party:::prettytree(bst@ensemble[[1]], names(bst@data@get("input"))) 
    #nt <- new("BinaryTree") 
    #nt@tree <- pt 
    #nt@data <- bst@data 
    #nt@responses <-bst@responses 
    #plot(nt,type="simple")
  }
  else{
    dtrain = xgb.DMatrix(as.matrix(sapply(d_train, as.numeric)), label=d_train$binary_winner)
    dvalid = xgb.DMatrix(as.matrix(sapply(d_valid, as.numeric)), label=d_valid$binary_winner)
    #model3 <- xgboost(dtrain,
                     #nrounds = 10, objective = "binary:logistic")
    model3 <- readRDS("./xgboost.rds")
    preds = predict(model3, as.matrix(d_train))
    rt3_train <- table(preds > .5, d_train$binary_winner)
    acc_train <- (rt3_train[1]+rt3_train[4])/sum(rt3_train)
    
    preds = predict(model3, as.matrix(d_valid))
    rt3_valid <- table(preds > .5, d_valid$binary_winner)
    acc_valid <- (rt3_valid[1]+rt3_valid[4])/sum(rt3_valid)
    
    preds = predict(model3, as.matrix(d_test))
    rt3_test <- table(preds > .5, d_test$binary_winner)
    acc_test <- (rt3_test[1]+rt3_test[4])/sum(rt3_test)
    
    sen <- (rt3_test[4])/(rt3_test[2]+rt3_test[4])
    spe <- (rt3_test[1])/(rt3_test[1]+rt3_test[3])
    pre <- (rt3_test[4])/(rt3_test[3]+rt3_test[4])
    F1 <- 2*pre*sen/(pre+sen)
    
    result_test <- c(preds)
    result_test[which(result_test>=0.5)] =1
    result_test[which(result_test<0.5)] =0
    result_test <- as.numeric(result_test)
    cm <- confusionMatrix(as.factor(result_test),reference = as.factor(y_test), mode = "prec_recall", positive="1")
    pdf("matrix_xg.pdf")
    draw_confusion_matrix(cm)
    dev.off()
    #score(acc_test,rt3_test)
    #xgb.plot.tree(feature_names=names(dtrain),render=FALSE,model=model3, trees=2)
    #export_graph(gr, 'tree.pdf', width=1500, height=1900)
  }
  #------------- for logistic plot
  #roc plot
  #這個function 可以畫圖
  #------------- for random forest plot
  #pdf("random_forest.pdf")
  #plot3 <- plot(randomForest(x=x_valid, y=y_valid, keep.forest=FALSE, ntree=500), log="y")
  #dev.off()
  #performance
  #(rtab <- table(resultframe))
  fold_num <- paste("fold",i,sep="")
  temp_out <- c(fold_num, round(acc_train,3),round(acc_valid,3),round(acc_test,3),methods)
  performance[i,] <- temp_out
}
#performance combine
average <-  c("ave.",as.character(round(mean(as.numeric(performance$training)),3)),as.character(round(mean(as.numeric(performance$validation)),3)),as.character(round(mean(as.numeric(performance$testing)),3)),methods)
performance <- rbind(performance,average)
write.csv(performance, file=report, fileEncoding = "UTF-8", row.names=F,quote = FALSE)




#save model

#saveRDS(modell, "./logistic.rds")
#saveRDS(rf_tree,"./rf_tree.rds")
#saveRDS(model3, "./xgboost.rds")



#load model
#model1 <- readRDS("./logistic.rds")
#model2<- readRDS("./rf_tree.rds")
#model3 <- readRDS("./xgboost.rds")

#plotting function
