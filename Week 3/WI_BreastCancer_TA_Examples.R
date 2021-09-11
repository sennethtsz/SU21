suppressWarnings(library(tidyverse))
suppressWarnings(library(ROCR))
suppressWarnings(library(pROC))
suppressWarnings(library(ggplot2))


names <- c('id_number', 'Cl.thickness', 'Cell.size', 'Cell.shape', 'Marg.adhesion',
           'Epith.c.size', 'Bare.nuclei', 'Bl.cromatin', 'Normal.nucleoli', 'Mitoses', 'Class')
breast_cancer <- read.table('breast-cancer-wisconsin.data', sep = ',', col.names = names)
str(breast_cancer)

###2 for benign, 4 for malignant

breast_cancer$Bare.nuclei <-as.numeric(breast_cancer$Bare.nuclei)

#code benign as 0 and malignant as 1
breast_cancer$Class <- ifelse(breast_cancer$Class==2,0,1)
str(breast_cancer)
drop <- c("id_number")
df = breast_cancer[,!(names(breast_cancer) %in% drop)]
#Remove NA for this example
df <-na.omit(df) 

#inspect label
head(df)
ggplot(df,aes(Class)) + geom_bar()

#create model
log.model <- glm(formula=Class ~ . , family = binomial,data = df)
summary(log.model)


#predict
fit.prob <- predict(log.model,newdata=df,type='response')
pred_y <- as.numeric(ifelse(fit.prob > 0.5,1,0))

#Create CF matrix with labels
act<-df$Class
compare <- as.data.frame(cbind(act, pred_y))
as.data.frame(table(compare))
cf_matrix<-table(compare$pred_y, compare$act)
tbl <- matrix(data=cf_matrix, nrow=2, ncol=2, 
              dimnames=list(c("neg", "pos"), c("neg", "pos")))

names(dimnames(tbl)) <- c("predicted", "act")
tbl

#Check Metrics
Acc<-(tbl[1,1]+tbl[2,2])/sum(tbl)
Acc

Precision <-tbl[2,2]/(tbl[2,2]+tbl[2,1])
Precision

Recall <-tbl[2,2]/(tbl[2,2]+tbl[1,2])
Recall

Specificity <-tbl[1,1]/(tbl[1,1]+tbl[2,1])
Specificity


#Create ROC-AUC
ROC_AUC <-as.data.frame(cbind(compare$act, fit.prob))
pred <- prediction(ROC_AUC$fit.prob, ROC_AUC$V1)
class(pred)
perf <- performance(pred, "tpr", "fpr") 
plot(perf, colorize=T)

auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values