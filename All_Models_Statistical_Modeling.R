

library(tidyverse)
library(caret)
library(pls)
library(ggplot2)
library(devtools)
library(ggfortify)



#### ------ REGRESSION MODELING [Dependent Variable as numeric continuous] ----- ######
#### Load a dataframe for regression modeling of physical activity scores as a function of tens of kinematic features ######
PASE_study_Kinematics_PCR = read.csv("PASE_study_Kinematics_PCR.csv", sep = ";", dec = ",", na.strings=c("", " ","NA"))


##### Split data into train (80%) and test (20%) sets ####
set.seed(1)
training.samples <- PASE_study_Kinematics_PCR$PASE %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- PASE_study_Kinematics_PCR[training.samples, ]
test.data <- PASE_study_Kinematics_PCR[-training.samples, ]



#  -------- PRINCIPAL COMPONENT REGRESSION ------- #
##### Build a Principal Component Regression model on the training set with 10-fold cross validation #####
set.seed(1)
model <- train(
  PASE~., data = train.data, method = "pcr",
  scale = TRUE,
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)


###### Plot the model's error as measured by the Root-mean-square deviation 
#####   as a function of the number of principal components use for regression ####
plot(model)

#### Pick the best number of principal components use for regression (i.e. the lowest Root-mean-square deviation error) #####
model$bestTune
summary(model$finalModel)

########## Run the test data on the model and fetch the predicted values
predictions <- model %>% predict(test.data)


###########  Extract the model performance metrics Root-mean-square deviation error and R(2)  ######
data.frame(
  RMSE = caret::RMSE(predictions, test.data$PASE),
  Rsquare = caret::R2(predictions, test.data$PASE)
)




#  -------- PARTIAL LEAST SQUARES  ------- #
##### Build a PLS model on (another random) training set with 10-fold cross validation #####
set.seed(123)
model <- train(
  PASE~., data = train.data, method = "pls",
  scale = TRUE,
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)



###### Plot the model's error as measured by the Root-mean-square deviation 
#####   as a function of the number of principal components use for regression ####
plot(model)



#### Pick the best number of principal components use for regression (i.e. the lowest Root-mean-square deviation error) #####
model$bestTune
summary(model$finalModel)

########## Run the test data on the model and fetch the predicted values
predictions <- model %>% predict(test.data)


###########  Extract the model performance metrics Root-mean-square deviation error and R(2)  ######
data.frame(
  RMSE = caret::RMSE(predictions, test.data$PASE),
  Rsquare = caret::R2(predictions, test.data$PASE)
)





#### ------ CLASSIFICATION MODELING [Dependent Variable as factor]----- ######
#### ------ Can you blindly discriminate the extreme (i.e. low vs high activity) groups based on their gait performance alone ? ---- ###

########## Principal Component Analysis  ########
set.seed(1)

##### Load the dataset #####
PASE_PCA_short = read.csv("PASE_PCA_short.csv", sep = ";", dec = ",", na.strings=c("", " ","NA"))

##### Set output as a factor #####
PASE_PCA_short$PA <- as.factor(PASE_PCA_short$PA)


##### Extract only the numeric variables onto a separate DF 
############ [not required, you may chose to selected only the columns of interest directly when performing PCA, e.g. data[:-2] #####
df <- PASE_PCA_short[c(1:54)]

############ Vizualize the PCA plot pn PC1 vs PC2 #####
autoplot(prcomp(df), data= PASE_PCA_short, colour = 'PA', shape= 'PA' ) +
  geom_point(aes(size =40,  colour = PA, shape= PA), show.legend = FALSE) +
  theme_minimal()


############ Perform PCA #####
PASEmodel_short <- preProcess(PASE_PCA_short[,-45], method = c("pca"))
PC <- predict(PASEmodel_short, PASE_PCA_short[,-45])

############ Check the cumulative variance #####
str(PC)

############ Extract the loadings of each variable on each principal component #####
PASEmodel_short$rotation


############ Add the classification (e.g. Low | High) onto the dataset containing each patient score on each principal component #####
tr <- cbind(PC, label=PASE_PCA_short[,45])
tr$label<-as.factor(tr$label)



######### TREE-BASED Classification #########
############ Split data over 80% vs 20% train vs test sets #####
vdatapart <- createDataPartition(tr$label, p = 0.8, list = F)

traindata <- tr[vdatapart,]
validdata <- tr[-vdatapart,]


############ Create a tree-based classification o the train set using the C5.0 function ########
model <- C5.0(label ~ ., data = traindata)

############ Check the model ########
summary(model)

############ Predict the output on the test/validation dataset ########
result <- predict(model, validdata[,-16])
result


############ Calculate its accuracy ########
(accuracy<-sum(result == validdata$label)/nrow(validdata))
result == validdata$label


############ Plot the tree ########
plot(model, colours(distinct = T))


############ Note: you may follow with with cross validation, ideally with a leave-one out approach ########
##### Example with 39 patients k = 39###

#set.seed(1)
#form <- "label ~ ."
#folds <- split(tr, cut(sample(1:nrow(tr)),39))
#errs.c50 <- rep(NA, length(folds))
#for (i in 1:length(folds)) {
#  test <- ldply(folds[i], data.frame)
#  train <- ldply(folds[-i], data.frame)
#  tmp.model <- C5.0(as.formula(form), train)
#  tmp.predict <- predict(tmp.model, newdata=test)
#  conf.mat <- table(test$label, tmp.predict)
#  errs.c50[i] <- 1 - sum(diag(conf.mat))/sum(conf.mat)
#}

# print(sprintf("average error using a "leave one out approach" cross validation on the C5.0 decision tree algorithm: %.3f percent", 100*mean(errs.c50)))




#####  Using a Linear Discriminant Analysis Model #######
########### Principal Component Analysis followed by Linear Discriminant Analysis ###########
library(MASS)
library(ROCR)
library(ROCR)
library("factoextra")


###### Import the dataset ######
PASE_PCA_short = read.csv("PASE_PCA_short.csv", sep = ";", dec = ",", na.strings=c("", " ","NA"))

###### Transfor the output into a factor ######
PASE_PCA_short$PA <- as.factor(PASE_PCA_short$PA)


###### Calculate the PC scores ######
PASE_PCA.pr_short <- prcomp(PASE_PCA_short[c(1:44)], center = TRUE, scale = TRUE)
summary(PASE_PCA.pr_short)

###### Extract the loadings of each variable onto each PC ######
PASE_PCA.pr_short$rotation[,1:10]

###### Extract the varimax rotated loadings of each variable onto each PC ######
my.var = varimax(PASE_PCA.pr_short$rotation)
myvarshort <-my.var$loadings[,1:10]
myvarshort <- as.data.frame(myvarshort)


###### NOTE ####
###### You may save your varimax into a dataframe and subsequentl plot it as follows ####
Varimaxtable <- read.csv("Varimax.csv", sep = ";", dec = ".", header = T)
Varimaxtable[Varimaxtable<0.4 & Varimaxtable>-0.4] <- ""

write.csv(Varimaxtable,"Varimaxtable_Trimmed.csv", row.names = FALSE)
Varimaxtable_Trimmed <- read.csv("Varimaxtable_Trimmed copy.csv", sep = ";", dec = ".", header = T)
Varimaxtable_Trimmed$Component <- factor(Varimaxtable_Trimmed$Component, levels = c("RC1", "RC2", "RC3", "RC4", "RC5", "RC6", "RC7", "RC8", "RC9", "RC10"))

ggplot(data = Varimaxtable_Trimmed,aes(x=Component, y=Variable, fill=Value)) + 
  geom_tile() + 
  geom_text(aes(label=Value))+
  scale_fill_continuous(low = '#018571', high = '#FC717F', na.value = 'white')+
  theme_minimal()
##### addapt accordingly ######




###### Check where to stop based on the Guttman-Kaiser criterion ######
screeplot(PASE_PCA.pr_short, type = "l", npcs = 12, main = "Screeplot of the first 12 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)


###### Check the cumulative explained variance at the set number of PCs (e.g. in this case 10 PCs -> CEV of 0.87) ######
cumpro2 <- cumsum(PASE_PCA.pr_short$sdev^2 / sum(PASE_PCA.pr_short$sdev^2))
plot(cumpro2[0:12], xlab = "PC #", ylab = "Cumulative proportion of explained variance", main = "Cumulative variance plot")
abline(v = 10, col="blue", lty=5)
abline(h = 0.87, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC10"),
       col=c("blue"), lty=5, cex=0.6)

###### Check your PC1 vs PC2 plot ######
plot(PASE_PCA.pr_short$x[,1],PASE_PCA.pr_short$x[,2], xlab="PC1 (28.85%)", ylab = "PC2 (16.32%)", main = "PC1 / PC2 - plot")

###### Make a more colorful plot ######
library("factoextra")
fviz_pca_ind(PASE_PCA.pr_short, geom.ind = "point", pointshape = 21, 
             pointsize = 4, 
             fill.ind = PASE_PCA_short$PA, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = T,
             legend.title = "Physical Activity Group") +
  ggtitle("2D PCA-plot based on the 44 kinematic features") +
  theme(plot.title = element_text(hjust = 0.5))


###### Plotting alternative PCs in yet another way (using ggplot2) ######
### a sample dataset, label your columns accordingly (!) ###
ggplot(data = PCs,aes(x = PC4, y = PC10, fill = Group))+
  geom_point(shape = 21,size=8, position = position_jitterdodge(), color="black",alpha=0.8, show.legend = T)+
  theme_minimal()+
  scale_fill_manual(values=c("#018571", "#FC717F"))



####### Create a data set using the 10 PCs (instead of 44 kinematic variable) to train a linear discriminant analysis model #####
PASE_PCA.pcst_short <- PASE_PCA.pr_short$x[,1:10]
PASE_PCA.pcst_short <- cbind(PASE_PCA.pcst_short, as.factor(PASE_PCA_short$PA))
colnames(PASE_PCA.pcst_short)[11] <- "PA"

####### Split data e.g. 70% vs 30% #####
set.seed(4)
smp_size <- floor(0.70 * nrow(PASE_PCA.pcst_short))
train_ind <- sample(nrow(PASE_PCA.pcst_short), size = smp_size)
train.df <- as.data.frame(PASE_PCA.pcst_short[train_ind, ])
test.df <- as.data.frame(PASE_PCA.pcst_short[-train_ind, ], )

####### Create a linear discriminant analysis model #####
PASE_PCA.lda <- lda(PA ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = train.df)

####### Apply it on the test data #####
PASE_PCA.lda.predict <- predict(PASE_PCA.lda, newdata = test.df)
PASE_PCA.lda
PASE_PCA.lda.predict

####### Extract the confusion matrix #####
table(PASE_PCA.lda.predict$class, test.df[,11])

#OR

####
example <- confusionMatrix(data=factor(PASE_PCA.lda.predict$class), reference = factor(test.df[,11]))




############## Tree based classification ###############
#vdata <- read.csv("PASE_PCA_short.csv", sep = ";", dec = ",", header = T,)
#
#df <- vdata[c(1:44)]
#autoplot(prcomp(df), data= vdata, colour = 'PA', shape= 'PA' ) +
#  geom_point(aes(size =40,  colour = PA, shape= PA), show.legend = FALSE) +
#  theme_minimal()
#
#set.seed(1)
#levodopamodel <- preProcess(vdata[,-45], method = c("pca"))
#PC <- predict(levodopamodel, vdata[,-45])
#str(PC)
#tr <- cbind(PC, label=vdata[,45])
#tr$label<-as.factor(tr$label)
#vdatapart <- createDataPartition(tr$label, p = 0.7, list = F)
#traindata <- tr[vdatapart,]
#validdata <- tr[-vdatapart,]
#model <- C5.0(label ~ ., data = traindata)
#summary(model)
#result <- predict(model, validdata[,-16])
#result
#(accuracy<-sum(result == validdata$label)/nrow(validdata))
#result == validdata$label
#plot(model, colours(distinct = T))




###### Should you which to compare individual PCs between groups ######
### synthetic example comparing PC10 of patients 1:11 vs 12:22 ###
res <- t.test(PC[1:11,10], PC[12:22,10], paired = TRUE)
res


#### Vizualize accordingly ######
ggplot(data = PCs,aes(x = Group, y = PCs[,10], fill = Group))+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.5, show.legend = F)+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color="black",show.legend = F) +
  geom_point(shape = 21,size=9, position = position_jitterdodge(), color="black",alpha=0.9, show.legend = F)+
  theme_minimal()+
  scale_fill_manual(values=c("#018571", "#FC717F"))+
  xlab ( NULL)+
  ylab(  c("PC10"))





####### Sample scatter plot with loess for vizualizaing clinical variables ######
ggplot(Longitudinal, aes(x = PASE, y = Longitudinal$Rigidity, color = Rigidity)) + 
  geom_smooth(colour="bisque2", fill="aquamarine3",  method = "loess", alpha = .1) + 
  geom_point(size = 10, alpha = 0.8, show.legend = FALSE) + 
  scale_shape(guide=FALSE) +
  theme(legend.position = c(.9, .75),
        legend.spacing.y = unit(1, "mm"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  labs(x = 'PASE Score',
       y = 'Rigidity Score')+
  scale_color_gradient(low = "bisque2", high = "aquamarine3") +
  theme_minimal(base_size = 20)








####### Mediation Analysis ##########
dfMediation = read.csv("dfMediation.csv", sep = ";", dec = ",", na.strings=c("", " ","NA"))

fit.totaleffect=lm(PDQ.8~PASE,dfMediation)
summary(fit.totaleffect)

fit.mediator=lm(UPDRS.III~PASE,dfMediation)
summary(fit.mediator)

fit.dv=lm(PDQ.8~PASE+UPDRS.III,dfMediation)
summary(fit.dv)

#install.packages("mediation")
library(mediation)
dfMediation <- na.omit(dfMediation)
set.seed(1)
results = mediate(fit.mediator, fit.dv, treat='PASE', mediator='UPDRS.III', boot=T)
summary(results)



