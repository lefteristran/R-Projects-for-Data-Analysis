
install.packages("readxl")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("dplyr")
install.packages("tidyr")
install.packages("gridExtra")
install.packages("purrr")
library("readxl")
library("ggplot2")
library("corrplot")
library("dplyr")
library("tidyr")
library("gridExtra")
library("purrr")
########################.   DATA PREPARATION    ##############################

##set your WD here##

#City names were not displayed correctly with csv file, need to use excel
predictions <- Brazil_census_data_prediction
dfCenso <-  data

df_regions <- data.frame(
  UF = c(12, 16, 13, 15, 11, 14, 17, 27, 29, 23, 21, 25, 22, 26, 24, 28, 53, 52, 51, 50, 32, 31, 33, 35, 41, 42, 43),
  State_letter = c("AC", "AP", "AM", "PA", "RO", "RR", "TO", "AL", "BA", "CE", "MA", "PB", "PI", "PE", "RN", "SE", "DF", "GO", "MT", "MS", "ES", "MG", "RJ", "SP", "PR", "SC", "RS"),
  State_name = c("Acre", "Amap?", "Amazonas", "Par?", "Rond?nia", "Roraima", "Tocantins", "Alagoas", "Bahia", "Cear?", "Maranh?o", "Para?ba", "Piau?", "Pernambuco", "Rio Grande do Norte", "Sergipe", "Distrito Federal", "Goi?s", "Mato Grosso", "Mato Grosso do Sul", "Esp?rito Santo", "Minas Gerais", "Rio de Janeiro", "S?o Paulo", "Paran?", "Santa Catarina", "Rio Grande do Sul"),
  Region_letter = c("NO", "NO", "NO", "NO", "NO", "NO", "NO", "NE", "NE", "NE", "NE", "NE", "NE", "NE", "NE", "NE", "CW", "CW", "CW", "CW", "SE", "SE", "SE", "SE", "SO", "SO", "SO"),
  Region_name = c("North", "North", "North", "North", "North", "North", "North", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Central West", "Central West", "Central West", "Central West", "Southeast", "Southeast", "Southeast", "Southeast", "South", "South", "South")
)

# Merge and exclude X and X.1 as they do not appear in the variable description
merged_censo <- merge(dfCenso, df_regions, by.x = "UF", by.y = "UF")
merged_censo[, c("X.1", "X")] = NULL
str(merged_censo)
#categorical variables as factors
merged_censo$State_letter <- as.factor(merged_censo$State_letter)
merged_censo$State_name <- as.factor(merged_censo$State_name)
merged_censo$Region_letter <- as.factor(merged_censo$Region_letter)
merged_censo$Region_name <- as.factor(merged_censo$Region_name)

library(dplyr)
#"r1404" as first column, because it is the var of interest
merged_censo <- merged_censo %>% select(R1040, everything())

#Add the new columns to 'predictions' that I added to dfcenso
predictions <- cbind(predictions, merged_censo[, c("State_letter", "State_name", "Region_letter", "Region_name")])
str(predictions)
#Match column order 
predictions <- predictions[names(merged_censo)]

#Check
summary(merged_censo)
setdiff(df_regions$UF, unique(merged_censo$UF)) #One State is not in df_Censo: Number 53 (Distrito Federal/ Region Name: Central West)
summary(merged_censo$UF)

###########################.  DESCRIPTIVE ANALYSIS    #########################

# correlation matrix for numerical data (this excludes the columns that were added with the merge)

#not sure if this is what is asked, if someone can check this would be appreciated
str(merged_censo)
numeric_data <- merged_censo[, sapply(merged_censo, is.numeric)]
str(numeric_data)
correlation_matrix <- cor(numeric_data)
corrplot(correlation_matrix, method = "color", type = "upper", tl.cex = 0.7)

library(dplyr)
#to check highly correlated variables see this DF
correlation_pairs <- data.frame(var1 = character(0), var2 = character(0), correlation = numeric(0))

for (i in 1:(ncol(correlation_matrix) - 1)) {
  for (j in (i + 1):ncol(correlation_matrix)) {
    if (correlation_matrix[i, j] > 0.7) { 
      correlation_pairs <- rbind(correlation_pairs, data.frame(var1 = colnames(correlation_matrix)[i], var2 = colnames(correlation_matrix)[j], correlation = correlation_matrix[i, j]))
    }
  }
}

#box plots for R1040 by State Name
ggplot(merged_censo, aes(x =  , y = R1040)) +
  geom_boxplot() +
  labs(title = "Box Plots of R1040 by State Name", x = "State Name", y = "R1040") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#box plots for R1040 by Region Name
ggplot(merged_censo, aes(x = Region_name, y = R1040)) +
  geom_boxplot() +
  labs(title = "Box Plots of R1040 by Region Name", x = "Region Name", y = "R1040")


#Other possible plots
ggplot(merged_censo, aes(x = Region_name, fill = Region_name)) +
  geom_bar() +
  labs(title = "Count of Observations by Region", x = "Region Name", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(merged_censo, aes(x = State_name, fill = State_name)) +
  geom_bar() +
  labs(title = "Count of Observations by State Name", x = "State Name", y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(size = 8), 
    legend.key.size = unit(0.2, "cm")  
  )



# 2.1 Generate your principal components using the appropriate covariates from your dataset.
library(dplyr)
str(numeric_data)
R1040<-numeric_data[1]
numeric_data<-scale(numeric_data)
pca_numeric<-princomp(numeric_data, cor = T, scores = TRUE)
summary(pca_numeric)
screeplot(pca_numeric)
#plot(pca_numeric$sdev^2,type="b",pch=18, cex=1, lwd=1, xlab="", ylab = "eigenvalue")
interpretation = cbind(cor(numeric_data, pca_numeric$scores)[,1:5], rowSums(cor(numeric_data, pca_numeric$scores)[,1:5]^2))
colnames(interpretation) = c("Comp. 1","Comp. 2","Comp. 3", "Comp.4", "Comp.5","Variance explained")
interpretation

#2.2 Test for the significant components using the permutation test.
#I use perm_test1 for testing first the results
perm_test1<-numeric_data

#test

library(dplyr)
nTests=1000
k=0
eigs.Xperm = matrix(NA,nTests,ncol(perm_test1))
xperm = as.data.frame(matrix(NA,nrow(perm_test1), ncol(perm_test1)))
xperm[,1]<-perm_test1[,1]
for (i in 1:nTests) {
  for (j in 1:ncol(perm_test1)) {
  set.seed(k)
  ind <- sample(1:nrow(perm_test1),replace=FALSE)
  xperm[,j]<-perm_test1[ind,j]
  k=k+1
  }
  res.xperm<-princomp(xperm, cor=T, scores=TRUE)
  eigs.Xperm[i,] <- res.xperm$sdev^2
}
#hist(eigs.Xperm, main = "Permutation Test")

#2.3 Decide on a final number1 of components to proceed with your analysis. Explain your decision
# and provide any relevant graphs.

plot(pca_numeric$sdev^2,type="b",pch=18, cex=1, lwd=1, xlab="", ylab = "eigenvalue")
lines(apply(eigs.Xperm,2,quantile,c(0.025)), type="b", col="red", xlab="", ylab="eigenvalue")
lines(apply(eigs.Xperm,2,quantile,c(0.925)), type="b", col="blue", xlab="", ylab="eigenvalue")


#perm_test1 - Anastasias Permutation Function

perm_range<-permtestPCA(perm_test1)

# 2.4 Construct a biplot and interpret the results. Take a look at the loadings as well. Can you
# think of an interpretation for the principal components?

op <- par(mfrow = c(1, 2))
biplot(pca_numeric, pc.biplot = TRUE, scale = 1, choices = 1:2, col = c("blue", "red"),
       asp = 1, cex = c(0.5, 1), main = "Biplot components 1 and 2")
biplot(pca_numeric, pc.biplot = TRUE, scale = 1, choices = 2:3, col = c("blue", "red"), 
       asp = 1, cex = c(0.5, 1), main = "Biplot components 3 and 4")
par(op)

print(summary(pca_numeric, loadings=TRUE, cutoff=0.2), digits=2)


#2.5 You now have to decide which variables are best explained by the principal components:

install.packages("boot")
library(boot)

#define Bootstrap function
my_boot_pca<-function(x, ind){
  res <- princomp(x[ind, ], cor=TRUE)
 return(res$sdev^2)
}

#Run Bootstrap
fit.boot<-boot(data = numeric_data, statistic = my_boot_pca, R=1000)

#Store the bootstrap stat - eigenvalues
eigs.boot<-fit.boot$t
head(eigs.boot)
pca_numeric$sdev^2

#plot

par(mfrow = c(1,1), mar = c(5,4,4,1) + 0.1)

#hist of 1st EigVal
l<-1
hist(eigs.boot[,1], xlab = "Eigenvalue 1", las=1, col = "blue",
     main = "Bootstrap Confidence Interval", breaks = 20,
     border = "white")
     
perc.alpha<-quantile(eigs.boot[,l], c(0.025, 1- 0.025))
perc.alpha     
abline(v=perc.alpha, col="red", lwd=2)   
abline(v=pca_numeric$sdev[1]^2, col = "green", lwd=2)
summary(pca_numeric)


# variance explained by the first principal component.

head(eigs.boot)
var.expl<-eigs.boot[,1]/rowSums(eigs.boot)

hist(var.expl, xlab = "Variance Explained", las=1, col = "blue",
     main = "Bootstrap Confidence Interval", breaks = 20,
     border = "white")

perc.alpha<-quantile(var.expl, c(0.025, 1- 0.025))
perc.alpha     
abline(v=perc.alpha, col="red", lwd=2)   
abline(v= pca_numeric$sdev[1]^2/sum(pca_numeric$sdev^2), col = "green", lwd=4)


par(mar = c(5,4,4,1) + 0.1)
boxplot(eigs.boot, las=1, xlab="Dimension", ylab= "Eigenvalue", col = "beige")



##################################################################


#3.1 - split date into training-test data

set.seed(123)  
train_index <- sample(1:nrow(numeric_data), nrow(numeric_data)*0.8)

train_data <- numeric_data[train_index, ]
test_data <- numeric_data[-train_index, ]

#3.2 Fit the benchmark linear regression using the original variables on training set.
#categorical var are not included ofc

set.seed(1000)
model_train <- lm(R1040 ~. , data = train_data)

summary(model_train)


#3.3 Fit the principal components regression in the training set.

chooseCRANmirror(graphics = FALSE, ind = 10)
if (!require("pacman")) install.packages("pacman")
# This used the function p_load from the pacman #
# package to load the other packages

pacman::p_load(pls)
# Use model to make predictions on a test set If
# cross validating, then validation='CV', else
# validation=NULL

#3.3 pcr ->train data /  Fit the principal components regression in the training set
pcr_model <- pcr(data = train_data, R1040 ~ ., validation = "CV",
                 scale = TRUE)
summary(pcr_model)


# Number of principal components previously
# chosen = n /  We chose 5


pcr_model_pred <- predict(pcr_model, train_data, ncomp = 4)
pcr_model

#3.4 Compute predictions using the models from 3.2 and 3.3 and the test set. Compare predictive
#performance using an appropriate metric. What is your conclusion?

predict_lin <- predict(model_train, test_data)
predict_lin

install.packages("Metrics")
library(Metrics)
mse(actual = test_data$R1040, predicted = as.numeric(predict_lin))

pcr_pred <- predict(pcr_model, test_data, ncomp = 4)
mse(actual = test_data$R1040, predicted = as.numeric(pcr_pred))

###
# 3.5 
#new name for testing 
predictions2<-predictions


str(predictions2)
#abstract characters-factors
numeric_sub<- subset(predictions2, select = -c(pesoRUR,MULHERTOT,HOMEMTOT,pesourb,
                                               NOMEMUN,State_letter,State_name,Region_name,Region_letter))
#log
numeric_sub <- log(numeric_sub + 1)


#check 

str(numeric_sub)
summary(numeric_sub) #there is no NA value


#split the data

train_index3.5 <- sample(1:nrow(numeric_sub), nrow(numeric_sub)*0.8)

train_data3.5 <- numeric_data[train_index3.5, ]
test_data3.5 <- numeric_data[-train_index3.5, ]


# linear regression on 3.5/predictions
set.seed(1000)
model_predictions3.5 <- lm(R1040 ~., data = train_data3.5)
summary(model_predictions3.5) # a lot of NA values


#pcr on predictions dataset
pcr_model3.5 <- pcr(data = train_data3.5, R1040 ~ ., validation = "CV",
                 scale = TRUE)

summary(pcr_model3.5)

pcr_model_pred3.5 <- predict(pcr_model3.5, train_data3.5, ncomp = 4)

pcr_model_pred3.5

#Compare predictions Linear-pcr on "predictions" dataset

predict_lin3.5 <- predict(model_predictions3.5, test_data3.5)
predict_lin3.5

mse(actual = test_data3.5$R1040, predicted = as.numeric(predict_lin3.5))
exp(mse(actual = test_data3.5$R1040, predicted = as.numeric(predict_lin3.5))) - 1

pcr_pred3.5 <- predict(pcr_model3.5, test_data3.5, ncomp = 4)
mse(actual = test_data3.5$R1040, predicted = as.numeric(pcr_pred3.5))

exp(mse(actual = test_data3.5$R1040, predicted = as.numeric(pcr_pred3.5))) - 1 
