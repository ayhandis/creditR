# creditR
## A Credit Risk Scoring and Validation Package

This package provides a number of R functions useful in applying the methods related to credit risk scoring. The package aims to facilitate the applications of the methods of variable analysis, variable selection, model development, model calibration, rating scale and model validation. Through the functions defined, these methodologies can be applied quickly on all modeling data or a specific variable. The package was issued for the use of credit risk professionals. Basic level knowledge about credit risk scoring methodologies is required for use of the package.

### Prerequisites
In order to install the creditR package, devtools package must also be installed. The devtools package can be installed by running the following code.
```
install.packages("devtools", dependencies = TRUE) 
```

### Getting Started
creditR package can be installed using the install_github function found in the devtools package.
```
library(devtools)
devtools::install_github("ayhandis/creditR)
library(creditR)
```

### A List of Functions
The functions available under the package are listed below.
```
ls("package:creditR")

 [1] "Adjusted.Binomial.test"              "Adjusted.Herfindahl.Hirschman.Index" "Anchor.point"                       
 [4] "bayesian.calibration"                "Binomial.test"                       "chisquare.test"                     
 [7] "correlation.cluster"                 "Gini.univariate"                     "Gini.univariate.data"               
[10] "Gini_elimination"                    "Herfindahl.Hirschman.Index"          "IV.calc"                            
[13] "IV.calc.data"                        "IV_elimination"                      "k.fold.cross.validation.glm"        
[16] "Kolmogorov.Smirnov"                  "master.scale"                        "missing_elimination"                
[19] "missing_ratio"                       "na_checker"                          "na_filler_contvar"                  
[22] "PSI.calc"                            "PSI.calc.data"                       "regression.calibration"             
[25] "scaled.score"                        "SSI.calc"                            "SSI.calc.data"                      
[28] "train_test_balanced_split"           "train_test_split"                    "variable.clustering"                
[31] "variable.clustering.gini"            "vif.calc"                            "woe.get.clear.data"                 
[34] "woe.glm.feature.importance"          "woe.table.calc"                     
```

### An Application of the Package
An example application of the package is shared below in a study of how some common steps in credit risk scoring are carried out using the functions provided in the package.

```
#This R script is designed to make the creditR package easier to understand.
#Obtaining a high accuracy model is not within the scope of this study.

#Attaching the library
library(creditR)

#Model data and data structure
data("germancredit")
str(germancredit)

#Preparing a sample data set.
sample_data <- germancredit[,c("duration.in.month","credit.amount","installment.rate.in.percentage.of.disposable.income",
                               "age.in.years","creditability")]

#Converting the ‘Creditability’ (default flag) variable into numeric type.
sample_data$creditability <- ifelse(sample_data$creditability == "bad",1,0)

#Calculating the missing ratios.
missing_ratio(sample_data)

#Splitting the data into train and test sets.
traintest <- train_test_split(sample_data,123,0.70)
train <- traintest$train
test <- traintest$test

#Applying WOE transformation on the variables.
woerules <- woe.binning(df = train,target.var = "creditability",pred.var = train,event.class = 1)
train_woe <- woe.binning.deploy(train, woerules, add.woe.or.dum.var='woe')

#Creating a dataset with the transformed variables and default flag.
train_woe <- woe.get.clear.data(train_woe,default_flag = "creditability",prefix = "woe")

#Applying the WOE rules used on the train data to the test data.
test_woe <- woe.binning.deploy(test, woerules, add.woe.or.dum.var='woe')
test_woe <- woe.get.clear.data(test_woe,default_flag = "creditability",prefix = "woe")

#Performing the IV and Gini calculations for the whole data set.
IV.calc.data(train_woe,"creditability")
Gini.univariate.data(train_woe,"creditability")

#Creating a new dataset by Gini elimination.IV elimination is also possible.
eliminated_data <- Gini_elimination(train_woe,"creditability",0.10)
str(eliminated_data)

#A demonstration of the functions useful in performing Clustering.
clustering_data <- variable.clustering(eliminated_data,"creditability", 2)
variable.clustering.gini(eliminated_data,"creditability", 2) # Returns the data for variables that have the maximum gini value in the dataset.
correlation.cluster(eliminated_data,clustering_data,variables = "variable",clusters = "Group")

#Creating a logistic regression model of the data.
model= glm(formula = creditability ~ .,
           family = binomial(link = "logit"),
           data = eliminated_data)
summary(model)

#Calculating variable weights. 
woe.glm.feature.importance(eliminated_data,model,"creditability")

#Generating the PD values for the train and test data.
ms_train_data <- cbind(eliminated_data,model$fitted.values)
ms_test_data <- cbind(test_woe[,colnames(eliminated_data)], predict(model,type = "response",newdata = test_woe))
colnames(ms_train_data) <- c("woe.duration.in.month.binned","woe.age.in.years.binned","woe.installment.rate.in.percentage.of.disposable.income.binned","creditability","PD")
colnames(ms_test_data) <- c("woe.duration.in.month.binned","woe.age.in.years.binned","woe.installment.rate.in.percentage.of.disposable.income.binned","creditability","PD")

#An example application of the Regression calibration method. The model is calibrated to the test_woe data.
regression.calibration(model,test_woe,"creditability")

#Creating a master scale.
master_scale <- master.scale(ms_train_data,"creditability","PD")


#Calibrating the master scale and the modeling data to the default rate of 5% using the bayesian calibration method.
ms_train_data$Score = log(ms_train_data$PD/(1-ms_train_data$PD)) 
ms_test_data$Score = log(ms_test_data$PD/(1-ms_test_data$PD)) 
bayesian_method <- bayesian.calibration(data = master_scale,average_score ="Score",total_observations = "Total.Observations",PD = "PD",central_tendency = 0.05,calibration_data = ms_train_data,calibration_data_score ="Score")

#After calibration, the information and data related to the calibration process can be obtained as follows.
bayesian_method$Calibration.model
bayesian_method$Calibration.formula
bayesian_method$data
bayesian_method$calibration_data

#The Scaled score can be created using the following function.
scaled.score(bayesian_method$calibration_data, "calibrated_pd", 3000, 15)

#Note : Model calibration is performed for illustration only. Model validation tests proceed through the original master scale as follows.

#Calculating the Vif values of the variables.
vif.calc(model)

#Calculating the Gini for the model.
Gini(model$fitted.values,ms_train_data$creditability)

#Performing the 5 Fold cross validation.
k.fold.cross.validation.glm(ms_train_data,"creditability",5,1)

#The KS test is performed on the distributions of the estimates for good and bad observations.
Kolmogorov.Smirnov(ms_train_data,"creditability","PD")
Kolmogorov.Smirnov(ms_test_data,"creditability","PD")

#Variable stabilities are measured.
SSI.calc.data(train_woe,test_woe,"creditability")

#The HHI test is performed to measure the concentration of the master scale.
Herfindahl.Hirschman.Index(master_scale,"Total.Observations")

#Performing the Anchor point test.
Anchor.point(master_scale,"PD","Total.Observations",0.30)

#The chisquared test is applied on the master scale.
chisquare.test(master_scale,"PD","Bad.Count","Total.Observations",0.90)

#The Binomial test is applied on the master scale.
master_scale$DR <- master_scale$Bad.Count/master_scale$Total.Observations
Binomial.test(master_scale,"Total.Observations","PD","DR",0.90,"one")
```

## Bug Fixes

Please inform me about the errors you have encountered while using the package via the e-mail address that is shared in the Author section.

## Author

* **Ayhan Dis**  - [Github](https://github.com/ayhandis) - [Linkedin](https://www.linkedin.com/in/ayhandis/)  - disayhan@gmail.com

## License

This project is licensed under the GPL-2 - See the [LICENSE.md](LICENSE.md) file for details

## Built With

* [R](https://cran.r-project.org/)
* [R Studio](https://www.rstudio.com/) 
