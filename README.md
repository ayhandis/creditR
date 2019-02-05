# creditR
## A Credit Risk Scoring and Validation Package

This package covers R functions related to the applications used in credit risk scoring. The package includes variable analysis, variable selection, model development, model calibration, rating scale and model validation methods. Through defined functions, methodologies can be applied quickly on all modeling data or a specific variable.The package was issued for the use of credit risk professionals. Basic level knowledge about credit risk scoring methodologies is required for use of the package.
### Prerequisites
In order to install the creditR package, devtools package must also be installed. You should run the following code to install the devtools package.
```
install.packages("devtools", dependencies = TRUE) 
```

### Getting Started
You can install the creditR package with the help of install_github code by using devtools package.
```
library(devtools)
install_github("ayhandis/creditR)
library(creditR)
```

### A List of Functions
The list of functions available under the package is shared below.
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
An example application of the package is shared below. It is just a study of how functions can be applied.

```
#This R script is designed to make the creditR package easier to understand.
#Within the scope of the study, it was not aimed to obtain a  high accuracy model.

#Attaching the library
library(creditR)

#Model data and data structure
data("germancredit")
str(germancredit)

#A sample data set was prepared.
sample_data <- germancredit[,c("duration.in.month","credit.amount","installment.rate.in.percentage.of.disposable.income",
"age.in.years","creditability")]

#Creditability (default flag) variable has been converted to numeric.
sample_data$creditability <- ifelse(sample_data$creditability == "bad",1,0)

#Missing ratios are calculated.
missing_ratio(sample_data)

#Data is splitted into train and test sets.
traintest <- train_test_split(sample_data,123,0.70)
train <- traintest$train
test <- traintest$test

#WOE transformation was applied on the variables.
woerules <- woe.binning(df = train,target.var = "creditability",pred.var = train,event.class = 1)
train_woe <- woe.binning.deploy(train, woerules, add.woe.or.dum.var='woe')

#A data set is created with transformed variables and default flag.
train_woe <- woe.get.clear.data(train_woe,default_flag = "creditability",prefix = "woe")

#The woe rules used on the train data were also used for the test data.
test_woe <- woe.binning.deploy(test, woerules, add.woe.or.dum.var='woe')
test_woe <- woe.get.clear.data(test_woe,default_flag = "creditability",prefix = "woe")

#IV and Gini calculations were performed for all data set.
IV.calc.data(train_woe,"creditability")
Gini.univariate.data(train_woe,"creditability")

#A new data set was created by Gini elimination. IV elimination is also possible.
eliminated_data <- Gini_elimination(train_woe,"creditability",0.10)
str(eliminated_data)

#In order to show only the use, clustering was performed.
clustering_data <- variable.clustering(eliminated_data,"creditability", 2)
variable.clustering.gini(eliminated_data,"creditability", 2) # Returns the data for variables that have the maximum gini value in the dataset.
correlation.cluster(eliminated_data,clustering_data,variables = "variable",clusters = "Group")

#A model was created using logistic regression.
model= glm(formula = creditability ~ .,
                   family = binomial(link = "logit"),
                   data = eliminated_data)
summary(model)

#Variable weights were calculated.
woe.glm.feature.importance(eliminated_data,model,"creditability")

#PD values were generated for train and test data.
ms_train_data <- cbind(eliminated_data,model$fitted.values)
ms_test_data <- cbind(test_woe[,colnames(eliminated_data)], predict(model,type = "response",newdata = test_woe))
colnames(ms_train_data) <- c("woe.duration.in.month.binned","woe.age.in.years.binned","woe.installment.rate.in.percentage.of.disposable.income.binned","creditability","PD")
colnames(ms_test_data) <- c("woe.duration.in.month.binned","woe.age.in.years.binned","woe.installment.rate.in.percentage.of.disposable.income.binned","creditability","PD")

#Regression calibration method was applied only as an example.The model was calibrated to the test_woe data.
regression.calibration(model,test_woe,"creditability")

#A master scale has been created.
master_scale <- master.scale(ms_train_data,"creditability","PD")


#The master scale and modeling data was calibrated to the default rate of 5% using the bayesian calibration method.
ms_train_data$Score = log(ms_train_data$PD/(1-ms_train_data$PD)) 
ms_test_data$Score = log(ms_test_data$PD/(1-ms_test_data$PD)) 
bayesian_method <- bayesian.calibration(data = master_scale,average_score ="Score",total_observations = "Total.Observations",PD = "PD",central_tendency = 0.05,calibration_data = ms_train_data,calibration_data_score ="Score")

#After calibration, information and data related to calibration are obtained as follows.
bayesian_method$Calibration.model
bayesian_method$Calibration.formula
bayesian_method$data
bayesian_method$calibration_data

#Scaled score could be created using the following function.
scaled.score(bayesian_method$calibration_data, "calibrated_pd", 3000, 15)

#Note : Model calibration is performed for illustration only. Model validation tests will proceed through the original master scale.

#Vif values of the model was calculated.
vif.calc(model)

#Model Gini value was calculated.
Gini(model$fitted.values,ms_train_data$creditability)

#5 Fold cross validation was performed.
k.fold.cross.validation.glm(ms_train_data,"creditability",5,1)

#The KS test was performed on the distributions of the estimates for good and bad observations.
Kolmogorov.Smirnov(ms_train_data,"creditability","PD")
Kolmogorov.Smirnov(ms_test_data,"creditability","PD")

#Variable stabilities are measured.
SSI.calc.data(train_woe,test_woe,"creditability")

#The HHI test was performed to measure the concentration of the master scale.
Herfindahl.Hirschman.Index(master_scale,"Total.Observations")

#Anchor point test was performed.
Anchor.point(master_scale,"PD","Total.Observations",0.30)

#The chisquared test was applied on the master scale.
chisquare.test(master_scale,"PD","Bad.Count","Total.Observations",0.90)

#The Binomial test was applied on the master scale.
master_scale$DR <- master_scale$Bad.Count/master_scale$Total.Observations
Binomial.test(master_scale,"Total.Observations","PD","DR",0.90,"one")

```

## Bug Fixes

Please inform me via e-mail that shared in the Author section for the errors encountered in the use of the package.

## Author

* **Ayhan Dis**  - [Github](https://github.com/ayhandis) - [Linkedin](https://www.linkedin.com/in/ayhandis/)  - disayhan@gmail.com

## License

This project is licensed under the GPL-2 - See the [LICENSE.md](LICENSE.md) file for details

### Built With

* [R](https://cran.r-project.org/)
* [R Studio](https://www.rstudio.com/) 
