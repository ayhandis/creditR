# creditR
## A Credit Risk Scoring and Validation Package

This package covers R functions related to the applications used in credit risk scoring. The package includes variable analysis, variable selection, model development, model calibration, rating scale and model validation methods. Through defined functions, methodologies can be applied quickly on all modeling data or a specific variable.The package was issued for the use of credit risk professionals. For the use of the package, basic level knowledge about credit risk scoring methodologies is required.
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

* Adjusted.Binomial.test                                                        * master.scale            * scaled.score
* Adjusted.Herfindahl.Hirschman.Index        * Gini_elimination                 * missing_elimination     * SSI.calc
* Anchor.point                               * Herfindahl.Hirschman.Index       * missing_ratio           * SSI.calc.data
* bayesian.calibration                       * IV.calc                          * na_checker              * train_test_balanced_split
* Binomial.test                              * IV.calc.data                     * na_filler_contvar       * train_test_split
* chisquare.test                             * IV_elimination                   * PSI.calc                * variable.clustering
* correlation.cluster                        * k.fold.cross.validation.glm      * PSI.calc.data           * variable.clustering.gini
* Gini.univariate                            * Kolmogorov.Smirnov               * regression.calibration  * vif.calc
* Gini.univariate.data             



         
                                 "woe.get.clear.data"                
[34] "woe.glm.feature.importance"          "woe.table.calc"

### An Application of the Package
An example application of the package is shared below. It is just a study of how functions can be applied.

```
Give an example
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
