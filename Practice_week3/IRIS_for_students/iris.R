# required packages
# install.packages("datasets")

install.packages("pmml")
library(pmml)

# grab iris data
in_iris=iris
names(in_iris)
colnames(in_iris)=c("sepal_length", "sepal_width",
                    "petal_length", "petal_width", 
                    "species" )
table(in_iris$species)

# build a regression model for sepal_length
iris_lm=lm(sepal_length~., data=in_iris)
summary(iris_lm)

# create pmml
iris_lm_pmml=pmml(iris_lm)
save_pmml(iris_lm_pmml, "iris.xml")

# install.packages("foreign") to prepare export iris
library(foreign)
write.foreign(df=in_iris, datafile="in_iris.csv", codefile="in_iris.sas", package="SAS")
