########### Acumos component source example ###########
# Calculate variable means by Species on Iris dataset
acumos_transform <- function(..., inputs=c(
  Sepal.Length="numeric",
  Sepal.Width="numeric",
  Petal.Length="numeric",
  Petal.Width="numeric",
  Species="character"
), outputs=c(
  mean.Sepal.Length="numeric",
  mean.Sepal.Width="numeric",
  mean.Petal.Length="numeric",
  mean.Petal.Width="numeric",
  Species="character"
)){
  df<-...
  means<-by(df, df$Species, function(x){
    apply(x[,-5], MARGIN = 2, mean)
  })
  do.call(rbind,means)
}
# Predict Species on Iris dataset using RF
## Train the model 
library(randomForest)
rf <- randomForest(Species ~ ., data=iris)
## Write the function executing the model
acumos_predict <- function(..., inputs=c(
  Sepal.Length="numeric",
  Sepal.Width="numeric",
  Petal.Length="numeric",
  Petal.Width="numeric"
)
, outputs=c(predictedSpecies="character")
){
  as.character(predict(rf, as.data.frame(list(...))))
} 
