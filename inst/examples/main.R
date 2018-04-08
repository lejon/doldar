library(doldar)

data(iris)

iterations <- 1000

lda <- sample_do(iris, Species~., iterations = iterations)

betas <- get_betas(lda)

mm <- model.matrix(Species~.,iris)
fit <- predict(lda,mm)

sum(fit$preds == as.integer(iris$Species)) / length(fit$preds)

