library(doldar)

data(iris)

iterations <- 1000

lda <- sample_do(Species~., iris, iterations = iterations)

betas <- get_betas(lda)

