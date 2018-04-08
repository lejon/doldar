
#' @importFrom rJava .jnew .jcall
#' @export
new_simple_do_config <- function(iterations, tmpdir = "/tmp") {
  lu <- .jnew("xyz.lejon.utils.LoggingUtils")
  .jcall(lu,"Ljava/io/File;","checkAndCreateCurrentLogDir",tmpdir);
  #SimpleLDAConfiguration(LoggingUtils logUtil, String scheme,
  #                       Integer noTopics, Double alpha, Double beta, Integer noIters,
  #                       Integer noBatches, Integer rareThreshold, Integer topicInterval,
  #                       Integer startDiagnostic, int seed, String datasetFn)
  slc <- .jnew("xyz.lejon.configuration.SimpleDOConfiguration")
  .jcall(slc,"V","setLoggingUtil",lu)
  .jcall(slc,"V","setNrIterations",as.integer(iterations))
  return(slc)
}

#' @export
sample_do <- function (x, ...) {
  UseMethod("sample_do", x)
}

#' @importFrom rJava .jnew .jcall
#' @export
sample_do.matrix <- function(ds, Y, iterations = 2000, samplerType="xyz.lejon.bayes.models.probit.HorseshoeDOProbitEJML") {
  #.jconstructors(samplerType)
  ldaconfig <- new_simple_do_config(iterations)
  X <- ds
  storage.mode(X) <- "double"
  Y <- as.integer(Y)

  lcfg <- .jcast(ldaconfig,"xyz.lejon.configuration.DOConfiguration")
  ex <- tryCatch(lda <- .jnew(samplerType,lcfg,.jarray(X,dispatch = T),Y), NullPointerException = function(ex) ex)
  #lda <- .jnew("cc.mallet.topics.SpaliasUncollapsedParallelLDA",.jcast(slc,"cc.mallet.configuration.LDAConfiguration"))
  .jcall(lda,"V", "sample", as.integer(iterations))
  betas  <- .jcall(lda,"[[D","getBetas",simplify = TRUE)
  res <- list(lda_obj = lda, formula = formula, data = ds, betas=betas)
  class(res) <- "diagonal_orthant"
  return(res)
}

#' @importFrom rJava .jnew .jcall
#' @export
sample_do.data.frame <- function(ds, formula, iterations = 2000, samplerType="xyz.lejon.bayes.models.probit.HorseshoeDOProbitEJML") {
  #.jconstructors(samplerType)
  ldaconfig <- new_simple_do_config(iterations)
  Yname <- all.vars(formula)[1]
  X <- model.matrix(formula,ds)
  storage.mode(X) <- "double"
  Y <- as.integer(ds[,Yname])

  lcfg <- .jcast(ldaconfig,"xyz.lejon.configuration.DOConfiguration")
  ex <- tryCatch(lda <- .jnew(samplerType,lcfg,.jarray(X,dispatch = T),Y), NullPointerException = function(ex) ex)
  #lda <- .jnew("cc.mallet.topics.SpaliasUncollapsedParallelLDA",.jcast(slc,"cc.mallet.configuration.LDAConfiguration"))
  .jcall(lda,"V", "sample", as.integer(iterations))
  betas  <- .jcall(lda,"[[D","getBetas",simplify = TRUE)
  res <- list(lda_obj = lda, formula = formula, data = ds, betas=betas)
  class(res) <- "diagonal_orthant"
  return(res)
}

#' @export
get_betas <- function(lda) {
  return(lda$betas)
}

#' @export
predict.diagonal_orthant <- function(model, newdata) {
  stopifnot(inherits(model,"diagonal_orthant"))
  eval <- .jnew("xyz.lejon.bayes.models.probit.DOEvaluation")
  betas <- model$betas
  #EvalResult result  = DOEvaluation.evaluateMaxA(testset, testLabels, betas, true);
  res <- .jcall(eval,"Lxyz/lejon/eval/EvalResult;", "predict",
                .jarray(newdata,dispatch = T),
                .jarray(betas,dispatch = T))
  predictions <- .jcall(res,"[I","getPredictedLabels")
  fit <- list(preds=predictions)
  class(fit) <- "diagonal_orthant.fit"
  return(fit)
}
