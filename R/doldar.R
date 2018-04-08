
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

#' @importFrom rJava .jnew .jcall
#' @export
sample_do <- function(formula, ds, iterations = 2000, samplerType="xyz.lejon.bayes.models.probit.HorseshoeDOProbitEJML") {
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
  return(lda)
}

#' @importFrom rJava .jcall
#' @export
get_betas <- function(lda) {
  theta  <- .jcall(lda,"[[D","getBetas",simplify = TRUE)
  return(theta)
}