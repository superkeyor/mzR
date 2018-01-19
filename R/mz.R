
#' preProcess + predict itself
#' @description reProcess + predict itself
#' @param x  x
#' @export
mz.precess = function(x,...){
  preProcessObj=caret::preProcess(x,...)
  return(predict(preProcessObj,x))
}