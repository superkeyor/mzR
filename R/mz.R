
#' preProcess + predict itself
#' @description reProcess + predict itself
#' @param x  x
#' @note short for the following example:
#' \cr prep <- preProcess(x, method = c("BoxCox","center", "scale"))
#' \cr x <- predict(prep, x)
#' @export
mz.precess = function(x,...){
  preProcessObj=caret::preProcess(x,...)
  return(predict(preProcessObj,x))
}

#' summaryFunction = fiveStats
#' @description summaryFunction = fiveStats
#' @export
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(..))

#' getModelInfo
#' @description \code{\link[caret]{getModelInfo}}
#' @param model train method name
#' @param regex T/F, matching with regex or not
#' @return returns an invisible data frame
#' @seealso \url{http://topepo.github.io/caret/available-models.html}
#' @export
mz.models = function(model = NULL, regex = TRUE, ...){
    models = ez.header('model_label'=character(),'method'=character(),'type'=character(),'library'=character(),'tuning_parameters'=character())
    modelLists = caret::getModelInfo(model=model,regex=regex,...)
    for (theMethod in names(modelLists)) {
        modelList = modelLists[[theMethod]]
        theLabel = toString(modelList$label)
        theType = toString(modelList$type)
        theLibrary = toString(modelList$library)
        theParameter = toString(paste0(modelList$parameters$parameter,' (',modelList$parameters$label,') '))
        models = ez.append(models,list(theLabel,theMethod,theType,theLibrary,theParameter),print2screen=F)
    }
    View(models)
    cat(sprintf('caret::getModelInfo("%s")[[i]] to see more details\n',model))
    return(invisible(models))
}