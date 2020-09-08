#' calcWScore
#'
#' calculate wscores from control and subject values
#'
#' @param controlData data.frame of values used to construct the model
#' @param subjectData data.frame of values used to determine wscores
#' @param target name of dependent variable (ie left-hand side of regression equation)
#' @param predictors string showing right-hand side of regression equation
#' @return array of wscores (1 per row in subjectData)
#' @examples
#' cValue = sample(40,20)                   # random control values
#' cGroup = factor( sample(2,20,replace=T)) # random 2-level control group
#' cVar =  sample(40,20)+20                 # random continuous control variable
#' sValue = sample(40,10)
#' sGroup = factor( sample(2,10,replace=T))
#' sVar = sample(40,10)+20
#' cData = data.frame(Value=cValue, Group=cGroup, Var=cVar)
#' sData = data.frame(Value=sValue, Group=sGroup, Var=sVar)
#' sData$wScore = calcWScore( cData, sData, 'Value', 'Group + Var')
#' @export calcWScore

calcWScore = function( controlData, subjectData, target, predictors ) {

  model = paste(target, "~", predictors)
  controlModel = lm( as.formula(model), controlData)
  wscoreDenom = sd(resid(controlModel))

  wScore = (subjectData[target] - predict(controlModel, subjectData))/wscoreDenom
  #names(wScore) = paste0(names(wScore),".wScore")
  #wScore = as.numeric(wScore)
  return(wScore[,1])

}
