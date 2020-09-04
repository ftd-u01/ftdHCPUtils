#' calcTSNR
#'
#' calculate time SNR for a vector of values (and times)
#'
#' @param values image values (typically ROI means)
#' @param times a time for each image value (assume even spacing if not provided)
#' @param remove.linear flag indicating if linear trends should be regressed out
#' @param na.rm flag to remove NA values from calculations
#' @return single tSNR value
#' @examples
#'
#' signal = sample( 40, 20 ) # 20 random values
#' times = c(1:20)
#' tSNR = calcTSNR( signal, times, TRUE )
#'
#' @export calcTSNR

calcTSNR = function( values, times=NULL, remove.linear=FALSE, na.rm=FALSE ) {

  tSNR = mean(values, na.rm=na.rm)/sd(values, na.rm=na.rm)

  if ( remove.linear ) {
    if ( is.null(times) ) {
      times = c(1:length(values))
    }

    df = data.frame(Value=values, Time=times)
    fit = lm(Value ~ Time, df)
    fitValues = df$Value - fit$fitted.values + fit$fitted.values[1]
    tSNR = mean(fitValues, na.rm=na.rm)/sd(fitValues, na.rm=na.rm)
  }

  return(tSNR)

}
