#' imageTSNR
#'
#' calculate time SNR for a vector of values (and times)
#'
#' @param image 4D image (typically ROI means)
#' @param labelImage 3D image of labeled regions to calculate tSNR within
#' @param labels optional array of lables to calculate tSNR within (default = all label values)
#' @param times optional array of time indices to use (use all if not provided)
#' @param remove.linear flag indicating if linear trends should be regressed out
#' @param na.rm flag to remove NA values from calculations
#' @return data.frame of Labels and corresponding tSNR values
#' @examples
#'
#' \dontrun{
#' image = antsImageRead("mri.nii.gz")
#' labelImg = antsImageRead("mri_labeled.nii.gz")
#' tSNR = imageTSNR( image, labelImg, remove.linear=T ) # all labels, all times
#' }
#' @export imageTSNR
imageTSNR = function( image, labelImage, labels=NULL, times=NULL, remove.linear=FALSE ) {

  if ( length(dim(image)) != 4 ) {
    exit("image must be 4D")
  }
  if ( length(dim(labelImage)) != 3) {
    exit("labelImage must be 3D")
  }

  if ( is.null(labels) ) {
    labels=unique(labelImage)
  }
  dat = data.frame(Label=labels)

  if ( is.null(times) ) {
    times = c(1:dim(image)[4])
  }

  dat$tSNR = rep(NA, length(dat$Label))

  for ( l in 1:length(dat$Label) ) {

    labelMeans = rep(NA, length(times))
    for ( t in 1:length(times) ) {
      timepoint = extractSlice(image, times[t], 4)
      labelMeans[t] = mean( timepoint[labelImage==dat$Label[l]] )
    }

    dat$tSNR[l] = calcTSNR( labelMeans, times, remove.linear=remove.linear)

  }

  return(dat)

}
