#' Bootstrap data
#'
#' Bootstrap data for use in cross-validation. Resampling with replacement is
#' done based on unique elements in the 'speaker' vector.
#'
#' @param data A data frame containing vowel formant data.
#' @param speaker A factor variable with subject IDs.
#' @return A data frame.
#' @examples
#' # TBD
#' @export



bootstrap_data = function (data, speaker = NULL){
  t_data = split (data, data[,speaker])
  t_speakers = sample (data[,speaker], length(unique(data[,speaker])), replace = TRUE)
  t_data = do.call(rbind, t_data[t_speakers])

  return (t_data)
}

