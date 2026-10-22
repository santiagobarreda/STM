#' Bootstrap data
#'
#' Bootstrap data for use in cross-validation. Resampling with replacement is
#' done based on unique elements in the 'speaker' vector: each unique speaker
#' has the same chance of being drawn, regardless of how many rows they have,
#' and all of a drawn speaker's rows are included.
#'
#' @param data A data frame containing vowel formant data.
#' @param speaker The name of the column in \code{data} containing speaker IDs.
#' @param relabel A boolean. If \code{TRUE}, speakers drawn more than once get
#' new IDs for their second and later copies (e.g. "m01_2", "m01_3"), so that
#' functions that group by speaker (e.g. \code{normalize()}) treat each copy
#' as a separate speaker. The speaker column is returned as a character
#' vector in this case.
#' @return A data frame.
#' @examples
#' \dontrun{
#' set.seed(1)
#' boot = bootstrap_data(h95_data_no_NA, speaker = "speaker")
#' length(unique(boot$speaker))
#' head(boot[, c("speaker", "vowel", "f1", "f2")])
#' }
#' @export


bootstrap_data = function (data, speaker = "speaker", relabel = TRUE){
  if (!is.character(speaker) || length(speaker) != 1 || !speaker %in% names(data))
    stop("speaker must be the name of a column in data.", call. = FALSE)

  ids = as.character(data[[speaker]])
  unique_speakers = unique(ids)
  draw = sample(unique_speakers, length(unique_speakers), replace = TRUE)
  speaker_rows = split(seq_len(nrow(data)), factor(ids, levels = unique_speakers))[draw]

  t_data = data[unlist(speaker_rows, use.names = FALSE), , drop = FALSE]

  if (relabel){
    copy = stats::ave(seq_along(draw), draw, FUN = seq_along)
    new_ids = ifelse(copy == 1, draw, paste(draw, copy, sep = "_"))
    t_data[[speaker]] = rep(new_ids, lengths(speaker_rows))
  }
  rownames(t_data) = NULL

  return (t_data)
}
