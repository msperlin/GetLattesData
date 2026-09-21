#' Downloads and reads Lattes data based on a vector of Lattes ids
#'
#' DEPRECATED. Please use function gld_get_lattes_data_from_zip to read the files
#'
#' Due to changes in the Lattes website, automatic download of xml files no
#' longer works without a captcha. The function is kept only to signal the
#' deprecation.
#'
#' @param id.vec A vector of Lattes ids (e.g. id.vec <- c('K4723925J2', 'K4713546D3') )
#' @param field.qualis Area of Qualis to get Qualis journal rankings (default equals NULL). Eg. area.qualis <- 'ECONOMIA'
#' @param folder.dl Name of folder where to store xml files (default = tempdir())
#'
#' @noRd
gld_get_lattes_data <- function(id.vec,
                                field.qualis = NULL,
                                folder.dl = tempdir()) {

  my.message <- paste0(
    'Due to changes in lattes website, the automatic download of xml files no longer works without captcha. ',
    '\nIn order to use the package, you must download the xml zip files individually (see XML button on top right of lattes page) and use ',
    'function gld_read_zip2() to read all the data'
  )

  lifecycle::deprecate_stop(
    when = "1.2",
    what = "gld_get_lattes_data()",
    with = "gld_read_zip2()",
    details = my.message
  )

}
