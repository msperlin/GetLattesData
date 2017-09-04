#' Downloads and reads Lattes data based on a vector of Lattes ids
#'
#' This function downloads xml data directly from Lattes and reads the resulting file, giving as output two dataframes
#' , one with the description of researchers and the second with several information about all publications
#'
#' @param id.vec A vector of Lattes ids (e.g. id.vec <- c('K4723925J2', 'K4713546D3') )
#' @param field.qualis Area of Qualis to get Qualis journal rankings (default equals NULL). Eg. area.qualis <- 'ECONOMIA'
#' @param folder.dl Name of folder where to store xml files (default = tempdir())
#'
#' @return Returns a list with two components:
#'  \describe{
#'   \item{tpesq}{A dataframe with information about researchers}
#'   \item{tpublic}{A dataframe with information about publications}
#' }
#'
#' @export
#'
#' @examples
#'
#' l.out <- gld_get_lattes_data(id.vec = 'K4713546D3',
#'                              field.qualis = 'ECONOMIA',
#'                              folder.dl = 'lattes files')
#' print(l.out$tpesq)
gld_get_lattes_data <- function(id.vec,
                                field.qualis = NULL,
                                folder.dl = tempdir()) {


  # check args
  id.vec <- as.character(id.vec)
  n.char <- nchar(id.vec)

  if (any(n.char!=10)) {
    stop('All ids should have 10 characters. Check your input for id.vec.')
  }

  if (!dir.exists(folder.dl)) {
    cat('Folder ', folder.dl, 'does not exists. Creating it..')
    dir.create(folder.dl)
  }

  # download files
  zip.out <- sapply(X = id.vec,
                    FUN = gld_download_lattes_files, folder.dl = folder.dl)

  # read files
  my.l <- lapply(zip.out, gld_read_zip)

  # save tpesq (quietly, please)
  suppressWarnings({
    tpesq   <- do.call(args = lapply(my.l, function(x) x$tpesq)  , what = dplyr::bind_rows)
    tpublic <- do.call(args = lapply(my.l, function(x) x$tpublic), what = dplyr::bind_rows)
  })

  # do Qualis
  if (!(is.null(field.qualis ))) {
    df.qualis <- gld_get_qualis(field.qualis = field.qualis)
    idx <- match(tpublic$ISSN, df.qualis$issn)

    tpublic$qualis <- df.qualis$ranking[idx]
  }

  # do sjr
  df.sjr <- gld_get_SJR()

  idx <- match(tpublic$ISSN, df.sjr$Issn)
  tpublic$SJR <- df.sjr$SJR[idx]
  tpublic$H.SJR <- df.sjr$`H index`[idx]

  # return data
  l.out <- list(tpesq = tpesq, tpublic = tpublic)
  return(l.out)

}
