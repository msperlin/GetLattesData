#' Downloads and reads Lattes data based on a vector of Lattes ids
#'
#' This function downloads xml data directly from Lattes and reads the resulting file, giving as output three dataframes
#' , a table with description of researchers, list of all publications and a table with completed supervisions
#'
#' @param id.vec A vector of Lattes ids (e.g. id.vec <- c('K4723925J2', 'K4713546D3') )
#' @param field.qualis Area of Qualis to get Qualis journal rankings (default equals NULL). Eg. area.qualis <- 'ECONOMIA'
#' @param folder.dl Name of folder where to store xml files (default = tempdir())
#'
#' @return Returns a list with two components:
#'  \describe{
#'   \item{tpesq}{A dataframe with information about researchers}
#'   \item{tpublic}{A dataframe with information about publications}
#'   \item{tsupervisions}{A dataframe with information about all supervisions}
#' }
#'
#' @export
#'
#' @examples
#'
#' l.out <- gld_get_lattes_data(id.vec = 'K4713546D3',
#'                              field.qualis = 'ECONOMIA')
#' print(l.out$tpesq)
gld_get_lattes_data <- function(id.vec,
                                field.qualis = NULL,
                                folder.dl = tempdir()) {


  # check args
  id.vec <- as.character(id.vec)
  n.char <- nchar(id.vec)

  if (any(n.char!=10)) {
    stop('All ids should be 10 character long. Check your input for id.vec.')
  }

  if (!dir.exists(folder.dl)) {
    cat('Folder ', folder.dl, 'does not exists. Creating it..')
    dir.create(folder.dl)
  }

  # check internet

  if (!curl::has_internet()) {
    stop('You need an internet connection to download files from Lattes.')
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
    tsupervisions <- do.call(args = lapply(my.l, function(x) x$tsupervisions), what = dplyr::bind_rows)
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

  # fix datatypes

  suppressWarnings({
    tpesq$name           <- as.character(tpesq$name)
    tpesq$phd.start.year <- as.numeric(tpesq$phd.start.year)
    tpesq$phd.end.year   <- as.numeric(tpesq$phd.end.year)
    tpesq$major.field    <- as.character(tpesq$major.field)
    tpesq$minor.field    <- as.character(tpesq$minor.field)
    tpesq$country.origin <- as.character(tpesq$country.origin)

    tpublic$name         <- as.character(tpublic$name)
    tpublic$year         <- as.numeric(tpublic$year)
    tpublic$language     <- as.character(tpublic$language)
    tpublic$start.page   <- as.numeric(tpublic$start.page)
    tpublic$end.page     <- as.numeric(tpublic$end.page)
    tpublic$order.aut    <- as.numeric(tpublic$order.aut)
    tpublic$n.authors    <- as.numeric(tpublic$n.authors)


  })

  # force utf-8
  my.enc.fct <- function(x){
    if (is.character(x)) Encoding(x) <- 'UTF-8'
    return(x)
  }

  tpesq <- as.data.frame(lapply(tpesq, my.enc.fct),
                         stringsAsFactors = F)

  tpublic <- as.data.frame(lapply(tpublic, my.enc.fct),
                           stringsAsFactors = F)

  tsupervisions <- as.data.frame(lapply(tsupervisions, my.enc.fct),
                           stringsAsFactors = F)


  # return data
  l.out <- list(tpesq = tpesq, tpublic = tpublic, tsupervisions = tsupervisions)
  return(l.out)

}
