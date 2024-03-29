#' Downloads and reads Lattes data based on a vector of Lattes ids
#'
#' DEPRECATED. Please use function gld_get_lattes_data_from_zip to read the files
#'
#' This function downloads xml data directly from Lattes and reads the resulting file, giving as output three dataframes
#' , a table with description of researchers, list of all publications and a table with completed supervisions
#'
#' @param id.vec A vector of Lattes ids (e.g. id.vec <- c('K4723925J2', 'K4713546D3') )
#' @param field.qualis Area of Qualis to get Qualis journal rankings (default equals NULL). Eg. area.qualis <- 'ECONOMIA'
#'@param folder.dl Name of folder where to store xml files (default = tempdir())
#'
#' @return Returns a list with two components:
#'  \describe{
#'   \item{tpesq}{A dataframe with information about researchers}
#'   \item{tpublic}{A dataframe with information about publications}
#'   \item{tsupervisions}{A dataframe with information about all supervisions}
#' }
#'
#' @noRd
#'
#' @examples
#'
#' \dontrun{
#' l.out <- gld_get_lattes_data(id.vec = 'K4713546D3',
#'                              field.qualis = 'ECONOMIA')
#' print(l.out$tpesq)
#' }
gld_get_lattes_data <- function(id.vec,
                                field.qualis = NULL,
                                folder.dl = tempdir()) {

#   my.message <- paste0('\n\n:(\n\nSadly, the Lattes address where the xml files were found and downloaded ',
# 'without a captcha wall is not online for a couple of weeks now.  Difficult to say why is that. ',
# 'I hope, and will keep track of, if a new site is published.',
# 'In the meanwhile, the features of GetLattesData are relegated to offline files.',
# '\nMarcelo Perlin, 2017-11-28 ')
#
#   cat(my.message)
#
#   cat('\n\nReturning an empty dataframe..')
#   return(data.frame())

  # deprecations message
  my.message <- paste0('Function gld_get_lattes_data is deprecated.',
                       '\n\nDue to changes in lattes website, the automatic download of xml files no longer works without captcha. ',
                       '\nIn order to use the package, you must  download the xml zip files individually (see XML button on top right of lattes page) and use ',
                       'function gld_get_lattes_data_from_zip to read all the data',
                       '\n\nExiting Now..')

  cat(my.message)

  return(FALSE)

  # check args
  id.vec <- as.character(id.vec)
  n.char <- nchar(id.vec)

  # Check length id
  if (any(n.char!=10)) {
    cat(paste0('You have ids with less or more than 10 characters. ',
               'You should check for the corresponding 10 char id in the _new_ Lattes website(http://lattes.cnpq.br/)',
               '. Just search for the name of the scholar. Exiting now..') )
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
    tpublic.published <- do.call(args = lapply(my.l, function(x) x$tpublic.published), what = dplyr::bind_rows)
    tpublic.accepted <- do.call(args = lapply(my.l, function(x) x$tpublic.accepted), what = dplyr::bind_rows)
    tsupervisions <- do.call(args = lapply(my.l, function(x) x$tsupervisions), what = dplyr::bind_rows)
    tbooks <- do.call(args = lapply(my.l, function(x) x$tbooks), what = dplyr::bind_rows)
    tconferences <- do.call(args = lapply(my.l, function(x) x$tconferences), what = dplyr::bind_rows)
  })

  # do Qualis
  if (!(is.null(field.qualis ))) {
    df.qualis <- gld_get_qualis(field.qualis = field.qualis)

    idx <- match(tpublic.published$ISSN, df.qualis$issn)
    tpublic.published$qualis <- df.qualis$ranking[idx]

    idx <- match(tpublic.accepted$ISSN, df.qualis$issn)
    tpublic.accepted$qualis <- df.qualis$ranking[idx]
  }

  # do sjr
  df.sjr <- gld_get_SJR()

  #idx <- match(tpublic.published$ISSN, df.sjr$Issn)
  # fix for multiple issn (https://github.com/msperlin/GetLattesData/issues/6#issuecomment-412626175)
  idx <- unlist(sapply(stringr::str_replace_all(tpublic.published$ISSN, "-", "" ),
                       function(issn.in, df.sjr){
                         temp.idx <- which(stringr::str_detect( df.sjr$Issn,issn.in))

                         if (stringr::str_trim(issn.in) == '') return(NA)

                         if(length(temp.idx) == 0){
                           temp.idx <- NA
                         }
                         return(temp.idx[1])
                       } ,
                       df.sjr = df.sjr,
                       USE.NAMES=F))


  tpublic.published$SJR <- df.sjr$SJR[idx]
  tpublic.published$H.SJR <- df.sjr$`H index`[idx]

  #idx <- match(tpublic.accepted$ISSN, df.sjr$Issn)
  # fix for multiple issn (https://github.com/msperlin/GetLattesData/issues/6#issuecomment-412626175)
  idx <- unlist(sapply(stringr::str_replace_all(tpublic.accepted$ISSN, "-", "" ),
                       function(issn.in, df.sjr){
                         temp.idx <- which(stringr::str_detect(df.sjr$Issn, issn.in ))

                         if (stringr::str_trim(issn.in) == '') return(NA)

                         if(length(temp.idx) == 0){
                           temp.idx <- NA
                         }
                         return(temp.idx[1])
                       } ,
                       df.sjr = df.sjr,
                       USE.NAMES=F))
  tpublic.accepted$SJR <- df.sjr$SJR[idx]
  tpublic.accepted$H.SJR <- df.sjr$`H index`[idx]

  # fix datatypes
  suppressWarnings({
    tpesq$name           <- as.character(tpesq$name)
    tpesq$phd.start.year <- as.numeric(tpesq$phd.start.year)
    tpesq$phd.end.year   <- as.numeric(tpesq$phd.end.year)
    tpesq$major.field    <- as.character(tpesq$major.field)
    tpesq$minor.field    <- as.character(tpesq$minor.field)
    tpesq$country.origin <- as.character(tpesq$country.origin)

    tpublic.published$name         <- as.character(tpublic.published$name)
    tpublic.published$year         <- as.numeric(tpublic.published$year)
    tpublic.published$language     <- as.character(tpublic.published$language)
    #tpublic.published$start.page   <- as.numeric(tpublic.published$start.page)
    #tpublic.published$end.page     <- as.numeric(tpublic.published$end.page)
    tpublic.published$order.aut    <- as.numeric(tpublic.published$order.aut)
    tpublic.published$n.authors    <- as.numeric(tpublic.published$n.authors)

    tpublic.accepted$name         <- as.character(tpublic.accepted$name)
    tpublic.accepted$year         <- as.numeric(tpublic.accepted$year)
    tpublic.accepted$language     <- as.character(tpublic.accepted$language)
    tpublic.accepted$order.aut    <- as.numeric(tpublic.accepted$order.aut)
    tpublic.accepted$n.authors    <- as.numeric(tpublic.accepted$n.authors)


  })

  # force utf-8
  my.enc.fct <- function(x){
    if (is.character(x)) Encoding(x) <- 'UTF-8'
    return(x)
  }

  tpesq <- dplyr::as_tibble(lapply(tpesq, my.enc.fct))

  tpublic.published <- dplyr::as_tibble(lapply(tpublic.published, my.enc.fct))

  tpublic.accepted <- dplyr::as_tibble(lapply(tpublic.accepted, my.enc.fct))

  tsupervisions <- dplyr::as_tibble(lapply(tsupervisions, my.enc.fct))

  tbooks <- dplyr::as_tibble(lapply(tbooks, my.enc.fct))

  tconferences <- dplyr::as_tibble(lapply(tconferences, my.enc.fct))

  # return data
  l.out <- list(tpesq = tpesq,
                tpublic.published = tpublic.published,
                tpublic.accepted = tpublic.accepted,
                tsupervisions = tsupervisions,
                tbooks = tbooks,
                tconferences = tconferences)

  return(l.out)

}
