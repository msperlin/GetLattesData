#' Reads zip files from Lattes
#'
#' This function reads zipped files from Lattes, giving as output a list with several dataframes
#'
#' @param zip.files A vector with location of zip files downloaded from Lattes website
#' @param field.qualis Area of Qualis to get Qualis journal rankings (default equals NULL). Eg. area.qualis <- 'ECONOMIA'
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
#' \dontrun{
#' # get files from pkg (you can download from other researchers in lattes website)
#' f.in <- system.file('extdata/3262699324398819.zip', package = 'GetLattesData')
#'
#' # set qualis
#' field.qualis = 'ECONOMIA'
#'
#' # get data
#' l.out <- gld_get_lattes_data_from_zip(f.in, field.qualis = field.qualis )
#'
#' # print it
#' print(l.out$tpesq)
#' print(l.out$tpublic.published)
#' }
gld_get_lattes_data_from_zip <- function(zip.files,
                                         field.qualis = NULL) {

  # check args
  zip.files <- as.character(zip.files)

  # Check extension
  my.extensions <- tools::file_ext(zip.files)

  if (any(my.extensions != 'zip')) {
    stop('All files in zip.files input should be with .zip extension. Check your inputs..')
  }

  # read files
  my.l <- lapply(zip.files, gld_read_zip)

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

  if (!all(is.na(idx))) {
    tpublic.published$SJR <- df.sjr$SJR[idx]
    tpublic.published$H.SJR <- df.sjr$`H index`[idx]

  } else if (nrow(tpublic.published)!=0) {

    tpublic.published$SJR <- NA
    tpublic.published$H.SJR <- NA
  }



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

  if (!all(is.na(idx))) {

    tpublic.accepted$SJR <- df.sjr$SJR[idx]
    tpublic.accepted$H.SJR <- df.sjr$`H index`[idx]

  } else if (nrow(tpublic.accepted) != 0){
    tpublic.accepted$SJR <- NA
    tpublic.accepted$H.SJR <- NA

  }

  #browser()

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

  tpesq <- as.data.frame(lapply(tpesq, my.enc.fct),
                         stringsAsFactors = F)

  tpublic.published <- as.data.frame(lapply(tpublic.published, my.enc.fct),
                                     stringsAsFactors = F)

  tpublic.accepted <- as.data.frame(lapply(tpublic.accepted, my.enc.fct),
                                    stringsAsFactors = F)

  tsupervisions <- as.data.frame(lapply(tsupervisions, my.enc.fct),
                                 stringsAsFactors = F)

  tbooks <- as.data.frame(lapply(tbooks, my.enc.fct),
                          stringsAsFactors = F)

  tconferences <- as.data.frame(lapply(tconferences, my.enc.fct),
                                stringsAsFactors = F)

  # return data
  l.out <- list(tpesq = tpesq,
                tpublic.published = tpublic.published,
                tpublic.accepted = tpublic.accepted,
                tsupervisions = tsupervisions,
                tbooks = tbooks,
                tconferences = tconferences)

  return(l.out)

}
