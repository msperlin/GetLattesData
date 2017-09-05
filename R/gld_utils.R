#' Reads Qualis table
#'
#' Read Qualis data available within the package. The original data is downloaded from
#' \url{https://sucupira.capes.gov.br/sucupira/public/index.xhtml}
#'
#' Latest Qualis: 2013-2016 | Latest Qualis update: 2017-09-04
#'
#' @inheritParams gld_get_lattes_data
#'
#' @return A dataframe with Qualis table, including columns for ISSN, field and ranking
#' @export
#'
#' @examples
#'
#' df.qualis <- gld_get_qualis()
#' df.qualis
gld_get_qualis <- function(field.qualis = 'ALL') {

  # load data from csv
  qualis.file <- system.file('extdata/Qualis_2013-2016.zip', package = 'GetLattesData')

  # set readr cols and read it!
  my.cols <- readr::cols(
    issn    = readr::col_character(),
    titulo  = readr::col_character(),
    area    = readr::col_character(),
    ranking = readr::col_character()
  )

  df.qualis <- readr::read_csv(qualis.file,
                               locale = readr::locale(encoding = 'Latin1'),
                               col_types = my.cols,
                               progress = F)

  # check area
  unique.fields <- c(unique(df.qualis$area), 'ALL')

  if ( !(field.qualis %in% unique.fields) ) {
    cat(paste0('ERROR: Cant find field ', field.qualis, ' in Qualis. \n\n',
                'You should use one of the following:\n\n', paste(unique.fields, collapse = '\t'))  )

    stop('CAnt find field in qualis.. See previous message')
  }

  if (field.qualis != 'ALL') {
    df.qualis <- df.qualis[df.qualis$area == field.qualis, ]
  }

  return(df.qualis)

}

#' Reads SJR table
#'
#' Reads localy available SJR table. Original data provided as an excel file in \url{http://scimagojr.com/journalrank.php}.
#'
#' Latest SJR: 2016 | Latest update: 2017-09-04
#'
#' @return A dataframe with SJR table
#' @export
#'
#' @examples
#' df.SJR <- gld_get_SJR()
#' df.SJR
gld_get_SJR <- function(){

  # get file
  sjr.file <- system.file('extdata/scimagojr_2016_shortver.zip', package = 'GetLattesData')

  # set cols and read it!
  my.cols <- readr::cols(
    Title = readr::col_character(),
    Issn = readr::col_character(),
    SJR = readr::col_double(),
    `H index` = readr::col_integer()
  )

  df.sjr <- readr::read_csv(sjr.file, col_types = my.cols, progress = F)

  # fix issn
  df.sjr$Issn <- stringr::str_replace(df.sjr$Issn, 'ISSN ', '')
  df.sjr$Issn <- paste0(stringr::str_sub(df.sjr$Issn, 1, 4),
                        '-',
                        stringr::str_sub(df.sjr$Issn, 5, 8))
  return(df.sjr)
}

#' Downloads data from Lattes
#'
#' @param id Id symbol from Lattes (see gld_get_lattes_data)
#' @inheritParams gld_get_lattes_data
#'
#' @return The name of downloaded zip file
#'
#' @examples
#'
#' \dontrun{
#' file.out <- gld_download_lattes_files(id = 'K4723925J2')
#' }
#'
gld_download_lattes_files <- function(id, folder.dl = tempdir()) {

  # set link
  base.link <- 'http://buscacv.cnpq.br/buscacv/rest/download/curriculo/'
  my.link <- paste0(base.link,id)

  # set destination file by indexing by date
  dest.file <- file.path(folder.dl, paste0(id,'_',Sys.Date(), '.zip') )

  # check file
  if (file.exists(dest.file)) {
    cat('\nFound file ', dest.file, '\t', 'skipping it')
  } else {
    cat('\nDownloading file ', dest.file)

    # download file
    utils::download.file(url = my.link,destfile = dest.file,
                         quiet = T, mode = 'wb', method = 'internal')
  }

  if (!file.exists(dest.file)) {
    stop('Error in downloading id ', id, '. Perhaps you should check your ids?')
  }

  return(dest.file)
}
