#' Reads Qualis table
#'
#' Read Qualis data available within the package, retrieved at 2023-11-27.
#' The original data is downloaded from
#' \url{https://sucupira.capes.gov.br/sucupira/public/index.xhtml}
#'
#' @noRd
gld_get_qualis <- function(field.qualis = 'ALL') {

  # load data from csv
  qualis.file <- system.file('extdata/Qualis_2017-2020.xlsx',
                             package = 'GetLattesData')

  # set readr cols and read it!
  # my.cols <- readr::cols(
  #   issn    = readr::col_character(),
  #   titulo  = readr::col_character(),
  #   area    = readr::col_character(),
  #   ranking = readr::col_character()
  # )

  my.cols <- c("issn", "titulo", "area", "ranking")

  df.qualis <- readxl::read_excel(qualis.file, col_names = my.cols)

  # df.qualis <- readr::read_csv(qualis.file,
  #                              locale = readr::locale(encoding = 'Latin1'),
  #                              col_types = my.cols,
  #                              progress = F)

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
#' Latest SJR: 2022 | Latest update: 2023-11-27
#'
#' @noRd
gld_get_SJR <- function(){

  # get file
  sjr.file <- system.file('extdata/fixed_scimagojr_2022_csv.csv', package = 'GetLattesData')

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
  #df.sjr$Issn <- paste0(stringr::str_sub(df.sjr$Issn, 1, 4),
  #                      '-',
  #                      stringr::str_sub(df.sjr$Issn, 5, 8))

  return(df.sjr)
}

#' Downloads data from Lattes
#'
#' @param id Id symbol from Lattes (see gld_get_lattes_data)
#' @inheritParams gld_get_lattes_data
#'
#' @return The name of downloaded zip file
#'
#' @noRd
#' @examples
#'
#' \dontrun{
#' file.out <- gld_download_lattes_files(id = 'K4723925J2')
#' }
#'
gld_download_lattes_files <- function(id, folder.dl = tempdir()) {

  # set link and find id cnpq

  my.html <- readLines(paste0('http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=', id))

  temp <- my.html[stringr::str_detect(my.html, 'idcnpq=')]

  cnpq.id <- stringr::str_match_all(temp[1], pattern = 'idcnpq=(\\d+)')[[1]][1,2]

  base.link <- 'http://buscatextual.cnpq.br/buscatextual/download.do?metodo=apresentar&idcnpq='
  my.link <- paste0(base.link,cnpq.id)

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


#' Fetches attributes of child nodes of a single parent node into one row
#'
#' Missing children are simply skipped (instead of being column-bound as empty
#' tibbles, which would drop the row via `dplyr::bind_cols`).
#'
#' @param node A single xml node
#' @param xpaths Character vector of xpaths of the child nodes
#'
#' @return A one-row tibble (or an empty tibble when no child has attributes)
#' @noRd
fetch_node_df <- function(node, xpaths) {

  parts <- lapply(xpaths, function(xp) fetch_df(node, xp))

  # keep only parts that actually have columns
  parts <- parts[vapply(parts, ncol, integer(1)) > 0]

  if (length(parts) == 0) return(tibble::tibble())

  dplyr::bind_cols(parts)
}

#' Parses a set of paper nodes (published or accepted) into a tibble
#'
#' Iterating over each parent node (instead of fetching the basic and detail
#' node sets separately and column-binding them) avoids silently misaligning
#' rows when a record is missing one of its sub-nodes.
#'
#' @param my_xml A xml document (xml2)
#' @param parent_tag Tag of the parent node, e.g. 'ARTIGO-PUBLICADO'
#'
#' @return A tibble
#' @noRd
parse_papers <- function(my_xml, parent_tag) {

  nodes <- xml2::xml_find_all(my_xml, paste0(".//", parent_tag))

  if (length(nodes) == 0) return(tibble::tibble())

  papers <- dplyr::bind_rows(
    lapply(nodes, function(node) {
      fetch_node_df(
        node,
        c(".//DADOS-BASICOS-DO-ARTIGO", ".//DETALHAMENTO-DO-ARTIGO")
      )
    })
  )

  return(papers)
}

#' Finds the row index of an ISSN in the SJR table
#'
#' Handles empty ISSNs and journals with multiple ISSNs. Returns one index
#' (or NA) per element of `issn.vec`.
#'
#' @param issn.vec Vector of ISSNs (e.g. '1234-5678')
#' @param df.sjr The SJR table (from gld_get_SJR)
#'
#' @return An integer vector with the matched row of df.sjr (NA if not found)
#' @noRd
match_sjr_idx <- function(issn.vec, df.sjr) {

  if (length(issn.vec) == 0) return(integer(0))

  idx <- vapply(
    stringr::str_replace_all(issn.vec, "-", ""),
    function(issn.in, df.sjr) {
      issn.in <- stringr::str_trim(issn.in)

      if (is.na(issn.in) || issn.in == '') return(NA_integer_)

      temp.idx <- which(stringr::str_detect(df.sjr$Issn, issn.in))

      if (length(temp.idx) == 0) return(NA_integer_)

      return(as.integer(temp.idx[1]))
    },
    FUN.VALUE = integer(1),
    df.sjr = df.sjr,
    USE.NAMES = FALSE
  )

  return(idx)
}

parse_at_prof <- function(l_in) {

  if (is.null(l_in$VINCULOS)) return(dplyr::tibble())

  my_df <- dplyr::bind_cols(
    dplyr::as_tibble(t(l_in$VINCULOS)),
    dplyr::as_tibble(t(l_in$.attrs))
  )

  names(my_df)

  cols_to_keep <- c("NOME-INSTITUICAO","ENQUADRAMENTO-FUNCIONAL" , "FLAG-DEDICACAO-EXCLUSIVA" ,
                    "ANO-INICIO", "ANO-FIM" )

  my_df <- my_df[ , cols_to_keep]

  return(my_df)

}
