#' Extracts information about papers from xml data
#'
#' @param l.papers A list with papers information
#' @param name.author Name of author
#' @param id.author ID of author
#'
#' @return A dataframe with several information about papers (published or accepted)
#'
#' @export
#'
#' @examples
#'
#' \dontrun{# INTERNAL USE
#'  tpublich <- gld.get.papers.info(i.papers, name.author, id.author)
#' }
#'
gld.get.papers.info <- function(l.papers, name.author, id.author) {

  data.public <- data.frame()

  # return empty df for NULL data.public
  if (is.null(l.papers)) return(data.public)

  # get data
  n.papers <- length(l.papers)

  for (i.l in l.papers){

    info <- do.call(c,list(i.l$`DADOS-BASICOS-DO-ARTIGO`,
                           i.l$`DETALHAMENTO-DO-ARTIGO`))

    # find order of authorship

    idx <- names(i.l) == 'AUTORES'
    coauthors <- do.call(rbind, i.l[idx])

    idx <- stringdist::amatch(as.character(name.author),
                              as.character(coauthors[, 1]),
                              maxDist = Inf)

    if ( (length(idx)!=0)&(!is.na(idx) ) ){
      info['order.aut'] <- unlist(coauthors[idx, 3])
      info['n.aut'] <- nrow(coauthors)

    } else {
      info['order.aut'] <- NA
      info['n.aut'] <- NA

    }

    # supress warnings (change of col classes)
    suppressWarnings(
      data.public <- dplyr::bind_rows(data.public, data.frame(t(info)) )
    )

  }

  if (n.papers!=0){
    data.public$id.file <- rep(id.author, n.papers)
    data.public$name <- name.author

    cols.to.keep <- c('id.file', 'name','TITULO.DO.ARTIGO','ANO.DO.ARTIGO','IDIOMA',
                      "TITULO.DO.PERIODICO.OU.REVISTA",
                      'PAIS-DE-PUBLICACAO','ISSN',
                      'order.aut', 'n.aut')


    idx <- cols.to.keep %in% names(data.public)
    data.public <- data.public[ , cols.to.keep[idx]]

    names(data.public) <- c('id.file', 'name', 'article.title', 'year', 'language','journal.title', 'contry.publication',
                            'ISSN', 'order.aut', 'n.authors')[idx]

    # fix ISSN
    data.public$ISSN <- paste0(stringr::str_sub(data.public$ISSN, 1,4),
                                   '-',
                                   stringr::str_sub(data.public$ISSN, 5,8) )

  }

  return(data.public)

}
