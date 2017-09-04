#' Reads xml data from lattes zip file
#'
#' @param zip.in A (single) name of zip file containing a xml file
#'
#' @return A list with the following items:
#'  \describe{
#'   \item{tpesq}{A dataframe with information about researchers}
#'   \item{tpublic}{A dataframe with information about publications}
#' }
#' @export
#'
#' @examples
#'
#' f.in <- system.file('extdata/K4440252H7_2017-09-04.zip', package = 'GetLattesData')
#' my.l <- gld_read_zip(f.in)
#' my.l
gld_read_zip <- function(zip.in){

  # error checking

  if (length(zip.in)>1) {
    stop('Function gld_read_zip  only reads one zip file at a time..')
  }

  if (!file.exists(zip.in)) {
    stop('File ', zip.in, 'does not exists..')
  }

  cat('\nReading ', basename(zip.in))

  # set temp dir for unzipping files
  my.tempdir <- tempdir()
  utils::unzip(zip.in, exdir = my.tempdir)

  # start reading files using XML
  my.l <- XML::xmlToList(XML::xmlParse(file.path(my.tempdir, 'curriculo.xml')))

  # Do RESEARCHERS

  LATTES.LOG <- do.call(c,list(my.l$.attrs))
  DOUTORADO <- do.call(c,list(my.l$`DADOS-GERAIS`$`FORMACAO-ACADEMICA-TITULACAO`$DOUTORADO$.attrs))
  DADOS.GERAIS <- do.call(c, list(my.l$`DADOS-GERAIS`$.attrs))
  AREAS <- do.call(c, list(my.l$`DADOS-GERAIS`$`AREAS-DE-ATUACAO`))

  if (is.null(DOUTORADO)) DOUTORADO <- c(NO.DOC=TRUE)

  GArea <- my.l$`DADOS-GERAIS`$`AREAS-DE-ATUACAO`[[1]][2]
  AArea <- my.l$`DADOS-GERAIS`$`AREAS-DE-ATUACAO`[[1]][3]

  # fix for GArea and AArea

  if (is.null(GArea)) GArea <- NA
  if (is.null(AArea)) AArea <- NA

  data.tpesq <- cbind(data.frame(t(LATTES.LOG)),
                      data.frame(t(DOUTORADO)),
                      data.frame(t(DADOS.GERAIS)),
                      data.frame(GArea = GArea),
                      data.frame(AArea= AArea))

  # all cols
  cols.to.keep <- c("NOME.COMPLETO" ,"NUMERO.IDENTIFICADOR","DATA.ATUALIZACAO","CODIGO.INSTITUICAO","NOME.INSTITUICAO" ,
                    "ANO.DE.INICIO","ANO.DE.CONCLUSAO","FLAG.BOLSA","NOME.COMPLETO.DO.ORIENTADOR" ,
                    "NUMERO.ID.ORIENTADOR", "CODIGO.INSTITUICAO.DOUT", "NOME.INSTITUICAO.DOUT", "PAIS.DE.NACIONALIDADE",
                    "CODIGO.INSTITUICAO.SANDUICHE","PERMISSAO.DE.DIVULGACAO",'GArea','AArea')

  # those to keep
  cols.to.keep <- c("NOME.COMPLETO" ,"DATA.ATUALIZACAO", "NOME.INSTITUICAO" ,
                    "ANO.DE.INICIO","ANO.DE.CONCLUSAO",  "PAIS.DE.NACIONALIDADE",
                    'GArea','AArea')


  idx <- cols.to.keep %in% names(data.tpesq)
  data.tpesq <- data.tpesq[, cols.to.keep[idx]]

  # fix names to eng
  names(data.tpesq) <- c('name', 'last.update', 'phd.institution', 'phd.start.year', 'phd.end.year',
                         'country.origin', 'Major Field', 'Minor Field')

  # clean data
  data.tpesq$last.update <- as.Date(data.tpesq$last.update, '%d%m%Y')
  rownames(data.tpesq) <- NULL
  data.tpesq$id.file <- basename(zip.in)

  # PAPERS
  cat(' - ', as.character(data.tpesq$name) )

  papers <- my.l$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-PUBLICADOS`

  data.tpublic <- data.frame()
  if (!is.null(papers)) { # if data is found for papers

    n.papers <- length(papers)

    for (i.l in papers){

      info <- do.call(c,list(i.l$`DADOS-BASICOS-DO-ARTIGO`,
                             i.l$`DETALHAMENTO-DO-ARTIGO`))

      # find order of authorship

      idx <- names(i.l) == 'AUTORES'
      coauthors <- do.call(rbind, i.l[idx])

      idx <- stringdist::amatch(as.character(data.tpesq$name),
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
        data.tpublic <- dplyr::bind_rows(data.tpublic, data.frame(t(info)) )
      )

    }


    if (n.papers!=0){
      data.tpublic$id <- rep(basename(zip.in), n.papers)
      data.tpublic$name <- data.tpesq$name

      cols.to.keep <- c('name','TITULO.DO.ARTIGO','ANO.DO.ARTIGO','IDIOMA',
                        "TITULO.DO.PERIODICO.OU.REVISTA",
                        'PAIS-DE-PUBLICACAO','ISSN', 'PAGINA.INICIAL', 'PAGINA.FINAL',
                        'order.aut', 'n.aut')


      idx <- cols.to.keep %in% names(data.tpublic)
      data.tpublic <- data.tpublic[ , cols.to.keep[idx]]

      names(data.tpublic) <- c('name', 'article.title', 'year', 'language','journal.title', 'contry.publication',
                               'ISSN', 'start.page', 'end.page', 'order.aut', 'n.authors')[idx]

    }

  }

  cat('\t found',n.papers, ' papers')
  # fix issn

  data.tpublic$ISSN <- paste0(stringr::str_sub(data.tpublic$ISSN, 1,4),
                              '-',
                              stringr::str_sub(data.tpublic$ISSN, 5,8) )

  # output
  my.l <- list(tpesq = data.tpesq,
               tpublic=data.tpublic)
  return(my.l)

}
