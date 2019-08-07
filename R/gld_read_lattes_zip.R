#' Reads xml data from lattes zip file
#'
#' @param zip.in A (single) name of zip file containing a xml file
#'
#' @return A list with the following items:
#'  \describe{
#'   \item{tpesq}{A dataframe with information about researchers}
#'   \item{tpublic}{A dataframe with information about publications}
#' }
#' @exportsdaQ
#' @examples
#'
#' f.in <- system.file('extdata/3262699324398819.zip', package = 'GetLattesData')
#' my.l <- gld_read_zip(f.in)
#' my.l
gld_read_zip <- function(zip.in){

  # error checking

  if (length(zip.in)>1) {
    stop('Function gld_read_zip  only reads one zip file at a time..')
  }

  if (!file.exists(zip.in)) {
    stop('File ', zip.in, ' does not exists..')
  }

  cat('\nReading ', basename(zip.in))

  # set temp dir for unzipping files
  my.tempdir <- tempdir()
  utils::unzip(zip.in, exdir = my.tempdir)

  # check enconding of file (pick highest prob)
  # DOESNT WORK (leave it for reference)
  #my.encoding <- stringi::stri_enc_detect(file.path(my.tempdir, 'curriculo.xml'))
  #my.encoding <- my.encoding[[1]]$Encoding[1]

  # start reading files using XML
  my.encoding <- 'ISO-8859-1'
  my.l <- XML::xmlToList(XML::xmlParse(file.path(my.tempdir, 'curriculo.xml'),
                                       encoding = my.encoding) )

  my.l$`DADOS-GERAIS`$`FORMACAO-ACADEMICA-TITULACAO`$GRADUACAO

  # Do RESEARCHERS
  LATTES.LOG <- do.call(c,list(my.l$.attrs))

  if (!is.list(my.l$`DADOS-GERAIS`$`FORMACAO-ACADEMICA-TITULACAO`$DOUTORADO)) {
    DOUTORADO <- do.call(c,list(my.l$`DADOS-GERAIS`$`FORMACAO-ACADEMICA-TITULACAO`$DOUTORADO))
  } else {
    DOUTORADO <- do.call(c,list(my.l$`DADOS-GERAIS`$`FORMACAO-ACADEMICA-TITULACAO`$DOUTORADO$.attrs))
  }

  if (!is.list(my.l$`DADOS-GERAIS`$`FORMACAO-ACADEMICA-TITULACAO`$MESTRADO)) {
    MESTRADO <- do.call(c,list(my.l$`DADOS-GERAIS`$`FORMACAO-ACADEMICA-TITULACAO`$MESTRADO))
  } else {
    MESTRADO <- do.call(c,list(my.l$`DADOS-GERAIS`$`FORMACAO-ACADEMICA-TITULACAO`$MESTRADO$.attrs))
  }

  if (!is.list(my.l$`DADOS-GERAIS`$`FORMACAO-ACADEMICA-TITULACAO`$GRADUACAO)) {
    GRAD <- do.call(c,list(my.l$`DADOS-GERAIS`$`FORMACAO-ACADEMICA-TITULACAO`$GRADUACAO))
  } else {
    GRAD <- do.call(c,list(my.l$`DADOS-GERAIS`$`FORMACAO-ACADEMICA-TITULACAO`$GRADUACAO$.attrs))
  }

  #fix names
  names(MESTRADO) <- paste0('MSC-', names(MESTRADO))
  names(DOUTORADO) <- paste0('DOC-', names(DOUTORADO))
  names(GRAD) <- paste0('GRAD-', names(GRAD))

  DADOS.GERAIS <- do.call(c, list(my.l$`DADOS-GERAIS`$.attrs))
  AREAS <- do.call(c, list(my.l$`DADOS-GERAIS`$`AREAS-DE-ATUACAO`))

  if (is.null(DOUTORADO)) DOUTORADO <- c(NO.DOC = TRUE)
  if (is.null(MESTRADO)) MESTRADO <- c(NO.MSC = TRUE)
  if (is.null(GRAD)) GRAD <- c(NO.GRAD = TRUE)

  GArea <- my.l$`DADOS-GERAIS`$`AREAS-DE-ATUACAO`[[1]][2]
  AArea <- my.l$`DADOS-GERAIS`$`AREAS-DE-ATUACAO`[[1]][3]

  # fix for GArea and AArea

  if (is.null(GArea)) GArea <- NA
  if (is.null(AArea)) AArea <- NA

  data.tpesq <- cbind(data.frame(t(LATTES.LOG)),
                      data.frame(t(GRAD)),
                      data.frame(t(MESTRADO)),
                      data.frame(t(DOUTORADO)),
                      data.frame(t(DADOS.GERAIS)),
                      data.frame(GArea = GArea),
                      data.frame(AArea= AArea))

  # all cols (just for reference)
  cols.to.keep <- c("NOME.COMPLETO" ,"NUMERO.IDENTIFICADOR","DATA.ATUALIZACAO","CODIGO.INSTITUICAO","NOME.INSTITUICAO" ,
                    "ANO.DE.INICIO","ANO.DE.CONCLUSAO","FLAG.BOLSA","NOME.COMPLETO.DO.ORIENTADOR" ,
                    "NUMERO.ID.ORIENTADOR", "CODIGO.INSTITUICAO.DOUT", "NOME.INSTITUICAO.DOUT", "PAIS.DE.NACIONALIDADE",
                    "CODIGO.INSTITUICAO.SANDUICHE","PERMISSAO.DE.DIVULGACAO",'GArea','AArea')

  # those to keep
  cols.to.keep <- c("NOME.COMPLETO" ,"DATA.ATUALIZACAO",
                    "GRAD.NOME.INSTITUICAO", "GRAD.ANO.DE.INICIO", "GRAD.ANO.DE.CONCLUSAO", "GRAD.NOME.CURSO",
                    "MSC.NOME.INSTITUICAO","MSC.ANO.DE.INICIO", "MSC.ANO.DE.CONCLUSAO",
                    "DOC.NOME.INSTITUICAO" ,"DOC.ANO.DE.INICIO","DOC.ANO.DE.CONCLUSAO",
                    "PAIS.DE.NACIONALIDADE", 'GArea','AArea')

  # set cols to change name
  better.names <- c('name', 'last.update',
                    'bsc.institution', 'bsc.start.year', 'bsc.end.year', 'bsc.course',
                    'msc.institution', 'msc.start.year', 'msc.end.year',
                    'phd.institution', 'phd.start.year', 'phd.end.year',
                    'country.origin', 'major.field', 'minor.field')


  idx <- cols.to.keep %in% names(data.tpesq)
  data.tpesq <- data.tpesq[, cols.to.keep[idx]]

  # fix names to eng
  names(data.tpesq) <- better.names[idx]

  # clean data
  data.tpesq$last.update <- as.Date(data.tpesq$last.update, '%d%m%Y')
  rownames(data.tpesq) <- NULL
  data.tpesq$id.file <- basename(zip.in)

  # fix issue with no PhD
  if (is.null(data.tpesq$phd.institution)){
    data.tpesq$phd.institution <- NA
    data.tpesq$phd.start.year <- NA
    data.tpesq$phd.end.year <- NA
  }

  # PUBLISHED PAPERS
  my.name <- as.character(data.tpesq$name)
  Encoding(my.name) <- 'UTF-8'
  cat(' - ', my.name)

  published.papers <- my.l$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-PUBLICADOS`

  data.tpublic.published <- gld.get.papers.info(published.papers, name.author = data.tpesq$name,
                                         id.author = basename(zip.in))

  cat(paste0('\n\tFound ',nrow(data.tpublic.published), ' published papers'))

  # ACCEPTED PAPERS
  accpt.papers <- my.l$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-ACEITOS-PARA-PUBLICACAO`

  data.tpublic.accepted <- gld.get.papers.info(accpt.papers, name.author = data.tpesq$name,
                                         id.author = basename(zip.in))


  cat(paste0('\n\tFound ', nrow(data.tpublic.accepted), ' accepted paper(s)'))

  # SUPERVISIONS
  ORIENTACOES <- my.l$`OUTRA-PRODUCAO`$`ORIENTACOES-CONCLUIDAS`
  ORIENTACOES.active <- my.l$`DADOS-COMPLEMENTARES`$`ORIENTACOES-EM-ANDAMENTO`

  cat(paste0('\n\tFound ', length(ORIENTACOES), ' supervisions'))

  data.supervisions <- data.frame()
  if (!is.null(ORIENTACOES)) {

    for (i.orient in ORIENTACOES) {
      i.orient[[1]]
      i.orient[[2]]
      course <- i.orient[[1]]['NATUREZA']
      type.course <- i.orient[[1]]['TIPO']
      std.name <- i.orient[[2]]['NOME-DO-ORIENTADO']
      year.supervision <- as.numeric(i.orient[[1]]['ANO'])

      temp.df <- data.frame(id.file = basename(zip.in),
                            name = data.tpesq$name,
                            situation = 'CONCLUIDA',
                            type.course,
                            course,
                            std.name,
                            year.supervision)

      rownames(temp.df) <-  NULL

      data.supervisions <- rbind(data.supervisions, temp.df)

    }
  }

  data.supervisions.active <- data.frame()
  if (!is.null(ORIENTACOES.active)) {

    for (i.orient in ORIENTACOES.active) {

      course <- i.orient[[1]]['NATUREZA']
      type.course <- i.orient[[1]]['TIPO']
      std.name <- i.orient[[2]]['NOME-DO-ORIENTANDO']
      year.supervision <- as.numeric(i.orient[[1]]['ANO'])

      temp.df <- data.frame(id.file = basename(zip.in),
                            name = data.tpesq$name,
                            situation = 'EM ANDAMENTO',
                            type.course,
                            course,
                            std.name,
                            year.supervision)

      rownames(temp.df) <-  NULL

      data.supervisions.active <- rbind(data.supervisions.active, temp.df)

    }

    data.supervisions <- rbind(data.supervisions, data.supervisions.active)


  }

  # books
  LIVROS.PUBLICADOS <- my.l$`PRODUCAO-BIBLIOGRAFICA`$`LIVROS-E-CAPITULOS`$`LIVROS-PUBLICADOS-OU-ORGANIZADOS`

  cat(paste0('\n\tFound ',length(LIVROS.PUBLICADOS), ' published books'))

  data.books.published <- data.frame()
  if (!is.null(LIVROS.PUBLICADOS)) {

    for (i.book in LIVROS.PUBLICADOS) {

      temp.df <- data.frame(id.file = basename(zip.in),
                            name = data.tpesq$name,
                            book.title = i.book$`DADOS-BASICOS-DO-LIVRO`['TITULO-DO-LIVRO'],
                            book.year = i.book$`DADOS-BASICOS-DO-LIVRO`['ANO'],
                            book.type = i.book$`DADOS-BASICOS-DO-LIVRO`['TIPO'],
                            book.lan  = i.book$`DADOS-BASICOS-DO-LIVRO`['IDIOMA'],
                            book.issn = i.book$`DETALHAMENTO-DO-LIVRO`['ISBN'],
                            book.npages = i.book$`DETALHAMENTO-DO-LIVRO`['NUMERO-DE-PAGINAS'],
                            book.edition = i.book$`DETALHAMENTO-DO-LIVRO`['NUMERO-DA-EDICAO-REVISAO'],
                            book.editor = i.book$`DETALHAMENTO-DO-LIVRO`['NOME-DA-EDITORA'], stringsAsFactors = F)

      rownames(temp.df) <-  NULL

      data.books.published <- rbind(data.books.published, temp.df)

    }
  }

  # books chapters
  LIVROS.CAPITULOS <- my.l$`PRODUCAO-BIBLIOGRAFICA`$`LIVROS-E-CAPITULOS`$`CAPITULOS-DE-LIVROS-PUBLICADOS`

  cat(paste0('\n\tFound ',length(LIVROS.CAPITULOS), ' book chapters'))

  data.books.chapters <- data.frame()
  if (!is.null(LIVROS.CAPITULOS)) {

    for (i.book in LIVROS.CAPITULOS) {

      temp.df <- data.frame(id.file = basename(zip.in),
                            name = data.tpesq$name,
                            book.title = i.book$`DETALHAMENTO-DO-CAPITULO`['TITULO-DO-LIVRO'],
                            book.chapter = i.book$`DADOS-BASICOS-DO-CAPITULO`['TITULO-DO-CAPITULO-DO-LIVRO'],
                            book.year  = i.book$`DADOS-BASICOS-DO-CAPITULO`['ANO'],
                            book.type  = i.book$`DADOS-BASICOS-DO-CAPITULO`['TIPO'],
                            book.lan   = i.book$`DADOS-BASICOS-DO-CAPITULO`['IDIOMA'],
                            book.issn = i.book$`DETALHAMENTO-DO-CAPITULO`['ISBN'],
                            book.edition = i.book$`DETALHAMENTO-DO-CAPITULO`['NUMERO-DA-EDICAO-REVISAO'],
                            book.editor = i.book$`DETALHAMENTO-DO-CAPITULO`['NOME-DA-EDITORA'], stringsAsFactors = F)

      rownames(temp.df) <-  NULL

      data.books.chapters <- rbind(data.books.chapters, temp.df)

    }
  }

  data.books <- dplyr::bind_rows(data.books.published, data.books.chapters)


  # conferences

  CONFERENCES <- my.l$`PRODUCAO-BIBLIOGRAFICA`$`TRABALHOS-EM-EVENTOS`

  cat(paste0('\n\tFound ',length(CONFERENCES), ' conference papers'))

  data.conferences <- data.frame()
  if (!is.null(CONFERENCES)) {

    for (i.conf in CONFERENCES) {

      temp.df <- data.frame(id.file = basename(zip.in),
                            name = data.tpesq$name,
                            article.title = i.conf$`DADOS-BASICOS-DO-TRABALHO`['TITULO-DO-TRABALHO'],
                            article.year = i.conf$`DADOS-BASICOS-DO-TRABALHO`['ANO-DO-TRABALHO'],
                            event.classification = i.conf$`DETALHAMENTO-DO-TRABALHO`['CLASSIFICACAO-DO-EVENTO'],
                            event.name = i.conf$`DETALHAMENTO-DO-TRABALHO`['NOME-DO-EVENTO'],
                            event.isbn = i.conf$`DETALHAMENTO-DO-TRABALHO`['ISBN'],
                            event.city = i.conf$`DETALHAMENTO-DO-TRABALHO`['CIDADE-DO-EVENTO'],
                            stringsAsFactors = F)

      rownames(temp.df) <-  NULL

      data.conferences <- rbind(data.conferences, temp.df)

    }
  }

  # output

  my.l <- list(tpesq = data.tpesq,
               tpublic.published = data.tpublic.published,
               tpublic.accepted = data.tpublic.accepted,
               tsupervisions = data.supervisions,
               tbooks = data.books,
               tconferences = data.conferences)

  return(my.l)

}
