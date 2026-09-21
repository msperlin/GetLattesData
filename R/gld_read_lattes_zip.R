#' Reads xml data from lattes zip file
#'
#' @param zip.in A (single) name of zip file containing a xml file
#'
#' @return A list with the following items:
#'  \describe{
#'   \item{tpesq}{A dataframe with information about researchers}
#'   \item{tpublic}{A dataframe with information about publications}
#' }
#' @noRd
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
  if (is.null(DOUTORADO)) DOUTORADO <- c(NO.DOC = TRUE)
  if (is.null(MESTRADO)) MESTRADO <- c(NO.MSC = TRUE)
  if (is.null(GRAD)) GRAD <- c(NO.GRAD = TRUE)

  names(MESTRADO) <- paste0('MSC-', names(MESTRADO))
  names(DOUTORADO) <- paste0('DOC-', names(DOUTORADO))
  names(GRAD) <- paste0('GRAD-', names(GRAD))

  DADOS.GERAIS <- do.call(c, list(my.l$`DADOS-GERAIS`$.attrs))
  AREAS <- do.call(c, list(my.l$`DADOS-GERAIS`$`AREAS-DE-ATUACAO`))

  GArea <- my.l$`DADOS-GERAIS`$`AREAS-DE-ATUACAO`[[1]][2]
  AArea <- my.l$`DADOS-GERAIS`$`AREAS-DE-ATUACAO`[[1]][3]

  # fix for GArea and AArea
  if (is.null(GArea)) GArea <- NA
  if (is.null(AArea)) AArea <- NA

  data.tpesq <- dplyr::bind_cols(dplyr::as_tibble(t(LATTES.LOG)),
                      dplyr::as_tibble(t(GRAD)),
                      dplyr::as_tibble(t(MESTRADO)),
                      dplyr::as_tibble(t(DOUTORADO)),
                      dplyr::as_tibble(t(DADOS.GERAIS)),
                      dplyr::tibble(GArea = GArea),
                      dplyr::tibble(AArea= AArea) )

  # all cols (just for reference)
  cols.to.keep <- c("NOME-COMPLETO" ,"NUMERO-IDENTIFICADOR","DATA-ATUALIZACAO","CODIGO-INSTITUICAO","NOME-INSTITUICAO" ,
                    "ANO-DE-INICIO","ANO-DE-CONCLUSAO","FLAG-BOLSA","NOME-COMPLETO-DO-ORIENTADOR" ,
                    "NUMERO-ID-ORIENTADOR", "CODIGO-INSTITUICAO-DOUT", "NOME-INSTITUICAO-DOUT", "PAIS-DE-NACIONALIDADE",
                    "CODIGO-INSTITUICAO-SANDUICHE","PERMISSAO-DE-DIVULGACAO",'GArea','AArea')

  # those to keep
  cols.to.keep <- c("NOME-COMPLETO" ,"NOME-EM-CITACOES-BIBLIOGRAFICAS", "DATA-ATUALIZACAO",
                    "GRAD-NOME-INSTITUICAO", "GRAD-ANO-DE-INICIO", "GRAD-ANO-DE-CONCLUSAO", "GRAD-NOME-CURSO",
                    "MSC-NOME-INSTITUICAO","MSC-ANO-DE-INICIO", "MSC-ANO-DE-CONCLUSAO",
                    "DOC-NOME-INSTITUICAO" ,"DOC-ANO-DE-INICIO","DOC-ANO-DE-CONCLUSAO",
                    "PAIS-DE-NACIONALIDADE", 'GArea','AArea')

  # set cols to change name
  better.names <- c('name', "name_in_citations", 'last.update',
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

  last_update <- lubridate::dmy(my.l$.attrs["DATA-ATUALIZACAO"])
  data.tpesq$last_update <- last_update

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

  data.tpublic.published <- gld.get.papers.info(published.papers,
                                                name.author = data.tpesq$name,
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

  data.supervisions <- dplyr::tibble()
  if (!is.null(ORIENTACOES)) {

    for (i.orient in ORIENTACOES) {
      i.orient[[1]]
      i.orient[[2]]
      course <- i.orient[[1]]['NATUREZA']
      type.course <- i.orient[[1]]['TIPO']
      std.name <- i.orient[[2]]['NOME-DO-ORIENTADO']
      year.supervision <- as.numeric(i.orient[[1]]['ANO'])
      type_supervision <- as.character(i.orient[[2]]["TIPO-DE-ORIENTACAO"])

      temp.df <- dplyr::tibble(id.file = basename(zip.in),
                            name = data.tpesq$name,
                            situation = 'CONCLUIDA',
                            type.course,
                            course,
                            std.name,
                            year.supervision,
                            type_supervision)

      rownames(temp.df) <-  NULL

      data.supervisions <- rbind(data.supervisions, temp.df)

    }
  }

  data.supervisions.active <- dplyr::tibble()
  if (!is.null(ORIENTACOES.active)) {

    for (i.orient in ORIENTACOES.active) {

      course <- i.orient[[1]]['NATUREZA']
      type.course <- i.orient[[1]]['TIPO']
      std.name <- i.orient[[2]]['NOME-DO-ORIENTANDO']
      year.supervision <- as.numeric(i.orient[[1]]['ANO'])
      type_supervision <- as.character(i.orient[[2]]["TIPO-DE-ORIENTACAO"])

      temp.df <- dplyr::tibble(id.file = basename(zip.in),
                            name = data.tpesq$name,
                            situation = 'EM ANDAMENTO',
                            type.course,
                            course,
                            std.name,
                            year.supervision,
                            type_supervision)

      rownames(temp.df) <-  NULL

      data.supervisions.active <- rbind(data.supervisions.active, temp.df)

    }

    data.supervisions <- rbind(data.supervisions, data.supervisions.active)


  }

  # books
  LIVROS.PUBLICADOS <- my.l$`PRODUCAO-BIBLIOGRAFICA`$`LIVROS-E-CAPITULOS`$`LIVROS-PUBLICADOS-OU-ORGANIZADOS`

  cat(paste0('\n\tFound ',length(LIVROS.PUBLICADOS), ' published books'))

  data.books.published <- dplyr::tibble()
  if (!is.null(LIVROS.PUBLICADOS)) {

    for (i.book in LIVROS.PUBLICADOS) {

      temp.df <- dplyr::tibble(id.file = basename(zip.in),
                            name = data.tpesq$name,
                            book.title = i.book$`DADOS-BASICOS-DO-LIVRO`['TITULO-DO-LIVRO'],
                            book.year = i.book$`DADOS-BASICOS-DO-LIVRO`['ANO'],
                            book.type = i.book$`DADOS-BASICOS-DO-LIVRO`['TIPO'],
                            book.lan  = i.book$`DADOS-BASICOS-DO-LIVRO`['IDIOMA'],
                            book.issn = i.book$`DETALHAMENTO-DO-LIVRO`['ISBN'],
                            book.npages = i.book$`DETALHAMENTO-DO-LIVRO`['NUMERO-DE-PAGINAS'],
                            book.edition = i.book$`DETALHAMENTO-DO-LIVRO`['NUMERO-DA-EDICAO-REVISAO'],
                            book.editor = i.book$`DETALHAMENTO-DO-LIVRO`['NOME-DA-EDITORA'])

      rownames(temp.df) <-  NULL

      data.books.published <- rbind(data.books.published, temp.df)

    }
  }

  # books chapters
  LIVROS.CAPITULOS <- my.l$`PRODUCAO-BIBLIOGRAFICA`$`LIVROS-E-CAPITULOS`$`CAPITULOS-DE-LIVROS-PUBLICADOS`

  cat(paste0('\n\tFound ',length(LIVROS.CAPITULOS), ' book chapters'))

  data.books.chapters <- dplyr::tibble()
  if (!is.null(LIVROS.CAPITULOS)) {

    for (i.book in LIVROS.CAPITULOS) {

      temp.df <- dplyr::tibble(id.file = basename(zip.in),
                            name = data.tpesq$name,
                            book.title = i.book$`DETALHAMENTO-DO-CAPITULO`['TITULO-DO-LIVRO'],
                            book.chapter = i.book$`DADOS-BASICOS-DO-CAPITULO`['TITULO-DO-CAPITULO-DO-LIVRO'],
                            book.year  = i.book$`DADOS-BASICOS-DO-CAPITULO`['ANO'],
                            book.type  = i.book$`DADOS-BASICOS-DO-CAPITULO`['TIPO'],
                            book.lan   = i.book$`DADOS-BASICOS-DO-CAPITULO`['IDIOMA'],
                            book.issn = i.book$`DETALHAMENTO-DO-CAPITULO`['ISBN'],
                            book.edition = i.book$`DETALHAMENTO-DO-CAPITULO`['NUMERO-DA-EDICAO-REVISAO'],
                            book.editor = i.book$`DETALHAMENTO-DO-CAPITULO`['NOME-DA-EDITORA'])

      rownames(temp.df) <-  NULL

      data.books.chapters <- rbind(data.books.chapters, temp.df)

    }
  }

  data.books <- dplyr::bind_rows(data.books.published, data.books.chapters)

  # conferences
  CONFERENCES <- my.l$`PRODUCAO-BIBLIOGRAFICA`$`TRABALHOS-EM-EVENTOS`

  cat(paste0('\n\tFound ',length(CONFERENCES), ' conference papers'))

  data.conferences <- dplyr::tibble()
  if (!is.null(CONFERENCES)) {

    for (i.conf in CONFERENCES) {

      temp.df <- dplyr::tibble(id.file = basename(zip.in),
                            name = data.tpesq$name,
                            article.title = i.conf$`DADOS-BASICOS-DO-TRABALHO`['TITULO-DO-TRABALHO'],
                            article.year = i.conf$`DADOS-BASICOS-DO-TRABALHO`['ANO-DO-TRABALHO'],
                            event.classification = i.conf$`DETALHAMENTO-DO-TRABALHO`['CLASSIFICACAO-DO-EVENTO'],
                            event.name = i.conf$`DETALHAMENTO-DO-TRABALHO`['NOME-DO-EVENTO'],
                            event.isbn = i.conf$`DETALHAMENTO-DO-TRABALHO`['ISBN'],
                            event.city = i.conf$`DETALHAMENTO-DO-TRABALHO`['CIDADE-DO-EVENTO'])

      rownames(temp.df) <-  NULL

      data.conferences <- rbind(data.conferences, temp.df)

    }
  }

  # employment
  if (!is.null(my.l$`DADOS-GERAIS`$`ATUACOES-PROFISSIONAIS`)) {

    AT_PROF <- do.call(c, list(my.l$`DADOS-GERAIS`$`ATUACOES-PROFISSIONAIS`))

    df_atprof <- dplyr::bind_rows(
      lapply(AT_PROF, parse_at_prof)
    )

    df_atprof$name <- data.tpesq$name
    df_atprof$id.file <- basename(zip.in)
    #df <- dplyr::as_tibble(t(AT_PROF))
  } else {
    df_atprof <- dplyr::tibble()
  }

  cat(paste0('\n\tFound ', nrow(df_atprof), ' employment registries'))

  # research and extension projects
  my_xml <- xml2::read_xml(zip.in)

  all_proj <- xml2::xml_find_all(my_xml, ".//PROJETO-DE-PESQUISA")

  if (length(all_proj) != 0) {

    l_info <- all_proj |>
      xml2::xml_attrs()

    df_projects <- purrr::map_df(
      l_info,
      function(x) tibble::as_tibble(t(x))
      )

    df_projects$name <- data.tpesq$name
    df_projects$id.file <- basename(zip.in)

    # all_names <- c("SEQUENCIA-PROJETO", "ANO-INICIO", "ANO-FIM", "NOME-DO-PROJETO",
    #                "SITUACAO", "NATUREZA", "NUMERO-GRADUACAO", "NUMERO-ESPECIALIZACAO",
    #                "NUMERO-MESTRADO-ACADEMICO", "NUMERO-MESTRADO-PROF", "NUMERO-DOUTORADO",
    #                "DESCRICAO-DO-PROJETO", "IDENTIFICADOR-PROJETO", "DESCRICAO-DO-PROJETO-INGLES",
    #                "NOME-DO-PROJETO-INGLES", "FLAG-POTENCIAL-INOVACAO", "NOME-COORDENADOR-CERTIFICACAO",
    #                "DATA-CERTIFICACAO", "NUMERO_TECNICO_NIVEL_MEDIO")
    to_keep <- c("name", "id.file", "SEQUENCIA-PROJETO", "ANO-INICIO", "ANO-FIM", "NOME-DO-PROJETO",
                 "SITUACAO", "NATUREZA", "NUMERO-GRADUACAO", "NUMERO-ESPECIALIZACAO",
                 "NUMERO-MESTRADO-ACADEMICO", "NUMERO-MESTRADO-PROF", "NUMERO-DOUTORADO",
                 "IDENTIFICADOR-PROJETO", "FLAG-POTENCIAL-INOVACAO", "NOME-COORDENADOR-CERTIFICACAO",
                 "DATA-CERTIFICACAO", "NUMERO_TECNICO_NIVEL_MEDIO")

    df_projects <- df_projects[, to_keep]

    # fix names
    df_projects <- df_projects |>
      janitor::clean_names()

  } else {
    df_projects <- dplyr::tibble()
  }

  cat(paste0('\n\tFound ', nrow(df_projects), ' projects'))

  # co-authors

  all_papers <- xml2::xml_find_all(my_xml, ".//ARTIGO-PUBLICADO")

  if (length(all_papers) != 0) {

    df_coauthors <- tibble::tibble()
    for (i_node in all_papers) {

      dados_basicos <- xml2::xml_find_all(i_node, ".//DADOS-BASICOS-DO-ARTIGO")

      paper_titulo <- dados_basicos |>
        xml2::xml_attr("TITULO-DO-ARTIGO")
      paper_ano <-  dados_basicos |>
        xml2::xml_attr("ANO-DO-ARTIGO") |>
        as.numeric()

      paper_coauthors <- i_node |>
        xml2::xml_find_all(".//AUTORES") |>
        xml2::xml_attrs()

      df_temp <- purrr::map_df(
        paper_coauthors,
        function(x) tibble::as_tibble(t(x))
      ) |>
        dplyr::mutate(
          title = paper_titulo,
          year = paper_ano
        )

      df_coauthors <- dplyr::bind_rows(
        df_coauthors,
        df_temp
      )

    }

    df_coauthors$name <- data.tpesq$name
    df_coauthors$id.file <- basename(zip.in)

  } else {
    df_coauthors <- tibble::tibble()
  }

  cat(paste0('\n\tFound ', nrow(df_coauthors), ' coauthors'))


  # output
  my.l <- list(tpesq = data.tpesq,
               tpublic.published = data.tpublic.published,
               tpublic.accepted = data.tpublic.accepted,
               tsupervisions = data.supervisions,
               tbooks = data.books,
               tconferences = data.conferences,
               t_df_atprof = df_atprof,
               tprojects = df_projects,
               tcoauthors = df_coauthors)

  return(my.l)

}
