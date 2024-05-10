#' Reads xml data from lattes zip file
#'
#' @param f_zip A (single) name of zip file containing a xml file
#'
#' @return A list with several items
#' @export
#' @examples
#'
#' f_in <- system.file('extdata/3262699324398819.zip', package = 'GetLattesData')
#' my_l <- gld_read_zip2(f_in)
#' my_l
gld_read_zip2 <- function(f_zip){

  # error checking
  if (length(f_zip) > 1) {
    cli::cli_abort('Function gld_read_zip  only reads one zip file at a time..')
  }

  if (tools::file_ext(f_zip) != 'zip') {
    cli::cli_abort("file extension for {f_zip} is not zip!")
  }

  if (!file.exists(f_zip)) {
    cli::cli_abort('File ', f_zip, ' does not exists..')
  }

  my_xml <- xml2::read_xml(f_zip, encoding = "Latin1")

  cli::cli_h1('\nReading lattes file {basename(f_zip)}')

  # cvitae ----
  fetch_df <- function(this_xml, xpath) {
    this_nodes <- xml2::xml_find_all(this_xml, xpath)

    if (length(this_nodes) == 0) {
      df_out <- tibble::tibble()
    } else {
      df_out <- this_nodes |>
        purrr::map_df(
          .f = function(x) tibble::as_tibble(janitor::clean_names(t(xml2::xml_attrs(x))))
        )
    }

    return(df_out)
  }

  cvitae <- fetch_df(my_xml, ".//DADOS-GERAIS")
  extra_info <- fetch_df(my_xml, "//CURRICULO-VITAE")
  areas <- fetch_df(my_xml, ".//AREA-DE-ATUACAO")
  main_area <- areas[1, ] # only keep first area

  cvitae <- cvitae |>
    dplyr::bind_cols(main_area) |>
    dplyr::bind_cols(extra_info)

  cli::cli_alert_success("got general info for {cvitae$nome_completo}")


  # academic background ----
  grad <- fetch_df(my_xml, ".//FORMACAO-ACADEMICA-TITULACAO//GRADUACAO")
  mestrado <-  fetch_df(my_xml, ".//FORMACAO-ACADEMICA-TITULACAO//MESTRADO")
  doutorado <-  fetch_df(my_xml, ".//FORMACAO-ACADEMICA-TITULACAO//DOUTORADO")
  pos_doc <- fetch_df(my_xml, ".//FORMACAO-ACADEMICA-TITULACAO//POS-DOUTORADO")

  cli::cli_alert_success(
    "got information for {nrow(grad)} bsc, {nrow(mestrado)} msc, {nrow(doutorado)} Phd, {nrow(pos_doc)} pos-doc"
  )

  # papers ----
  node_papers <- xml2::xml_find_all(my_xml, ".//ARTIGO-PUBLICADO")

  papers <- tibble::tibble()
  for (i_node  in node_papers) {

    temp_attrs <- xml2::xml_find_all(i_node, "DADOS-BASICOS-DO-ARTIGO")[[1]] |>
      xml2::xml_attrs() |>
      t() |>
      tibble::as_tibble() |>
      janitor::clean_names()

    nodes_detalhes <- xml2::xml_find_all(i_node, "DETALHAMENTO-DO-ARTIGO")[[1]] |>
      xml2::xml_attrs() |>
      t() |>
      tibble::as_tibble() |>
      janitor::clean_names()

    papers <- dplyr::bind_rows(
      papers,
      dplyr::bind_cols(
        temp_attrs,
        nodes_detalhes
      )
    )

  }

  cli::cli_alert_success(
    "got {nrow(papers)} published papers"
  )

  # books
  book_1 <- fetch_df(my_xml, ".//LIVRO-PUBLICADO-OU-ORGANIZADO//DADOS-BASICOS-DO-LIVRO")
  book_2 <- fetch_df(my_xml, ".//LIVRO-PUBLICADO-OU-ORGANIZADO//DETALHAMENTO-DO-LIVRO")

  books <- dplyr::bind_cols(book_1, book_2)

  cli::cli_alert_success(
    "got {nrow(books)} books"
  )

  # supervisions MSC ----
  superv_msc_1 <- fetch_df(
    my_xml,
    ".//ORIENTACOES-CONCLUIDAS//ORIENTACOES-CONCLUIDAS-PARA-MESTRADO//DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO"
  )
  superv_msc_2 <- fetch_df(
    my_xml,
    ".//ORIENTACOES-CONCLUIDAS//ORIENTACOES-CONCLUIDAS-PARA-MESTRADO//DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO"
  )

  superv_msc <- dplyr::bind_cols(
    superv_msc_1,
    superv_msc_2
  )

  # supervisions MSC ----
  superv_phd_1 <- fetch_df(
    my_xml,
    ".//ORIENTACOES-CONCLUIDAS//ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO//DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO"
  )
  superv_phd_2 <- fetch_df(
    my_xml,
    ".//ORIENTACOES-CONCLUIDAS//ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO//DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO"
  )

  superv_phd <- dplyr::bind_cols(
    superv_phd_1,
    superv_phd_2
  )

  superv_others_1 <- fetch_df(
    my_xml,
    ".//ORIENTACOES-CONCLUIDAS//OUTRAS-ORIENTACOES-CONCLUIDAS//DADOS-BASICOS-DE-OUTRAS-ORIENTACOES-CONCLUIDAS"
  )

  superv_others_2 <- fetch_df(
    my_xml,
    ".//ORIENTACOES-CONCLUIDAS//OUTRAS-ORIENTACOES-CONCLUIDAS//DETALHAMENTO-DE-OUTRAS-ORIENTACOES-CONCLUIDAS"
  )

  superv_others <- dplyr::bind_cols(
    superv_others_1,
    superv_others_2
  )

  superv_all <- dplyr::bind_rows(
    superv_others,
    superv_msc,
    superv_phd
  )

  cli::cli_alert_success(
    "got {nrow(superv_all)} supervisions"
  )


  # employment
  all_prof <- xml2::xml_find_all(my_xml,
                                 ".//ATUACOES-PROFISSIONAIS//ATUACAO-PROFISSIONAL")

  at_prof <- tibble::tibble()
  for (i_node in all_prof) {

    temp_attrs <- i_node |>
      xml2::xml_attrs() |>
      t() |>
      tibble::as_tibble() |>
      janitor::clean_names()

    nodes_vinculos <- xml2::xml_find_all(i_node, ".//VINCULOS")

    for (i_vinc in nodes_vinculos) {

      temp_df <- xml2::xml_attrs(i_vinc) |>
        t() |>
        tibble::as_tibble() |>
        janitor::clean_names() |>
        dplyr::bind_cols(temp_attrs)

      at_prof <- dplyr::bind_rows(
        at_prof,
        temp_df
      )
    }

  }

  cli::cli_alert_success(
    "got {nrow(at_prof)} employment/activity registries"
  )

  # research and extension projects
  projs <- fetch_df(my_xml, ".//PROJETO-DE-PESQUISA") |>
    dplyr::select(-dplyr::contains("descricao_do_projeto"))

  cli::cli_alert_success(
    "got {nrow(projs)} projects"
  )

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
        ) |>
        janitor::clean_names()

      df_coauthors <- dplyr::bind_rows(
        df_coauthors,
        df_temp
      )

    }

  } else {
    df_coauthors <- tibble::tibble()
  }

  cli::cli_alert_success(
    "got {nrow(df_coauthors)} coauthors"
  )


  # output
  l_out <- list(
    info = cvitae,
    course_bachelors = grad,
    course_msc = mestrado,
    course_phd = doutorado,
    pos_doc = pos_doc,
    books = books,
    published_papers = papers,
    supervisions = superv_all,
    at_prof = at_prof,
    projects = projs,
    coauthors = df_coauthors
  )

  # parse and fix list output
  fix_df <- function(df_in) {
    this_names <- names(df_in)

    to_numeric <- which(stringr::str_detect(this_names, "ano|numero_|mes_|_pagina"))

    for (i in to_numeric) {
      df_in[[ i]] <- as.numeric(df_in[[i]])
    }

    # add info
    df_in$nome_completo <- l_out$info$nome_completo
    df_in$id_file <- basename(f_zip)

    df_in <- dplyr::relocate(df_in, nome_completo)

    return(df_in)
  }

  l_out <- purrr::map(l_out, fix_df)

  return(l_out)

}
