get_coauthors <- function(my_xml) {

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

  return(df_coauthors)
}
