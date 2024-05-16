get_conferences <- function(my_xml) {

  all_conf <- xml2::xml_find_all(my_xml, ".//APRESENTACAO-DE-TRABALHO")

  if (length(all_conf) != 0) {

    df_conf <- tibble::tibble()

    for (i_node in all_conf) {
      conf_dados_basicos <- fetch_df(i_node, ".//DADOS-BASICOS-DA-APRESENTACAO-DE-TRABALHO")

      conf_detalhes <- fetch_df(i_node, ".//DETALHAMENTO-DA-APRESENTACAO-DE-TRABALHO")

      df_conf <- dplyr::bind_rows(
        df_conf,
        dplyr::bind_cols(
          conf_dados_basicos,
          conf_detalhes
        )
      )
    }

  } else {
    df_conf <- tibble::tibble()
  }

  return(df_conf)
}
