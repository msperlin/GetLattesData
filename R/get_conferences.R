get_conferences <- function(my_xml) {

  all_conf <- xml2::xml_find_all(my_xml, ".//APRESENTACAO-DE-TRABALHO")

  if (length(all_conf) == 0) return(tibble::tibble())

  df_conf <- dplyr::bind_rows(
    lapply(all_conf, function(node) {
      fetch_node_df(
        node,
        c(
          ".//DADOS-BASICOS-DA-APRESENTACAO-DE-TRABALHO",
          ".//DETALHAMENTO-DA-APRESENTACAO-DE-TRABALHO"
        )
      )
    })
  )

  return(df_conf)
}
