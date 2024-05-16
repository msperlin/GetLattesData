get_superv <- function(my_xml) {

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

  return(superv_all)
}
