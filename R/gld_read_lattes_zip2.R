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

  cli::cli_h1('\nReading lattes file {basename(f_zip)}')

  my_xml <- xml2::read_xml(f_zip, encoding = "Latin1")

  text_xml <- xml2::xml_text(my_xml)

  if (text_xml == "Erro ao recuperar o XML") {
    cli::cli_alert_warning("cant read xml file {f_zip}, no data found. Returning empty list")
    return(list())

  }

  # cvitae ----
  cvitae <- get_cvitae(my_xml)

  cli::cli_alert_success("got general info for {cvitae$nome_completo}")

  # academic background ----
  l_acad_back <- get_acad_back(my_xml)

  cli::cli_alert_success(
    "got information for {nrow(l_acad_back$grad)} bsc, {nrow(l_acad_back$mestrado)} msc, {nrow(l_acad_back$doutorado)} Phd, {nrow(l_acad_back$pos_doc)} pos-doc"
  )

  # published papers ----
  published_papers <- get_published_papers(my_xml)

  cli::cli_alert_success(
    "got {nrow(published_papers)} published papers"
  )

  # accepted papers ----
  accepted_papers <- get_accepted_papers(my_xml)

  cli::cli_alert_success(
    "got {nrow(accepted_papers)} accepted papers"
  )

  # conference papers
  conferences <- get_conferences(my_xml)

  cli::cli_alert_success(
    "got {nrow(conferences)} conferences"
  )

  # books ----
  books <- get_books(my_xml)

  cli::cli_alert_success(
    "got {nrow(books)} books"
  )

  # supervisions MSC ----
  superv_all <- get_superv(my_xml)

  cli::cli_alert_success(
    "got {nrow(superv_all)} supervisions"
  )

  # employment ----
  at_prof <- get_employment(my_xml)

  cli::cli_alert_success(
    "got {nrow(at_prof)} employment/activity registries"
  )

  # research and extension projects
  projs <- get_projs(my_xml)

  cli::cli_alert_success(
    "got {nrow(projs)} projects"
  )

  # co-authors
  coauthors <- get_coauthors(my_xml)

  cli::cli_alert_success(
    "got {nrow(coauthors)} coauthors"
  )


  # output
  l_out <- list(
    info = cvitae,
    course_bachelors = l_acad_back$grad,
    course_msc = l_acad_back$mestrado,
    course_phd = l_acad_back$doutorado,
    pos_doc = l_acad_back$pos_doc,
    published_papers = published_papers,
    accepted_papers = accepted_papers,
    books = books,
    supervisions = superv_all,
    at_prof = at_prof,
    projects = projs,
    coauthors = coauthors,
    conferences = conferences
  )

  # parse and fix list output
  fix_df <- function(df_in) {
    this_names <- names(df_in)

    to_numeric <- which(stringr::str_detect(this_names, "ano|numero_|mes_|_pagina"))

    for (i in to_numeric) {
      suppressWarnings({
        df_in[[ i]] <- as.numeric(df_in[[i]])
      })
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
