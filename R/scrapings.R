#' @importFrom dplyr %>%
#' @importFrom rlang .data

pdf_list <- function(page){
  tmp <- xml2::read_html(page) %>%
    rvest::html_nodes("a")

  tmp_list <- tibble::tibble("link" = rvest::html_attr(tmp, "href"), "case" = rvest::html_text(tmp))

  tmp <- tmp %>%
    rvest::html_attr("href") %>%
    stringr::str_subset("\\.pdf") %>%
    stringr::str_subset("^((?!publicinfo).)*$") %>%
    stringr::str_subset("^((?!diff).)*$")

  tmp <- tibble::tibble("link" = tmp)

  tmp_list <- dplyr::semi_join(tmp_list, tmp, by = "link")

  tmp_list$link <- stringr::str_c("https://www.supremecourt.gov", tmp_list$link)

  tmp_list
}


opinion_author <- function(srcdoc){
  srcdoc %>%
    purrr::map_chr(author_search) %>%
    stringr::str_to_title()
}

opinion_type <- function(srcdoc){
  srcdoc %>%
    purrr::map_chr(opinion_type2)
}

text_clean <- function(srcdoc){
  srcdoc %>%
    purrr::map_chr(text_clean_h1)
}

text_clean_h1 <- function(char_in){
  char_in %>%
    stringr::str_replace_all("-\\r\\n", "") %>%
    stringr::str_replace_all("\\r\\n", " ")
}

prep_text_h1 <- function(srcdoc){
  srcdoc <- srcdoc %>%
    tibble::as_tibble() %>%
    dplyr::mutate(is_first = stringr::str_detect(.data$value, "\\r\\nSUPREME COURT OF THE UNITED STATES\\r\\n")) %>%
    dplyr::mutate(group_no = 1)

  for(i in seq_along(srcdoc$is_first)) {
    if(i != 1){
      if (srcdoc$is_first[[i]] == "TRUE"){
        srcdoc$group_no[[i]] <- srcdoc$group_no[[i - 1]] + 1
      }
      else if(srcdoc$is_first[[i]] == "FALSE"){
        srcdoc$group_no[[i]] <- srcdoc$group_no[[i - 1]]
      }
    }
  }
  srcdoc %>%
    dplyr::select(-.data$is_first)
}


prep_text <- function(srcdoc) {
  tmp <- prep_text_h1(srcdoc) %>%
    dplyr::group_by(.data$group_no) %>%
    dplyr::group_split() %>%
    purrr::map(dplyr::transmute, "text" = .data$value)

  tmp <- purrr::map(tmp[], dplyr::pull, .data$text ) %>%
    purrr::map(stringr::str_c, collapse = "")

}

# opauth_h3 will check an opinion for the word "Syllabus" and if detected will
# assign as Author; if not detected, opauth_h3 will call opauth_h4 which will
# perform a similar search for "Per Curiam"

author_search <- function(char_in){
  patterns <- c("(Syllabus)", "(Per Curiam)",
                "((?<=Opinion of  )(.{1,15})(?=, J.))",
                "((?<=JUSTICE ).+(?= announced the judgment of))",
                "((?<=JUSTICE ).+(?= delivered the opinion of))",
                "((?<=JUSTICE ).+(?=, with whom))",
                "(.+(?=, J. ?, dissenting))", "(.+(?=, J., ?concurring))",
                "(.+(?=, C. ?J. ?, dissenting))", "(.+(?=, C. ?J. ?, concurring))",
                "((?<=(CHIEF )?JUSTICE ).+(?=, ((concurring)|(dissenting))))"
  )

  patterns <- stringr::str_c(patterns, collapse = "|")

  char_in <- char_in %>%
    stringr::str_trunc(2000)

  char_in %>%
    stringr::str_extract(patterns) %>%
    stringr::str_trim()
}

author_search2 <- function(char_in) {
  char_in <- char_in %>%
    stringr::str_trunc(2000)

  tmp <- author_search_detect(char_in, "Syllabus")
  tmp <- ifelse(is.na(tmp), author_search_detect(char_in, "Per Curiam"), tmp)
  tmp <- ifelse(is.na(tmp), stringr::str_extract(char_in, "(?<=JUSTICE ).+(?= announced the judgment of)"), tmp)
  tmp <- ifelse(is.na(tmp), stringr::str_extract(char_in, "(?<=JUSTICE ).+(?= delivered the opinion of)"), tmp)
  #tmp <- ifelse(is.na(tmp), stringr::str_extract(char_in, ".+(?=, C. J.,)"), tmp)
  #tmp <- ifelse(is.na(tmp), stringr::str_extract(char_in, ".+(?=, C.J.,)"), tmp)
  tmp <- ifelse(is.na(tmp), stringr::str_extract(char_in, ".+(?=, C?.? ?J., dissenting)"), tmp)
  #tmp <- ifelse(is.na(tmp), stringr::str_extract(char_in, ".+(?=, J. , dissenting)"), tmp)
  tmp <- ifelse(is.na(tmp), stringr::str_extract(char_in, ".+(?=, C?.? ?J., concurring)"), tmp)
  #tmp <- ifelse(is.na(tmp), stringr::str_extract(char_in, ".+(?=, J. , concurring)"), tmp)

  tmp %>%
    stringr::str_trim()

}


author_search_detect <- function(char_in, pattern) {
  ifelse(stringr::str_detect(char_in, pattern), pattern, NA)
}


# author_search2 <- function(char_in){
#
#   tester <- stringr::str_detect(char_in, "Syllabus")
#   ifelse(tester, "Syllabus", author_search_h1(char_in))
# }

author_search_h1 <- function(char_in) {
  tester <- stringr::str_detect(char_in, "Per Curiam")
  ifelse(tester, "Per Curiam", author_search_h2(char_in))
}

author_search_h2 <- function(char_in) {
  tester <- stringr::str_extract(char_in, "(?<=JUSTICE ).+(?= announced the judgment of)")

  ifelse(is.na(tester), author_search_h3(char_in), tester)
}

author_search_h3 <- function(char_in) {
  tester <- stringr::str_extract(char_in, "(?<=JUSTICE ).+(?= delivered the opinion of)")

  ifelse(is.na(tester), author_search_h4(char_in), tester)
}

author_search_h4 <- function(char_in) {
  tester <- stringr::str_extract(char_in, ".+(?=, J.,)") %>%
    stringr::str_trim()
  tester


}


opinion_type2 <- function(char_in){
  patterns <- c(
    "((Syllabus)|(Per Curiam))",
    "(( ((announced)|(delivered)) the ((judgment)|(opinion)) of))",
    "((((C. J.)|(J.)), ((concurring)|(dissenting))))",
    "(JUSTICE ).+, ?((concurring)|(dissenting))"
  )


  patterns <- stringr::str_c(patterns, collapse = "|")

  char_in <- stringr::str_trunc(char_in, 2000)

  tmp <- stringr::str_extract(char_in, patterns)

  tmp <- ifelse(stringr::str_detect(tmp, "Per Curiam"), "Majority", tmp)
  tmp <- ifelse(stringr::str_detect(tmp, "the ((judgment)|(opinion)) of"), "Majority", tmp)
  tmp <- ifelse(stringr::str_detect(tmp, "dissenting"), "Dissenting", tmp)
  tmp <- ifelse(stringr::str_detect(tmp, "concurring"), "Concurring", tmp)

  tmp
}

opinion_type_h0 <- function(char_in){
  tester <- stringr::str_detect(char_in, "Syllabus")
  ifelse(tester, "Syllabus", opinion_type_h1(char_in))
}

opinion_type_h1 <- function(char_in){
  tester <- stringr::str_detect(char_in, "Per Curiam")
  ifelse(tester, "Majority", opinion_type_h2(char_in))
}

opinion_type_h2 <- function(char_in){
  tester <- stringr::str_extract(char_in, "(?<=JUSTICE ).+(?= announced the judgment of)")

  ifelse(is.na(tester), opinion_type_h3(char_in), "Majority")
}

opinion_type_h3 <- function(char_in){
  tester <- stringr::str_extract(char_in, "(?<=JUSTICE ).+(?= delivered the opinion of)")

  char_in <- char_in %>%
    stringr::str_trunc(500)

  ifelse(is.na(tester), opinion_type_h4(char_in), "Majority")
}

opinion_type_h4 <- function(char_in){
  tester <- stringr::str_detect(char_in, ", J., concurring")
  ifelse(tester, "Concurring", opinion_type_h5(char_in))
}

opinion_type_h5 <- function(char_in){
  tester <- stringr::str_detect(char_in, ", J., dissenting")
  ifelse(tester, "Dissenting", "Juice")
}


