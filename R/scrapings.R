#' @importFrom dplyr %>%

pdf_list <- function(page){
  xml2::read_html(page) %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    stringr::str_subset("\\.pdf") %>%
    stringr::str_subset("^((?!publicinfo).)*$") %>%
    stringr::str_subset("^((?!diff).)*$") %>%
    stringr::str_c("https://www.supremecourt.gov", .)
}


opinion_author <- function(srcdoc){
  prep_text(srcdoc) %>%
    purrr::map_chr(author_search) %>%
    stringr::str_to_title()
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
    dplyr::select(-is_first)
}


prep_text <- function(srcdoc) {
  tmp <- prep_text_h1(srcdoc) %>%
    dplyr::group_by(group_no) %>%
    dplyr::group_split() %>%
    purrr::map(dplyr::transmute, "text" = value)

  tmp <- purrr::map(tmp[], dplyr::pull, text ) %>%
    purrr::map(stringr::str_c, collapse = "")

}

# opauth_h3 will check an opinion for the word "Syllabus" and if detected will
# assign as Author; if not detected, opauth_h3 will call opauth_h4 which will
# perform a similar search for "Per Curiam"



author_search <- function(char_in){

  tester <- stringr::str_detect(char_in, "Syllabus")
  ifelse(tester, "Syllabus", author_search_h1(char_in))
}

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
