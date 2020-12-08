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

}

opauth_h1 <- function(srcdoc){
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


opauth_h2 <- function(srcdoc) {

}
