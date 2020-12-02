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
