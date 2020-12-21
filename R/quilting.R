
quilt_pdf <- function(srcpdf){
  tmp <- pdftools::pdf_text(srcpdf) %>%
    prep_text()

  tibble::tibble("text" = text_clean(tmp), "author" = opinion_author(tmp), "type" = opinion_type(tmp))
}


quilt_pdf_several <- function(srclist){
  srclist %>%
    purrr::map_dfr(quilt_pdf)
}
