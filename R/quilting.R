
quilt_pdf <- function(srcpdf, case_name){
  tmp <- pdftools::pdf_text(srcpdf) %>%
    prep_text()

  tibble::tibble("text" = text_clean(tmp), "author" = opinion_author(tmp), "type" = opinion_type(tmp), "case" = case_name)
}


quilt_pdf_several <- function(srclist, case_name){
  srclist %>%
    purrr::map2_dfr(case_name, quilt_pdf)
}
