quilt_pdf <- function(srcpdf){
  tmp <- pdftools::pdf_text(srcpdf) %>%
    prep_text()

  tibble::tibble("text" = text_clean(tmp), "author" = opinion_author(tmp), "type" = opinion_type(tmp))
}
