## code to prepare `SCOTUS_opinions_2019_session` dataset goes here
tmp_list <- pdf_list("https://www.supremecourt.gov/opinions/slipopinion/19")

SCOTUS_opinions_2019_session <- quilt_pdf_several(tmp_list[[1]], tmp_list[[2]])

SCOTUS_opinions_2019_session %>%
  write.csv("data-raw/SCOTUS_opinions_2019_session.csv")

usethis::use_data(SCOTUS_opinions_2019_session, overwrite = TRUE)

