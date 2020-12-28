## code to prepare `SCOTUS_opinions_2019_session` dataset goes here
tmp_list <- pdf_list("https://www.supremecourt.gov/opinions/slipopinion/19")

SCOTUS_opinions_2019_session <- quilt_pdf_several(tmp_list[[1]], tmp_list[[2]])

SCOTUS_opinions_2019_session %>%
  write.csv("data-raw/SCOTUS_opinions_2019_session.csv")

known_types <- tibble::tibble(type = c("Syllabus", "Majority", "Dissenting", "Concurring", "Statement"))
known_authors <- read.csv(system.file("extdata", "authors.csv", package = "opiniontext"))

SCOTUS_opinions_2019_session <- SCOTUS_opinions_2019_session %>%
  dplyr::semi_join(known_types, by = "type") %>%
  dplyr::semi_join(known_authors, by = "author")

usethis::use_data(SCOTUS_opinions_2019_session, overwrite = TRUE)

