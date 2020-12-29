tmp_list <- pdf_list("https://www.supremecourt.gov/opinions/slipopinion/19")

opinions_2019 <- quilt_pdf_several(tmp_list[[1]], tmp_list[[2]])

opinions_2019 %>%
  write.csv("data-raw/opinions_2019.csv")

known_types <- tibble::tibble(type = c("Syllabus", "Majority", "Dissenting", "Concurring", "Statement"))
known_authors <- read.csv(system.file("extdata", "authors.csv", package = "opiniontext"))

opinions_2019 <- opinions_2019 %>%
  dplyr::semi_join(known_types, by = "type") %>%
  dplyr::semi_join(known_authors, by = "author")

usethis::use_data(opinions_2019, overwrite = TRUE)
