# From https://github.com/richelbilderbeek/pirouette_article/issues/56:
#
# Write script that shows the true and twin error for hundreds of trees with
# 10, 1k, 2k, ..., 10k nucleotides
suppressMessages(library(pirouette))
suppressMessages(library(ggplot2))

################################################################################
# Constants
################################################################################
is_testing <- is_on_travis()

root_folder <- getwd()
example_no <- 21

seed_to_sequence_length <- function(rng_seed) {
  sequence_lengths <- c(100, 248, 500, 1000, 2000, 4000, 8000, 16000)
  sequence_length <- sequence_lengths[rng_seed + 1 - 314]
  if (is.na(sequence_length)) stop("Invalid seed")
  sequence_length
}
testthat::expect_equal(seed_to_sequence_length(314), 100)
testthat::expect_equal(seed_to_sequence_length(315), 248)
testthat::expect_equal(seed_to_sequence_length(316), 500)
testthat::expect_equal(seed_to_sequence_length(317), 1000)
testthat::expect_equal(seed_to_sequence_length(318), 2000)
testthat::expect_equal(seed_to_sequence_length(319), 4000)
testthat::expect_equal(seed_to_sequence_length(320), 8000)
testthat::expect_equal(seed_to_sequence_length(321), 16000)
testthat::expect_error(seed_to_sequence_length(322))

for (rng_seed in seq(314, 321)) {
  folder_name <- file.path(paste0("example_", example_no, "_", rng_seed))

  set.seed(rng_seed)
  phylogeny <- create_yule_tree(n_taxa = 6, crown_age = 10)

  pir_params <- create_std_pir_params(
    folder_name = folder_name,
    sequence_length = seed_to_sequence_length(rng_seed)
  )

  if (is_testing) {
    pir_params <- shorten_pir_params(pir_params)
  }

  errors <- pir_run(
    phylogeny = phylogeny,
    pir_params = pir_params
  )

  utils::write.csv(
    x = errors,
    file = file.path(folder_name, "errors.csv"),
    row.names = FALSE
  )

  pir_plot(errors) +
    ggsave(file.path(folder_name, "errors.png"), width = 7, height = 7)

  pir_to_pics(
    phylogeny = phylogeny,
    pir_params = pir_params,
    folder = folder_name
  )

  pir_to_tables(
    pir_params = pir_params,
    folder = folder_name
  )
}
