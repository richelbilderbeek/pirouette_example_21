# From https://github.com/richelbilderbeek/pirouette_article/issues/56:
#
# Write script that shows the true and twin error for hundreds of trees with
# 10, 1k, 2k, ..., 10k nucleotides
library(pirouette)
library(beautier)
library(beastier)
library(testthat)
library(ggplot2)

# Constants
example_no <- 21
rng_seed <- 314
crown_age <- 10
folder_name <- paste0("example_", example_no)

sequence_lengths <- c(100, 248, 500, 1000, 2000, 4000, 8000, 16000)
n_phylogenies_per_sequence_length <- 5
is_testing <- is_on_ci()
if (is_testing) {
  sequence_lengths <- c(100, 248)
  n_phylogenies_per_sequence_length <- 3
}
n_sequence_lengths <- length(sequence_lengths)
n_pir_params <- n_sequence_lengths * n_phylogenies_per_sequence_length

# Create phylogenies
phylogenies <- list()
for (i in seq_len(n_phylogenies_per_sequence_length)) {
  set.seed(rng_seed)
  phylogeny <- create_yule_tree(n_taxa = 6, crown_age = 10)
  phylogenies[[i]] <- phylogeny
}
# 1 2 3 1 2 3
phylogenies <- rep(phylogenies, each = n_sequence_lengths)

# Create pirouette parameter sets
expect_equal(n_pir_params, length(phylogenies))
pir_paramses <- create_std_pir_paramses(
  n = n_pir_params,
  folder_name = folder_name
)
expect_equal(length(pir_paramses), length(phylogenies))
if (is_testing) {
  pir_paramses <- shorten_pir_paramses(pir_paramses)
}

# Set the alignment lengths
# 1 1 1 2 2 2
sequence_lengthses <- rep(
  sequence_lengths, each = n_phylogenies_per_sequence_length
)
expect_equal(length(sequence_lengthses), length(pir_paramses))
for (i in seq_along(sequence_lengthses)) {
  pir_paramses[[i]]$alignment_params$root_sequence <- create_blocked_dna(
    length =  sequence_lengthses[[i]]
  )
}

# Do the runs
# DIRTY HACK: ONLY DO THE LAST 6
pir_outs <- pir_runs(
  phylogenies = phylogenies[34:40],
  pir_paramses = pir_paramses[34:40]
)

# Save summary
n_replicates <- n_phylogenies_per_sequence_length
pir_plots(pir_outs) +
  ggtitle(paste("Number of replicates: ", n_replicates)) +
  ggsave(file.path(folder_name, "errors.png"), width = 7, height = 7)

# Save individual runs
for (i in seq_along(sequence_lengths)) {
  sequence_length <- sequence_lengths[i]
  from_index <- ((i - 1) * n_replicates) + 1
  to_index <- ((i - 1) * n_replicates) + n_replicates
  pir_plots(
    pir_outs = pir_outs[from_index:to_index]
  ) + ggtitle(paste("Sequence length:", sequence_length)) +
    ggsave(
      filename = paste0("errors_", sequence_length, ".png"),
      width = 7,
      height = 7
    )
}

# Save
expect_equal(length(pir_paramses), length(pir_outs))
expect_equal(length(pir_paramses), length(phylogenies))
for (i in seq_along(pir_outs)) {
  pir_save(
    phylogeny = phylogenies[[i]],
    pir_params = pir_paramses[[i]],
    pir_out = pir_outs[[i]],
    folder_name = dirname(pir_paramses[[i]]$alignment_params$fasta_filename)
  )
}
