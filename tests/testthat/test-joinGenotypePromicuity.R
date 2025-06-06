test_that("joinGenotypePrimisuity only creates expexted loci", {
  
  loci <- c("A", "B", "C", "DP", "DQ", "DR")
  expect_contains(loci , unique(joinGenotypePromiscuity(test_HLA_table1, promiscuity)$Locus ) )
})
