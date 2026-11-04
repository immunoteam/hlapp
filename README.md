# HLAapp webtool

The HLApp webtool is available on: <https://gergely-koppany.shinyapps.io/hlapp/>

# User manual

Version 1.0 \| 22 October 2026

Use this guide to prepare HLA genotypes, select outputs, interpret
peptide-binding promiscuity and divergence, and export results.

The app supports HLA-A, HLA-B and HLA-C, HLA-DP, HLA-DQ and HLA-DRB1.
The metrics describe different properties; a high score does not by
itself establish clinical benefit.

### Choose the output that matches your question

| **Metric** | **What it describes** | **Output level** |
|----|----|----|
| Promiscuity | Breadth of the peptide sequence preferences of one HLA molecule | Individual alleles, means within HLA-A/B/C/DP/DQ/DRB1 and across HLA-I and II |
| HED | Physicochemical divergence between homologous HLA protein sequences | Separate values for HLA-A/B/C/DP/DQ/DRB1 and means across HLA-I and II |
| Functional divergence | Complementarity of peptide-binding submotifs for an allele pair | Separate values for HLA-A/B/C/DP/DQ/DRB1 and means across HLA-I and II |
| Homozygosity | Whether only one distinct allele remains at a gene | Gene-specific flags |

### Quick start

1\. Download example data on the Calculate HLA metrics tab and prepare
one row per sample with a unique ID.

2\. Upload the table, choose the ID column and one representation of the
HLA genotype, and set the delimiter used inside allele-list cells.

3\. Select the metrics to calculate: allele or locus promiscuity,
homozygosity, HED and/or functional divergence. Choose Get results,
review completeness, then export.

Navigation: input preparation on page 2; calculation workflow on page 3;
promiscuity on page 4; HED on page 5; functional divergence on page 6;
additional tools on page 7; troubleshooting and verification on page 8;
references on page 9.

# Prepare the input table

Use a rectangular table with column names in the first row and one
sample per subsequent row. Include a unique, nonempty sample ID. Column
names are selected in the interface and need not follow a fixed naming
scheme. Remove titles, notes, merged cells and extra header rows.

| **File** | **Confirmed reader behavior** |
|----|----|
| CSV | Comma-separated columns. Quote any cell that itself contains commas. |
| TSV | Tab-separated columns. |
| XLSX or XLS | Excel table; the reader uses its default first worksheet. |
| TXT | Comma-separated by default in the upload module. Prefer CSV or TSV to avoid ambiguity. |

## Two ways to represent genotypes

1\. Separate columns. Store the two copies of each gene in separate
columns, including both identical copies for a known homozygote. For
example:

ID,A1,A2,B1,B2,C1,C2\
S01,A\*01:01,A\*03:01,B\*51:01,B\*57:01,C\*07:01,C\*15:02\
S02,A\*02:01,A\*02:01,B\*13:02,B\*18:01,C\*06:02,C\*07:01

2\. Allele lists. Store multiple alleles in a cell, separated by commas,
semicolons or spaces; select the matching delimiter in step 2. The outer
file delimiter and the delimiter inside a cell are different settings.
Comma is the recommended internal delimiter.

ID,HLA_I\
S01,"A0101,A0301,B5101,B5701,C0701,C1502"

The app example contains both HLA_I/HLA_II lists and separate gene
columns. Select either the lists or the separate columns for a sample,
not both. Combining duplicate representations can inflate allele counts
and prevent calculation.

### Allele names and class II complexes

Use uppercase, two-field allele names with the locus included: A\*01:01
or A0101; DRB1\*07:01 or DRB10701. The importer removes several
punctuation characters and truncates longer numeric names to four
digits. Use canonical compact names if detection fails. Do not assume
that low-resolution typing, ambiguous calls, null-expression suffixes or
every modern allele name is supported.

DP example: DPA10103-DPB10301\
DQ example: DQA10201-DQB10303\
DR example: DRB10701

For DP and DQ, provide explicit alpha-beta complexes, or separate alpha-
and beta-chain calls. With separate calls, the code forms every
alpha-beta combination within the sample, including potential trans
combinations, and discards DQ combinations if they are unable to form
stable complexes (i.e., they are forbidden pairings \[1\]). DR is
represented by DRB1 alone.

Known homozygosity is different from incomplete typing. A single allele
in a list does not prove homozygosity. Preserve both copies where known;
never fill an unknown second allele by copying the first.

## Calculate and export results

### Step 1 Upload file

Open “Calculate HLA metrics” and remain on the “1. Upload file” tab.\
Use Browse... to choose your table. Download example data provides the
app’s own CSV template.\
After the file is read successfully, select Next -\>.\
If the table cannot be read, check the file format and remove any
non-table lines.

### Step 2 Select input columns

Choose the column of sample IDs, then tick the HLA columns to analyze.
Select / Deselect all is convenient only when every detected column is
needed. For example, in the published example data, it would select both
alternative genotype representations, so you have to choose columns
individually.

Set the internal delimiter to comma, space or semicolon. Inspect Your
input, which previews the first three rows. The source checks for
candidate HLA columns in the first six rows and checks whether alleles
can be recognized within the first 100 rows. A successful recognition
message does not validate every sample. Ensure all intended loci are
selected, then click Next -\>.

### Step 3 Choose output

| **Control** | **Selection and meaning** |
|----|----|
| Allele promiscuity | Select loci for allele-level promiscuity values |
| Locus promiscuity | Select loci for calculating the mean of available scores within each locus. |
| Homozygosity | Choose yes to include gene-specific homozygosity flags. |
| HLA evolutionary divergence | Choose HLA-I and/or HLA-II for class-level HED values. |
| Functional divergence | Select loci for calculating functional divergence between alleles within each locus |

The output choices are updated from recognized loci. Select at least one
output and resolve excessive-allele errors (i.e., when more than two
alleles are provided for a given locus) to enable Get results. Go back
returns to the preceding step. Get results calculates the selected
summaries.

### Read and export the table

The results are returned in a table format with all the selected
calculated metrics for each ID in one row. You can export the results in
.xlsx and .csv format or copy them to clipboard.

You can select any functional divergence value in the table and click on
the “Visualize functional divergence” button to see and compare
submotifs of peptides bound by the two alleles at the given locus.

## HLA peptide binding promiscuity

Promiscuity describes how broadly an HLA molecule can bind peptide
sequences.

### Calculating promiscuity for HLA class I molecules

In the original publication, we analyzed positive IEDB binding or T-cell
assay records for 8–12-residue peptides, retaining alleles with at least
400 distinct peptides \[2\]. In the current implementation of the app,
promiscuity was calculated using the Sarkizova et al. immunopeptidomics
dataset \[3\] to avoid bias caused by heterogeneous study results
deposited in the IEDB. For each peptide length L and position y,
amino-acid frequencies P are compared with human-proteome background
frequencies Q by calculating the Kullback-Laibler divergence value.
\[2\]

``` math
D_{KL,L}(y)\text{ } = \Sigma_{a}P_{L}(a,y)\text{ }\log\lbrack P_{L}(a,y)\frac{\text{ }}{\text{ }}Q(a)\rbrack
```

A value of 10⁻⁷ is added to all amino-acid frequencies at a position
containing a zero frequency. Position values are averaged within each
length and corrected by subtracting the mean from 1,000 random
human-proteome peptide samples of matching size and length.
Length-specific corrected values are weighted by their repertoire
proportions. Promiscuity is the reciprocal. \[2\]

``` math
D_{corrected,L} = \text{ }(\frac{1}{L})\Sigma_{y}D_{KL,L}(y)\text{ } - D_{random,L}
```

``` math
\Pr\text{ } = \text{ }1\frac{\text{ }}{\text{ }}\lbrack\Sigma_{L}w_{L}D_{corrected,L}\rbrack
```

Here w_L is the fraction of peptides of length L. Lower corrected
divergence from background gives higher Pr. \[2\]

### Calculating promiscuity for HLA class II molecules

To determine promiscuity for HLA-II molecules, we applied the same
approach to unique nine-residue binding cores from a large
immunopeptidomics dataset \[4\]. We retained alleles with at least 100
core sequences.

### App implementation

The app calculates promiscuity for each selected locus. Locus
promiscuity is an unweighted arithmetic mean of alleles for
HLA-A/B/C/DRB1 and alpha-beta chain combinations for HLA-DP/DQ. Missing
scores are ignored during calculation.

``` math
\Pr_{mean} = \text{ }(\frac{1}{k})\Sigma_{j}\Pr_{j}
```

Here k is the number of available scores at the locus.

## HLA evolutionary divergence

HED quantifies physicochemical differences between the two homologous
HLA protein sequences. The Grantham amino-acid distance reflects
composition, polarity and volume. For aligned peptide-binding domains of
length N, HED is the mean site-wise Grantham distance. Identical
sequences give zero. According to Chowell et al., we averaged the HLA-A,
HLA-B and HLA-C pairwise values to obtain mean HLA-I HED. \[5–7\]

``` math
HED(u,v)\text{ } = \text{ }(\frac{1}{N})\Sigma_{i}G(u_{i},v_{i})
```

``` math
{HED}_{I} = \text{ }\lbrack{HED}_{A} + {HED}_{B} + {HED}_{C}\rbrack\frac{\text{ }}{\text{ }}3
```

G is the Grantham distance for the two amino acids at aligned position
i. Higher HED denotes more physicochemical sequence divergence; it is a
proxy for binding-repertoire differences rather than a direct
measurement of peptide overlap. \[5,6\]

For HLA-II, distances are compared within DPA1, DPB1, DQA1, DQB1 and
DRB1, then the five values are averaged.

``` math
{HED}_{II} = \text{ }\lbrack{HED}_{DPA1} + {HED}_{DPB1} + {HED}_{DQA1} + {HED}_{DQB1} + {HED}_{DRB1}\rbrack\frac{\text{ }}{\text{ }}5
```

HED values for allele pairs at the same locus are calculated using the
perl script published by Pierini et al. \[6,
https://sourceforge.net/projects/granthamdist/\]. HLA allele sequences
were downloaded from the same resource.

HED values for HLA-I and HLA-II are only calculated if all loci are
represented and have valid HED values.

### 

## HLA functional divergence

HLA functional divergence describes the degree to which an individual’s
HLA allele at given locus differ in their peptide-binding preferences,
reflecting their capacity to present distinct sets of antigens to T
cells.

### Calculating functional divergence

Following the approach by Viard et al., we determined nonamer peptide
clusters from 95 HLA-I alleles. We summarized clusters as submotifs,
retaining highly selective position–amino-acid features with an entropy
value below 0.1. We then compared the submotif overlap of two alleles
yielding a divergence measure: greater divergence means lower overlap
between the repertoire of the two alleles. As in the original study,
singletons were retained as a separate subgroup. \[8\]

We applied the same approach for HLA-II alleles by using unique
nine-residue binding cores from a large immunopeptidomics dataset
\[ref\].

**Warning.** The original publication’s calculation scripts were not
available to the app developers. While the reproduced values show strong
correlation with the ones in the original publication (Spearman’s rho
was 0.84, 0.79, and 0.9 for HLA-A, B, and C, respectively), some of them
may differ. Therefore, the numeric result should be reported as an
app-derived reproduction, with its version stated.

### 

## Explore motifs and translate allele names

### Submotif explorer

This module visualizes peptide clusters and submotifs used for
calculating functional divergence between HLA alleles (see page 6. and
ref. \[8\]).

Choose “Submotif explorer”, then HLA-I or HLA-II under “Select HLA
class”\
When the data are loaded, “Select allele form list, or start typing”:
chooses a molecule\
Select a cluster: chooses the peptide cluster of the selected allele
(the clustered are ordered by decreasing number of peptides).\
The general view shows the molecule’s motif, total peptide count and an
NMDS plot; the selected cluster is highlighted with green.

On the right side, you can see the submotif of the selected peptide
cluster, the peptide count, and the entropy of amino acids at each
position. On the entropy plot, the red dashed line indicates the entropy
value of 0.1, which is used for defining the submotif (see page 6.).

Peptide sequences can be visualized with the “Show peptides/Hide
peptides” button. All data on the submotif can be downloaded:

| **Download control**   | **File and intended use**        |
|------------------------|----------------------------------|
| Download Submotif Plot | SVG vector graphic               |
| Download Submotif Data | RDS motif data for analysis in R |
| Download Peptide List  | TXT peptide list                 |
| Download Entropy Plot  | PNG graphic                      |
| Download Entropy Data  | RDS vector of entropy values     |

### Submotif comparison

Choose “Submotif comparison” and select under Allele 1 and Allele 2. The
code displays each molecule’s binding motif and the submotif composition
text, and also the functional-divergence value for the two alleles.

### Translate allele formats

Enter canonical compact identifiers, one per line, in the “Enter
alleles” field; for example A0101, DRB10701 or DQA10201-DQB10303.
Alternatively, select the “Load alleles from PromsicuityApp” to import
unique allele names in the “Calculate HLA metrics” module. Select
NetMHCpan, MHCnuggets and/or MixMHCpred.

The output includes columns for NetMHCpan/NetMHCIIpan, MHCnuggets, and
MixMHCpred/MixMHC2pred as selected. Unsupported entries trigger a
warning and are excluded from the translation table.

## Troubleshoot and verify the analysis

| **Symptom** | **Action** |
|----|----|
| No candidate input ID column | Use unique, nonempty IDs in a dedicated column that contains no HLA allele calls. |
| No HLA columns or alleles recognized | Use uppercase locus-prefixed two-field calls. Put valid calls near the top; check the internal delimiter and selected columns. |
| More than two alleles error | Select one genotype representation. Remove accidental duplicate columns. The code permits up to four DP or DQ complexes but only two A, B, C or DRB1 entries. |
| Only one allele warning | Check that both gene copies are present where required. Do not replace unknown calls with fabricated homozygosity. |
| Blank NA or NaN result | Check allele or pair coverage. Missing lookup values are not evidence of zero. Available-case averages can hide incomplete coverage. |
| Empty plots or disabled results | Check input selections and data availability. Reload the app if the session stops responding. |

### Check every sample before interpretation

Check exported IDs against the input, review each required gene and
score, and count missing outputs. Preserve known homozygous copies,
record excluded DQ combinations, and state whether partial-case means
were accepted. Save the input, selected columns and delimiters, output
choices, export and analysis date. These records make comparisons
reproducible.

## Scientific and implementation references

Bracketed numbers refer to these sources. Links open the original
methods or implementation. Access date for web and repository material
is 13 October 2026.

\[1\] Raymond CK et al. Ancient haplotypes of the HLA Class II region.
Genome Research. 2005;15.9:1250-1257. Definition of forbidden HLA-DQ
alpha and beta chain pairings.

[doi.org/10.1101/gr.3554305](https://doi.org/10.1101/gr.3554305)

\[2\] Manczinger M et al. Negative trade-off between neoantigen
repertoire breadth and the specificity of HLA-I molecules shapes
antitumor immunity. Nature Cancer. 2021;2:950–961. Original HLA-I
KL-based promiscuity method.

[doi.org/10.1038/s43018-021-00226-4](https://doi.org/10.1038/s43018-021-00226-4)

\[3\] Sarkizova S et al. A large peptidome dataset improves HLA class I
epitope prediction across most of the human population. Nature
Biotechnology. 2020;38:199–209. HLAthena peptide data underlying the
published submotif approach.

[doi.org/10.1038/s41587-019-0322-9](https://doi.org/10.1038/s41587-019-0322-9)

\[4\] HLA-II peptide-binding diversity shapes humoral immune responses
and susceptibility to infections. Nature Communications. 2026. Methods
section Determining the promiscuity of HLA-II molecules.

[doi.org/10.1038/s41467-026-76184-1](https://doi.org/10.1038/s41467-026-76184-1)

\[5\] Chowell D et al. Evolutionary divergence of HLA class I genotype
impacts efficacy of cancer immunotherapy. Nature Medicine.
2019;25:1715–1720. HED calculation and HLA-I aggregation.

[doi.org/10.1038/s41591-019-0639-4](https://doi.org/10.1038/s41591-019-0639-4)

\[6\] Pierini F and Lenz TL. Divergent Allele Advantage at Human MHC
Genes Signatures of Past and Ongoing Selection. Molecular Biology and
Evolution. 2018;35:2145–2158. Sequence divergence and peptide repertoire
analysis.

[doi.org/10.1093/molbev/msy116](https://doi.org/10.1093/molbev/msy116)

\[7\] Grantham R. Amino acid difference formula to help explain protein
evolution. Science. 1974;185:862–864. Original physicochemical
amino-acid distance.

[doi.org/10.1126/science.185.4154.862](https://doi.org/10.1126/science.185.4154.862)

\[8\] Viard M et al. Impact of HLA class I functional divergence on HIV
control. Science. 2024;383:319–325. Submotif-based method; supplementary
Data S1 supplies reference pairwise values.

[doi.org/10.1126/science.adk0777](https://doi.org/10.1126/science.adk0777)

### Support

For app errors or feedback, use the [promiscuity_app
repository](https://github.com/immunoteam/promiscuity_app). Include the
app date, selected settings and a minimal example without identifiable
sample information (screenshot is preferred).
