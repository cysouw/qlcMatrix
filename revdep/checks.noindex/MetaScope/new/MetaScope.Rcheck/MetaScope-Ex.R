pkgname <- "MetaScope"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('MetaScope')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("align_details")
### * align_details

flush(stderr()); flush(stdout())

### Name: align_details
### Title: A universal parameter settings object for Rsubread alignment
### Aliases: align_details
### Keywords: datasets

### ** Examples

data("align_details")




cleanEx()
nameEx("align_target")
### * align_target

flush(stderr()); flush(stdout())

### Name: align_target
### Title: Align microbiome reads to a set of reference libraries
### Aliases: align_target

### ** Examples

#### Align example reads to an example reference library using Rsubread




cleanEx()
nameEx("align_target_bowtie")
### * align_target_bowtie

flush(stderr()); flush(stdout())

### Name: align_target_bowtie
### Title: Align microbiome reads to set of indexed Bowtie2 libraries
### Aliases: align_target_bowtie

### ** Examples

#### Align example reads to an example reference library using Rbowtie2

## Create temporary directory to store file
target_ref_temp <- tempfile()
dir.create(target_ref_temp)

## Dowload reference genome
MetaScope::download_refseq("Morbillivirus hominis",
                           reference = FALSE,
                           representative = FALSE,
                           compress = TRUE,
                           out_dir = target_ref_temp,
                           caching = TRUE
)

## Create temporary directory to store the indices
index_temp <- tempfile()
dir.create(index_temp)

## Create bowtie2 index
MetaScope::mk_bowtie_index(
  ref_dir = target_ref_temp,
  lib_dir = index_temp,
  lib_name = "target",
  overwrite = TRUE
)

## Create temporary directory for final file
output_temp <- tempfile()
dir.create(output_temp)

## Get path to example reads
readPath <- system.file("extdata", "virus_example.fastq",
                        package = "MetaScope")

## Align to target genomes
target_map <-
  MetaScope::align_target_bowtie(
    read1 = readPath,
    lib_dir = index_temp,
    libs = "target",
    align_dir = output_temp,
    align_file = "bowtie_target",
    overwrite = TRUE,
    bowtie2_options = "--very-sensitive-local"
  )

## Remove extra folders
unlink(target_ref_temp, recursive = TRUE)
unlink(index_temp, recursive = TRUE)
unlink(output_temp, recursive = TRUE)




cleanEx()
nameEx("bt2_16S_params")
### * bt2_16S_params

flush(stderr()); flush(stdout())

### Name: bt2_16S_params
### Title: A universal parameter object for Bowtie 2 16S alignment
### Aliases: bt2_16S_params
### Keywords: datasets

### ** Examples

data("bt2_16S_params")




cleanEx()
nameEx("bt2_loose_params")
### * bt2_loose_params

flush(stderr()); flush(stdout())

### Name: bt2_loose_params
### Title: A universal parameter object for Bowtie 2 loose alignment
### Aliases: bt2_loose_params
### Keywords: datasets

### ** Examples

data("bt2_loose_params")




cleanEx()
nameEx("convert_animalcules")
### * convert_animalcules

flush(stderr()); flush(stdout())

### Name: convert_animalcules
### Title: Create a multi-assay experiment from MetaScope output for usage
###   with animalcules
### Aliases: convert_animalcules

### ** Examples

tempfolder <- tempfile()
dir.create(tempfolder)

# Create three different samples
samp_names <- c("X123", "X456", "X789")
all_files <- file.path(tempfolder,
                       paste0(samp_names, ".csv"))

create_IDcsv <- function (out_file) {
  final_taxids <- c("273036", "418127", "11234")
  final_genomes <- c(
    "Staphylococcus aureus RF122, complete sequence",
    "Staphylococcus aureus subsp. aureus Mu3, complete sequence",
    "Measles virus, complete genome")
  best_hit <- sample(seq(100, 1050), 3)
  proportion <- best_hit/sum(best_hit) |> round(2)
  EMreads <- best_hit + round(runif(3), 1)
  EMprop <- proportion + 0.003
  dplyr::tibble(TaxonomyID = final_taxids,
                Genome = final_genomes,
                read_count = best_hit, Proportion = proportion,
                EMreads = EMreads, EMProportion = EMprop) |>
    dplyr::arrange(dplyr::desc(.data$read_count)) |>
    utils::write.csv(file = out_file, row.names = FALSE)
  message("Done!")
  return(out_file)
}
out_files <- vapply(all_files, create_IDcsv, FUN.VALUE = character(1))

# Create annotation data for samples
annot_dat <- file.path(tempfolder, "annot.csv")
dplyr::tibble(Sample = samp_names, RSV = c("pos", "neg", "pos"),
              month = c("March", "July", "Aug"),
              yrsold = c(0.5, 0.6, 0.2)) |>
  utils::write.csv(file = annot_dat,
                   row.names = FALSE)

# Convert samples to MAE
outMAE <- convert_animalcules(meta_counts = out_files,
                              annot_path = annot_dat,
                              which_annot_col = "Sample",
                              end_string = ".metascope_id.csv",
                              qiime_biom_out = FALSE,
                              path_to_write = tempfolder,
                              NCBI_key = NULL)

unlink(tempfolder, recursive = TRUE)




cleanEx()
nameEx("count_matches")
### * count_matches

flush(stderr()); flush(stdout())

### Name: count_matches
### Title: Count the number of base lengths in a CIGAR string for a given
###   operation
### Aliases: count_matches

### ** Examples

# A single cigar string: 3M + 3M + 5M
cigar1 <- "3M1I3M1D5M"
count_matches(cigar1, char = "M")

# Parse with operator "P": 2P
cigar2 <- "4M1I2P9M"
count_matches(cigar2, char = "P")

# Apply to multiple strings: 1I + 1I + 5I
cigar3 <- c("3M1I3M1D5M", "4M1I1P9M", "76M13M5I")
vapply(cigar3, count_matches, char = "I",
       FUN.VALUE = numeric(1))




cleanEx()
nameEx("demultiplex")
### * demultiplex

flush(stderr()); flush(stdout())

### Name: demultiplex
### Title: Demultiplexing sequencing reads
### Aliases: demultiplex

### ** Examples


## Get barcode, index, and read data locations
barcodePath <- system.file("extdata", "barcodes.txt", package = "MetaScope")
indexPath <- system.file("extdata", "virus_example_index.fastq",
                         package = "MetaScope")
readPath <- system.file("extdata", "virus_example.fastq",
                         package = "MetaScope")

## Demultiplex
demult <- demultiplex(barcodePath, indexPath, readPath, rcBarcodes = FALSE,
                      hammingDist = 2)
demult




cleanEx()
nameEx("download_refseq")
### * download_refseq

flush(stderr()); flush(stdout())

### Name: download_refseq
### Title: Download RefSeq genome libraries
### Aliases: download_refseq

### ** Examples

#### Download RefSeq genomes

## Download all RefSeq reference Shotokuvirae kingdom genomes
download_refseq('Shotokuvirae', reference = TRUE, representative = FALSE,
                out_dir = NULL, compress = TRUE, patho_out = FALSE,
                caching = TRUE)




cleanEx()
nameEx("extract_reads")
### * extract_reads

flush(stderr()); flush(stdout())

### Name: extract_reads
### Title: Helper function for demultiplexing
### Aliases: extract_reads

### ** Examples


## Create temporary directory
ref_temp <- tempfile()
dir.create(ref_temp)

## Load example barcode, index, and read data into R session
barcodePath <- system.file("extdata", "barcodes.txt", package = "MetaScope")
bcFile <- read.table(barcodePath, sep = "\t", header = TRUE)

indexPath <- system.file("extdata", "virus_example_index.fastq",
package = "MetaScope")
inds <- Biostrings::readDNAStringSet(indexPath, format = "fastq")

readPath <- system.file("extdata", "virus_example.fastq",
                        package = "MetaScope")
reads <- Biostrings::readQualityScaledDNAStringSet(readPath)

## Extract reads from the first barcode
results <- extract_reads(1, bcFile[, 2], bcFile[, 1], inds, reads,
                        rcBarcodes = FALSE, location = ref_temp)

## Extract reads from multiple barcodes
more_results <- lapply(1:6, extract_reads, bcFile[, 2], bcFile[, 1], inds,
                       reads, rcBarcodes = FALSE, location = ref_temp)

## Remove temporary directory
unlink(ref_temp, recursive = TRUE)




cleanEx()
nameEx("filter_host")
### * filter_host

flush(stderr()); flush(stdout())

### Name: filter_host
### Title: Use Rsubread to align reads against one or more filter libraries
###   and subsequently remove mapped reads
### Aliases: filter_host

### ** Examples

#### Filter reads from bam file that align to any of the filter libraries

## Assuming a bam file has been created previously with align_target()




cleanEx()
nameEx("filter_host_bowtie")
### * filter_host_bowtie

flush(stderr()); flush(stdout())

### Name: filter_host_bowtie
### Title: Use Rbowtie2 to align reads against one or more filter libraries
###   and subsequently remove mapped reads
### Aliases: filter_host_bowtie

### ** Examples

#### Filter reads from bam file that align to any of the filter libraries

## Assuming a bam file has already been created with align_target_bowtie()
# Create temporary filter library
filter_ref_temp <- tempfile()
dir.create(filter_ref_temp)

## Download reference genome
MetaScope::download_refseq("Orthoebolavirus zairense",
                           reference = FALSE,
                           representative = FALSE,
                           compress = TRUE,
                           out_dir = filter_ref_temp,
                           caching = TRUE)

## Create temp directory to store the indices
index_temp <- tempfile()
dir.create(index_temp)

## Create filter index
MetaScope::mk_bowtie_index(
  ref_dir = filter_ref_temp,
  lib_dir = index_temp,
  lib_name = "filter",
  overwrite = TRUE
)

## Create temporary folder to hold final output file
output_temp <- tempfile()
dir.create(output_temp)

## Get path to example bam
bamPath <- system.file("extdata", "bowtie_target.bam",
                       package = "MetaScope")
target_copied <- file.path(output_temp, "bowtie_target.bam")
file.copy(bamPath, target_copied)

## Align and filter reads
filter_out <-
  filter_host_bowtie(
    reads_bam = target_copied,
    lib_dir = index_temp,
    libs = "filter",
    threads = 1
  )

## Remove temporary directories
unlink(filter_ref_temp, recursive = TRUE)
unlink(index_temp, recursive = TRUE)
unlink(output_temp, recursive = TRUE)




cleanEx()
nameEx("get_children")
### * get_children

flush(stderr()); flush(stdout())

### Name: get_children
### Title: Get child nodes from NCBI taxonomy
### Aliases: get_children

### ** Examples

## Get all child species and strains in bacteria superkingdom
get_children('Bacteria','superkingdom')

## Get all child species and strains in fungi kingdom
get_children('Fungi', 'kingdom')

## Get all child species in primate order
get_children('Primates', 'order')




cleanEx()
nameEx("metascope_id")
### * metascope_id

flush(stderr()); flush(stdout())

### Name: metascope_id
### Title: Identify which genomes are represented in a processed sample
### Aliases: metascope_id

### ** Examples

#### Align reads to reference library and then apply metascope_id()
## Assuming filtered bam files already exist

## Create temporary directory
file_temp <- tempfile()
dir.create(file_temp)

#### Subread aligned bam file

## Create object with path to filtered subread csv.gz file
filt_file <- "subread_target.filtered.csv.gz"
bamPath <- system.file("extdata", filt_file, package = "MetaScope")
file.copy(bamPath, file_temp)

## Run metascope id with the aligner option set to subread
metascope_id(input_file = file.path(file_temp, filt_file),
             aligner = "subread", num_species_plot = 0,
             input_type = "csv.gz")

#### Bowtie 2 aligned .csv.gz file

## Create object with path to filtered bowtie2 bam file
bowtie_file <- "bowtie_target.filtered.csv.gz"
bamPath <- system.file("extdata", bowtie_file, package = "MetaScope")
file.copy(bamPath, file_temp)

## Run metascope id with the aligner option set to bowtie2
metascope_id(file.path(file_temp, bowtie_file), aligner = "bowtie2",
             num_species_plot = 0, input_type = "csv.gz")

## Remove temporary directory
unlink(file_temp, recursive = TRUE)




cleanEx()
nameEx("mk_bowtie_index")
### * mk_bowtie_index

flush(stderr()); flush(stdout())

### Name: mk_bowtie_index
### Title: Make a Bowtie2 index
### Aliases: mk_bowtie_index

### ** Examples

#### Create a bowtie index from the example reference library

## Create a temporary directory to store the reference library
ref_temp <- tempfile()
dir.create(ref_temp)

## Download reference genome
download_refseq('Shotokuvirae', reference = TRUE, representative = FALSE,
                out_dir = ref_temp, compress = TRUE, patho_out = FALSE,
                caching = TRUE)

## Create the reference library index files in the current directory
mk_bowtie_index(ref_dir = ref_temp, lib_dir = ref_temp,
                lib_name = "target", threads = 1, overwrite = FALSE)

## Remove temporary directory
unlink(ref_temp, recursive = TRUE)




cleanEx()
nameEx("mk_subread_index")
### * mk_subread_index

flush(stderr()); flush(stdout())

### Name: mk_subread_index
### Title: Make a Subread index
### Aliases: mk_subread_index

### ** Examples

#### Create a subread index from the example reference library

## Create a temporary directory to store the reference library
ref_temp <- tempfile()
dir.create(ref_temp)

## Download reference genome
out_fasta <- download_refseq('Orthoebolavirus zairense', reference = FALSE,
                             representative = FALSE, out_dir = ref_temp,
                             compress = TRUE, patho_out = FALSE,
                             caching = TRUE)

## Make subread index of reference library
mk_subread_index(out_fasta)
unlink(ref_temp)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
