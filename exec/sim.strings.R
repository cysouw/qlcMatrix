#!/usr/bin/env Rscript

# =================
# Copyright 2015 Michael Cysouw <cysouw@mac.com>
#
# This file is free software: you may copy, redistribute and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.
#
# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# =================

# =====
# usage
# =====

DOC <- "
Usage: 
  sim.strings [-h -d -s SEPARATOR -r DECIMALS <STRINGS>...]

Using the function sim.strings() from the R-package qlcMatrix. Efficient computation of pairwise string similarities using a cosine similarity on bigram vectors. Details at http://www.rdocumentation.org/packages/qlcMatrix/functions/sim.strings.html

Options:
  -h, --help      Showing this help text
  -d, --distance  Return distances instead of similarities
  -s SEPARATOR    Separator, defaults to nothing, use 'S' to get space [default: ]
  -r DECIMALS     Round result to decimals [default: 3]
"

# ==============
# docopt parsing
# ==============

attach(docopt::docopt(DOC))

# for piping data
if (length(STRINGS) == 0) {
	STRINGS <- scan(file("stdin") , sep = "\n" , quiet = TRUE , what = "character")
	closeAllConnections() 
}

# default values are strings by default, not numbers
r <- as.numeric(r)

# space cannot be passed as argument in bash
if (s == "S") {s <- " "}

# ======
# R code
# ======

library(methods) # this declaration is a bug; should not be necessary

result <- qlcMatrix::sim.strings(STRINGS, sep = s)
if (distance) {
	result <- as.matrix(1 - result)
} else {
	result <- as.matrix(result)
}
result <- round(result, digits = r)


# =============
# Return output
# =============

write.table(result
	, file = ""
	, sep = "\t"
	, row.names = FALSE
	, col.names = FALSE
	, quote = FALSE
	)
	