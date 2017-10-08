# grabcite

[![CircleCI](https://circleci.com/gh/agrafix/grabcite.svg?style=svg)](https://circleci.com/gh/agrafix/grabcite)

GrabCite is a tool to generate data sets for tasks like citation recommendation. It supports various input formats such as:

* Plain Text (i.E. pdftotext output)
* PDF
* Grobid Tei XML
* Ice Cite JSON
* CiteSeerX Databases (experimental)
* TeX (+ Bib) Files

The output format is split into 3 files per input file, one containing the individual sentences and global citation markers, one containing meta information for citation markers and one containing citation markers for the paper itself. To use the tool, you can build it from source.

## Building from source

```bash
# install haskell stack
curl -sSL https://get.haskellstack.org/ | sh

# clone the repo
git clone https://github.com/agrafix/grabcite && cd grabcite

# install dependencies and build
stack setup
stack build

# run it
stack exec -- grabcite-datagen --help
```

## Command line interface

The main tool is called `grabcite-datagen`. It can be run from any linux command line. There are two supported use cases:

### Unpack an ArXiv.org dump

To unpack an arxiv.org dump (i.E. extract all archives and find the correct .tex/.bib file), you can run:

```bash
grabcite-datagen --in-mode InPdf --in-dir [DUMP_DIR] --recursive --arxiv-to-tex-mode --arxiv-meta-xml [LOCATION_OF_META_XML] --out-dir [TARGET_DIR] --jobs [JOBS]
```

### Generating a data set

To build a data set, first figure out what your input is shaped. As stated above we support:

* `InText`: Plain Text (i.E. pdftotext output)
* `InPdf`: PDF
* `InGrobid`: Grobid Tei XML
* `InIceCite`: Ice Cite JSON
* `InIceCiteBasic`: Ice Cite JSON, but ignore the roles detected
* `InCiteSeerX`: CiteSeerX Databases (experimental)
* `InTex`: TeX (+ Bib) Files

Then, run the command line:

```bash
grabcite-datagen --in-mode [MODE_FROM_ABOVE] --in-dir [DATA_DIR] --recursive --out-dir [OUT_DIR] --jobs 4
```

If you which to write the output to the database, add the following flags:

```bash
--out-db [POSTGRESQL_CONNECTION_STRONG] --db-mig-dir [MIGRATION_FILES] --data-source-name [NAME_OF_DATA_SOURCE]
```

`[MIGRATION_FILES]` is the path to the `database` folder in this repository. You can add `--db-overwrite` if you wish to overwrite previous data with the same `data-source-name`.

Experimental support for `InCiteSeerX` requires to supply `--in-db-conn-str` and point to the CiteSeerX Oracle DB. This has not properly been tested yet.

Add `--debug` to output debug messages.

### Hints

To speed up the process between runs, you can copy the `ref_cache.json` from a previous run into your new output directory before launching the task. This will prevent unneeded DBLP-ID lookups.
