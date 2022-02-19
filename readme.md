# BlockChainPublic

## Install

1. Clone this git repository to your local environment by:



## Folder Structure

This is a replication package for "[Security-Cost Efficiency of Competing Proof-of-Work Cryptcurrencies](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3974376)".

The folder structure is 

- `cleaned`: Store cleaned data.
- `output`: Store transformed data nd other outputs of the analysis.
- `main`: Store code for cleaning, transforming, and analyzing the data.
- `report`: Store reporting Rmd documents.
- `R`: Store R function definitions.
- `src`: Store C++ function definitions.

The files `DESCRIPTION` and `NAMESPCE` describe the meta data of this package. It does not contain the raw data, because the raw data is too large. All analysis can be replicated based on the cleaned data.
