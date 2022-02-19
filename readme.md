# BlockChainPublic

## Install

1. Install the latest version of R, RStudio, git, and aws cli. Install C/C++ compilers. In Windows, you can install Rtools.
2. Clone this git repository to your local environment by running the following in the terminal:
  ```
  git clone git@github.com:kohei-kawaguchi/BlockChainPublic.git
  ```
3. As you clone the git repository, double click the `BlockChainpublic.Rproj` to open it as a project in RStudio. Ever code is written relative to this project root folder. By starting it as an RStudio project, you can always start with this projet root folder as the current working directory.
4. Test whether you can clean and rebuild the project as an R package by clicking the clean and rebuild command from the build tab.
![Clean and Rebuild](image/build.png)
5. If it succeeds, the build console will show the following message. You may be required to install some R packages, such as `RcppEigen` and `Rcpp`.
![Build Message](image/build_message.png)
6. You can load the library by `library(BlockChainPublic)` to call R functions defined in `R/` folder. C++ functions defined in `src/` folder is exported to R functions by `Rcpp`.
7. Download `cleaned/` folder and `output/` folder from aws s3 bucket by running the following command in the terminal:
```
aws s3 cp s3://blockchain-kawaguchi-noda-public/cleaned cleaned --recursive
aws s3 cp s3://blockchain-kawaguchi-noda-public/output output --recursive
```


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
