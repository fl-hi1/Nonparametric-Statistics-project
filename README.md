
<!-- omit from toc -->
# Nonparametric-Statistics-project
Analysis of data on smoking trends and effects of regulatory polices on OECD countries

This project was developed for the course of **Nonparametric Statistics** for the MSc. in Mathematical Engineering at Politecnico di Milano, A.Y. 2022/2023.

<!-- omit from toc -->
# Table of contents

- [Installation](#installation)
  - [How to clone the repository](#how-to-clone-the-repository)
  - [How to install the packages](#how-to-install-the-packages)
  - [How to compile the PDF files](#how-to-compile-the-pdf-files)
- [Running the analysis](#running-the-analysis)
- [Final results](#final-results)
- [Authors](#authors)


# Installation

## How to clone the repository

```
git clone https://github.com/fl-hi1/Nonparametric-Statistics-project.git
git submodule update --init
git submodule update --recursive
```



## How to build the `IFWEEVERADDANYPACKAGE` package

Open the `./data/path/to/myRproject.Rproj` in RStudio and type:

- `Ctrl+Shift+B` on Windows
- `CMD+Shift+B` on macOS



## How to install the packages

Install the required packages from CRAN

```
packages_list <-
    c(
        "tidyverse",
        
    )
install.packages(packages_list)
```


## How to compile the PDF files

To compile the presentations, run the following in the root of the repo

```
make prese1
make prese2
make prese3
```

To compile the report, run

```
make report
```

To compile everything, run

```
make pdf
```

To remove temporary `LaTeX` files, run

```
make clean
```

To remove both temporary and pdf files, run

```
make distclean
```

# Running the analysis

The repository contains different files to perform the analysis

- `01_XXXX.Rmd` is a notebook containing XXXX.


# Final results

The final presentations can be found here:


The final report can be found here:


The results from the simulations knitted can be found here:


# Authors


- Flavia Petruso ([@fl-hi1](https://github.com/fl-hi1))
- Valentin Lacombe
