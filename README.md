# Store Sales Forecast [R]

This repo contains the code for the Kaggle competition [Store Sales Time-Series Forecasting](https://www.kaggle.com/competitions/store-sales-time-series-forecasting/overview). 

## Setup

The dependencies are managed using [renv](https://rstudio.github.io/renv/articles/renv.html). 
To install the defined dependencies for this project, just run the following R code:

```r
# only if renv is not already installed
install.packages("renv")
renv::restore()
```

The package `catboost` is not tracked via `renv`. However, an installation of 
the used version `1.1.1` can be made with the following code:

```r
# Possible tgz files based on OS:
# Windows: catboost-R-Windows-1.1.1.tgz
# Linux: catboost-R-Linux-1.1.1.tgz
# macOS: catboost-R-Darwin-1.1.1.tgz
devtools::install_url(
  "https://github.com/catboost/catboost/releases/download/v1.1.1/catboost-R-Darwin-1.1.1.tgz",
  INSTALL_opts = c("--no-multiarch", "--no-test-load")
)
```

## Conventional Commits

`commit -m  "Tag MESSAGE"`

`commit -m "feat added linear regression to toolstack"`

| Type              | Content                         | 
| ----------------- | ---------------------------- | 
| fix               | Patches a bug in the codebase. | 
| feat              | Introduces a new feature to the codebase.  |
| test              | Adding missing tests or correcting existing tests. | 
| docs              | Adds, updates or revises documentation that is stored in the repository. | 
| ops               | Changes that affect operational components, like infrastructure, deployment, backup,  |
| refactor          | Refactoring existing code in the product, but without altering or changing existing behaviour in the product.  | 
| build             | Changes that affect build components or external dependencies, like build tool, ci pipeline, project version. | 
| perf              | Code changes that improves the performance or general execution time of the product but does not fundamentally change an existing feature.  |
| chore             | Includes a technical or preventative maintenance task that is necessary for managing the product or the repository, but it is not tied to any specific feature or user story e.g., modifying gitignore. | 
| style             | Changes, that do not affect the meaning of the code (white-spaces, formatting, missing semi-colons etc.)  | 
| revert            | Reverts one or more commits that were previously included in the product, but accidentally merged or serious issues were discovered that required their removal. |


## Folder Structure

```
├── 01_data
│   ├── intermediate              <- prepared data and other intermediate outputs
│   ├── raw                       <- provided raw data
│   └── results                   <- forecasting and evaluation results
├── 02_code                       <- R scripts
├── 03_figures                    <- relevant plots based on EDA and results
├── 04_reports                    <- RMarkdown reports for more details
├── R                             <- All defined R functions
├── README.md
├── renv                          <- contains renv related resources
├── renv.lock                     <- locked dependencies used for this project
└── store-sales-forecast-r.Rproj
```

The folder `01_data` is part of the `.gitignore` file. Therefore, you need to 
setup this folder by your own. You can download the needed data from the [Kaggle Competition](https://www.kaggle.com/competitions/store-sales-time-series-forecasting/overview).