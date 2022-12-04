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

## Commit Style

`commit -m  "Tag MESSAGE"`

`commit -m "MOD Added linear regression to toolstack"`

Where to commit? (Tag)

* **PPL**   - Pipeline
* **MOD**   - Model
* **SEL**   - Feature selection
* **EXP**   - Exploration
* **DAT**   - Data related
* **ORG**   - Organisation


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