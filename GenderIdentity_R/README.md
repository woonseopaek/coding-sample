# Woonseo Paek, R Coding Sample

This is part of the replication package for "How Do Marriage Markets Respond to Relative Income Under Gender Identity Norms?: Evidence from South Korea". 

This sample covers only the R code that builds the dataset and estimates main results (Table 1). The code in this sample was entirely written by me.

A summary of this paper is forthcoming in Atlantic Economic Journal under a different title: 'Marriage Markets and Relative Income Under Gender Identity Norms'. 


## Structure

```
.
├── README.md
├── Code/
│   ├── valuesRaw.R               loads and merges the 2008–2024 raw survey files
│   └── Main.R                    constructs variables, estimates potential wages, runs regressions
├── Data/
│   ├── raw/                      MDIS R scripts and data files (not included)
│   │   ├── 2008_연간자료(A형_시군_중분류)_....R
│   │   ├── 2008_연간자료(A형_시군_중분류)_....csv
│   │   ├── ...
│   │   ├── 2024_하반기(C형_시도_중분류_부가항목)_....R
│   │   └── 2024_하반기(C형_시도_중분류_부가항목)_....csv
│   └── derived/
│       └── valuesRaw.csv         created by Code/valuesRaw.R (not included)
└── Output/
    └── regression_table.tex      created by Code/Main.R
```

## How to run
0. Download the data (see Data below). 
1. Set `dir` at the top of both scripts in `Code/` to the project folder.
2. Run `Code/valuesRaw.R` ->  `Code/Main.R`.

Requires R (≥ 4.0) with `readr`, `dplyr`, `stringr`, `tidyr`, `fixest`, and `vtable`. Takes approximately 15-20 minutes to run. 

## Data

The data are not included, as I am not authorized to re-distribute them. Instead, they are public-use microdata and can be downloaded following the process below. 

To download:
1. Sign up and log in to MDIS (https://mdis.kostat.go.kr), then go to *Download Service* (공공용 다운로드).
2. Select *Local Area Labour Force Survey* and extract each year in R format:
   - 2008–2009: annual data, Type A (시군)
   - 2010–2012: Q4 data, Type A (시군)
   - 2013–2024: second-half data, Type C (시도, with supplementary items)
3. Put the downloaded R scripts and data files in `Data/raw/`.
