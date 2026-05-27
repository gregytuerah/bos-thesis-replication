# Data Access and Placement

## Indonesia Family Life Survey

This project uses public-use files from the Indonesia Family Life Survey
(IFLS). The data are distributed by RAND to registered users. Under RAND's
public-use conditions, the IFLS data files cannot be redistributed through
this repository, and each collaborator who accesses the data must register.

Register for and download IFLS data through:

- [RAND IFLS access page](https://www.rand.org/well-being/social-and-behavioral-policy/data/FLS/IFLS/access.html)

## Required Files

After downloading the appropriate IFLS waves/modules, place the following files
under `Data/Raw/` with this exact relative structure:

```text
Data/Raw/
├── 00_bk_ar1.dta
├── 07_bk_ar1.dta
├── 07_bk_sc.dta
├── 14_bk_ar1.dta
├── 14_bk_sc1.dta
└── More on IFLS/
    ├── cf07_all_dta/
    │   └── schl.dta
    ├── pce-1993-1997_2000-2007/
    │   └── pce07nom.dta
    └── hh14_all_dta/
        ├── b1_ks0.dta
        ├── b1_ks1.dta
        ├── b1_ks2.dta
        ├── b1_ks3.dta
        └── b2_kr.dta
```

## Files Produced Locally

Running `Rscript Code/run_main_thesis_pipeline.R` constructs intermediate
record-level analysis files in `Data/Processed/`. That directory is excluded
from version control because the files are derived from IFLS individual and
household records.

The repository includes only reported tables, figures, and manuscript files,
not IFLS raw files or record-level derived data.
