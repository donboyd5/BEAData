# BEAData

This package contains several BEA data files that I use frequently. I will try to update the quarterly files once each quarter, and the annual files once each year. Datasets:

* `nipa.a`: NIPA annual data, organized by individual tables (a variable can appear more than once)
* `nipa.au`: NIPA annual data, unique - one observation per variable per year

Install it from github with:
  
```{r}
devtools::install_github("donboyd5/BEAData")
```
