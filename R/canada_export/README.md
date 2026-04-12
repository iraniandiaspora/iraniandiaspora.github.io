# R/canada_export/ — reference copies

These are **reference copies** of the upstream Canada pipeline scripts. They
are here to document how the intermediate CSVs under `data/canada/` are
produced from raw 2021 Canadian Census PUMF microdata.

**These scripts cannot be run from the deployment repo.** They reference
`here("data/pumf_individual/sub-data.txt")`, which is a symlink inside the
upstream `IDD_Canada/` repository (not a path inside this deployment repo).
The raw PUMF is 738 MB and is not committed here — it lives outside version
control in the project's shared `_data/` directory.

To regenerate any of the Canada intermediate CSVs, run the equivalent script
in `../IDD_Canada/R/` instead:

```
cd ../IDD_Canada
Rscript R/generate_province_distribution.R
Rscript R/final_iranian_population_breakdown.R
# ...etc
```

Outputs land in `../IDD_Canada/output_*_page/` and are then copied by hand
into the corresponding subdirectories under `iraniandiaspora.github.io/data/canada/`.

## Narrow identification rule

The aligned scripts use `filter_iranians_narrow()` from
`../IDD_Canada/R/comprehensive_iranian_variables_correct_pumf.R`. The rule is:

```
born in Iran
OR  Iranian ethnic origin
OR  (Persian mother tongue or Persian at home
     AND second/third generation
     AND West Asian visible minority)
```

This sums to ~223,968 weighted (the headline shown on `ca-population`).
A plain union of the four criteria gives ~240,189, which includes Afghan
Dari, Tajik, and other Central Asian Persian speakers who are not Iranian.

## Files in this directory

Every `.R` file here mirrors the corresponding file in `../IDD_Canada/R/`.
If you update either copy, remember to update the other.
