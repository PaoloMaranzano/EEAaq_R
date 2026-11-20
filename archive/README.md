# Archive

Material of past versions of EEAaq, kept for reference. Nothing here is part
of the package (the folder is in `.Rbuildignore`) and nothing here is read
by the code.

| Folder | Content |
|---|---|
| `releases/` | Source tarballs of EEAaq 1.0.0, 1.0.1, 1.0.2 and 1.0.3 as built for CRAN. The official copies of every release, 0.0.1 included, are in the [CRAN archive](https://cran.r-project.org/src/contrib/Archive/EEAaq/). |
| `releases/backup-copies/` | Two alternative builds kept by the maintainer: 1.0.0 with its vignette, and a second build of 1.0.1. |
| `vignettes-2025/` | The static (PDF) and dynamic (HTML, leaflet maps) vignettes written for version 1.0.0 in 2025, with their R Markdown sources. |
| `scripts/` | Debugging scripts of 2025: `Debug.R` and the February 2025 draft of the new `EEAaq_get_data()` by Samir Doghmi. |
| `EEAaq-v0/` | The original repository of the project (github.com/AgostinoTassanMazzocco/EEAaq, 2023-2024), merged here with its full git history: the sources of versions 0.0.x (`EEAaq/`) and the experiments on missing-data imputation and the related simulations (`NA Imputation/`). Versions 0.0.x download `LAU.rds`, `NUTS.rds` and `stations_reshaped.rds` from that repository, so it is archived on GitHub (read-only) rather than deleted. |

## In the git history only

Some files were removed from the working tree in the reorganisation of
November 2026 but remain in the history of this repository:

* the tag **`legacy-layout-2026-11-20`** is the repository as it was before
  the reorganisation: the snapshots `Backup/Package - January 2025`,
  `February 2025` and `January 2026`, the folder `Package/` with version
  1.0.3, the rendered vignettes and their figures;
* the tag **`v1.0.3`** is the commit of the package sources published on CRAN
  as 1.0.3 (identical R code);
* the commits of Samir Doghmi (February-August 2026) are the development
  history of 1.0.3 and 1.0.4;
* the LAU, NUTS and station files of 2023-2024 and the simulation results
  (`.RData`) of `EEAaq-v0/` are in the history of that folder.

```sh
git show legacy-layout-2026-11-20:"Backup/Package - January 2026/R/EEAaq_get_data.R"
git log --oneline -- archive/EEAaq-v0
```

## Outside the repository

The project folder on the maintainers' shared drive keeps the material that
was never in git: the thesis on which version 0.0.x is based, the conference
abstracts and presentations, and the simulation studies on missing-data
imputation. The LaTeX sources of the 2024 presentation and of the first
vignette are on Overleaf.
