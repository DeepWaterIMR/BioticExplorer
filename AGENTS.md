# AGENTS.md

This file provides guidance to Codex (Codex.ai/code) when working with code in this repository.

## Package Overview

**BioticExplorer** (v0.7.1) is a Shiny web application for exploring and analyzing IMR (Institute of Marine Research) NMD Biotic data. It operates in two modes:

- **File mode**: Loads local NMD Biotic v3 XML files directly (no external dependencies)
- **Database mode**: Connects to a DuckDB database created and maintained by the companion package **BioticExplorerServer**. Database mode is auto-detected at startup by `findBesDatabase()` in `R/other_functions.R`, which checks `$BES_DB_PATH`, then `~/IMR_biotic_BES_database/bioticexplorer.duckdb`, and on Windows also `%USERPROFILE%\IMR_biotic_BES_database\bioticexplorer.duckdb` (the current BioticExplorerServer and BAIT default, because R expands `~` through the OneDrive-redirected Documents folder there).

## Running the App

```r
# From local clone
shiny::runApp("/path/to/BioticExplorer")

# From GitHub
shiny::runGitHub("BioticExplorer", "MikkoVihtakari")
```

There are no automated tests. There is no `devtools::check()` workflow — this is a Shiny app package, not a library package.

## Architecture

### File Layout

```
app.R                          # Entire UI + server (1,620 lines) — main entry point
R/
  processBiotic_functions.R    # XML parsing and table merging
  filtering_functions.R        # Filter UI updates, filter chain construction, map updates
  figure_functions.R           # All plot/map functions (~1,200 lines)
  other_functions.R            # Small utilities (se, tryCatchWE, loadingLogo, etc.)
  install_requirements.R       # Installs missing CRAN/GitHub packages on first run
www/
  logo.png / logo_bw.png       # IMR logo; bw version shown while app is busy
```

### Core Data Structures

Three reactive data frames (`rv$stnall`, `rv$indall`, `rv$mission`) flow through the entire app:

| Table | Source | Contents |
|-------|--------|----------|
| `stnall` | mission + fishstation + catchsample merged | Station- and catch-level data; used for maps, overview plots |
| `indall` | stnall + individual + agedetermination merged | Individual fish measurements; used for species analysis |
| `mission` | mission element only | Cruise metadata; used for cruise overview tab |

These are populated either by `processBioticFile(s)()` (file mode) or by dplyr queries on lazy DuckDB tables (database mode), then stored in `rv`.

### Key Design Points

- **Two-mode switching**: `rv$uploadDbclicked` tracks database vs. file mode. The "Upload DB" tab (server mode) and "Upload" tab (file mode) populate the same `rv$stnall/indall/mission` reactives, so all downstream tabs are mode-agnostic.
- **Dynamic filter chain**: `makeFilterChain(db = TRUE/FALSE)` builds a list of `rlang::parse_exprs()` expressions from UI inputs; these are applied via `dplyr::filter()` to either in-memory data.tables or lazy DBI tables.
- **Database connection**: Read-only DuckDB connection opened at startup if the database file exists. Lazy `dplyr::tbl()` references are used; `.collect()` is called only after the user clicks "Fetch data".
- **Plot pre-computation**: `speciesOverviewData()` and `individualFigureData()` pre-aggregate data once after each load/filter action; individual plot functions receive the pre-computed output, not raw data.
- **Loading indicator**: `loadingLogo()` injects JavaScript to swap the header logo to a greyscale version while Shiny is busy.
- **Session persistence**: Users can download filtered data as RDS and re-upload it to resume a session without re-querying the database.

### BioticExplorerServer Dependency

Database mode requires BioticExplorerServer to have run `compileDatabase()` to populate the DuckDB file and the `dbIndex.rda` index. The index drives all database-mode filter dropdowns. Without BioticExplorerServer, only file mode is available.
