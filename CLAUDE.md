# scrape-swish

R script that scrapes swimming class availability from Swish Swimming's iClassPro API and exports it as CSV for use in Google Sheets.

## How to run

```sh
Rscript scrape-swish.R
```

Output goes to `./swish-sessions.csv` (configurable via `output_path` at the top of the script).

## Dependencies

R packages: `httr`, `rvest`, `jsonlite`, `data.table`, `purrr`

## API endpoints

All under `https://app.iclasspro.com/api/open/v1/swishswimming/`:
- `classes?locationId=1&limit=N&page=N` — paginated class listings
- `class-programs/1` — locations (mapped via `programId`)
- `levels/active/1` — skill levels (mapped via `levelId`)

## Key patterns

- `parse_api_list()` converts the API's nested list responses into data.tables
- `check_response()` validates HTTP responses and stops with a clear error on failure
- Location names are remapped to short forms via `loc.remap` for the Google Sheets helper columns
