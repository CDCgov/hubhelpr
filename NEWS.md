# hubhelpr 0.1.0

This is the first release of `hubhelpr`.

Until now, any callers of `hubhelpr` had it installed from the default branch (`main`), so any merge in that branch changed what they produced and there was no record of which version produced a given output. `setup-hubhelpr` gained a `version` input (via PR #276), and this release is the first reference a caller can pin to.

Find below recent updates made to different parts of `hubhelpr`.

## Reports

* `generate_hub_report()` wraps the four report-writing calls the `generate-viz-data` action previously made separately (via PR #264).
* `write_ref_date_summary_all()` output gained the 10th and 90th quantiles and model designation flags (via PR #236).
* Weekly location exclusions, along with the minimum number of designated models required to report an ensemble, are read from machine-readable records in the hub reports repository (`cfa-forecast-hub-reports`). Regenerating an older reference date therefore applies the rules that date was published under rather than today's (via PRs #247 & #252).

## Model Designation

* `get_model_designation_current()` resolves designation from current model metadata, and `get_model_designation_as_of()` resolves it from the hub's weekly submission record for a past reference date (via PR #255).

## Target Data

* NHSN formatting is available on its own as `format_nhsn_data_as_hubverse()`, so archived snapshots are formatted exactly as current weekly pulls are. Observations before a series' first reported value are dropped while gaps in the interior of the data are kept (via PR #268).
* NSSP and NHSN target-data start dates are configurable, defaulting to including all available data (via PR #257).
* `derived_task_ids` is no longer hard-coded (via PR #267).

## Hubs & Infrastructure

* `hub_cloud_path()` looks up S3 bucket paths for supported hubs (via PR #234).
* New `s3-bucket-upload` action (via PR #239).
* The ensemble action no longer opens a duplicate pull request when it runs more than once for the same reference date (via PR #272).
