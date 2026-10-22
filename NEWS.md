# hubhelpr 0.2.0

## Breaking Changes

- Removed the `nssp_update_local` argument from `update_hub_target_data()` and `get_hubverse_format_nssp_data()` and the matching input from the `update-target-data` composite action. NSSP emergency department visit data is now always pulled from the `data.cdc.gov` endpoint. The hub-hosted `auxiliary-data/nssp-raw-data/latest.parquet` that this argument read was removed from both hubs on 2026-10-12. Callers passing `nssp_update_local` now error on an unused argument. Those passing `FALSE`, including both hubs' `update-target-data.yaml`, get unchanged behaviour.
- `write_viz_target_data(use_hub_data = FALSE)` previously read NSSP data from that same local file. It now pulls from `data.cdc.gov`.

# hubhelpr 0.1.0

This is the first release of `hubhelpr`.

Until now, any callers of `hubhelpr` had it installed from the default branch (`main`), so any merge in that branch changed what they produced and there was no record of which version produced a given output. `setup-hubhelpr` gained a `version` input (via PR #276), and this release is the first reference a caller can pin to.
