# 0.2.5

* Cache results only for 1 hour, could be important when run on servers.
* Add notice about caching results for 1 hour across all connected functions.
* Add additional description for validation function, result structure.
* Change the order in `README` file.
* Optional `lifeduration` and `checkred` for all validation functions.

# 0.2.4

* Polish descriptions.
* Deployment to R CRAN.
* Update `NEWS` file.

# 0.2.3

* Updated DESCRIPTION file description.
* Updated `README` file.
* Fixed `pac_true_size` with used `exclude_joint` argument, should not count checked package dependencies.
* Secured against duplicates in `pac_compare_versions`.
* Removed the "Description" column from a `pac_timemachine` result.
* Added the `reverse` argument for `pac_deps` which working for description versions too.
* Added the `repos` argument for `pac_deps`.
* Remove `base` argument in `pac_true_size`, as not see any value added.
* Add `checkred` variable for validation functions.
* `pac_health` for newest release younger than x days, checking if package is red labeled on CRAN checks pages.
* Default 14 days as limit for healthy version, and non red check for the newest version.
* Added new functions `pac_checkred`/`pacs_checkred`.

# 0.2.2

* Fixed `pac_deps` when `description_v = TRUE`, minimal required versions were taken from all local DESCRIPTION files. This will fix `pac_validate`/`pacs_validate` too, which were to optimistic.

# 0.2.1

* Added `https` for all URL.

# 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Added useful connected packages to Suggests.
* Added `roxygen2` to all exported functions.
* Achieved 80% of coverage.
* Written a clear `README` file.
* Removed all Imports from the `DESCRIPTION` file.
* Added `memoise` dependency to reduce the number of web calls.
* Created a decent basket of `pac`/`pacs` prefix functions.
* Removed all `\dontrun` calls.

