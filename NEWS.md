# OrgHeatmap 0.1.2

*   Resubmission to CRAN addressing feedback from the CRAN team.
*   **CRAN-required changes:**
    *   Reformatted the `Description` field in the DESCRIPTION file to a single continuous paragraph.
    *   Added a reference to the methodological preprint: `Zhou et al. (2022) <doi:10.1101/2022.09.07.506938>`.
    *   Replaced `\dontrun{}` with `\donttest{}` in function examples and unwrapped executable examples as per CRAN policy.

# OrgHeatmap 0.1.1

*   Initial submission, first release to CRAN.
*   Fixed the logic for color parameter validation in the `OrgHeatmap()` function, resolving the ERROR that caused test failures.
*   Added more comprehensive color format checks (Hex codes and color names).
*   Rebuilt the LICENSE file.
*   Added the `.github` directory to the `.Rbuildignore` file as per CRAN policy.

# OrgHeatmap 0.1.0

*   Initial version released on GitHub.
*   Provided the `OrgHeatmap()` function for drawing organ expression heatmaps.
*   Included example datasets and detailed usage documentation.