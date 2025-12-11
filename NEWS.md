# OrgHeatmap 0.3.2
*   Fixed the issue where mouse organ contours were incorrectly separated from the body outline; organs now correctly connect to the body silhouette.


# OrgHeatmap 0.3.1
*   Resubmission to CRAN addressing feedback from the CRAN team.
*  **CRAN-required changes:**
    * Corrected spelling in DESCRIPTION field: fixed "pre", and "supportscustomization"
    * Addressed potential spelling issues by including a WORDLIST file for terms: 'subcellular'
    * Updated Title field to proper title case: "Visualization Tool for Numerical Data on Human/Mouse Organs and Organelles"
    * Removed non-standard file 'organelle_expression_plot.png' from package directory
    
    
# OrgHeatmap 0.3.0
*   **Added subcellular organelle visualization support** (new visualization type "organelle").
*   Integrated subcellular organelle coordinate data to enable plotting of subcellular contours.
*   Updated filtering functionality to support organelle type-based filtering (complementary to existing organ system filtering for human/mouse).
*   Extended data type compatibility to better support subcellular-level data (e.g., protein abundance in organelles).
*   Maintained consistency with existing human/mouse organ visualization workflows while adding organelle-specific options.


# OrgHeatmap 0.2.0
*    Added mouse organ visualization support (new species "mouse")
*    Updated organ system mapping for mouse (mouse_organ_systems)
*    Added mouse-specific example data in extdata/


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