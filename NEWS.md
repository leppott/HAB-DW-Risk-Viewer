HAB-DW-Risk-Viewer-NEWS
================
<Erik.Leppo@tetratech.com>
2025-08-22 14:34:56.63649

<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

    #> Last Update: 2025-08-22 14:34:56.667995

# HAB-DW-Risk-Viewer 0.3.0.9006 (2025-08-22)

- tests: Update prediction dataset tests to save missing values

# HAB-DW-Risk-Viewer 0.3.0.9005 (2025-08-22)

- tests: Check prediction datasets for all model variables

# HAB-DW-Risk-Viewer 0.3.0.9004 (2025-08-21)

- refactor: Update prediction data
  - Remove extra records from mean and median
  - Add column HABs_Treat_Score2
    - Value of 0

# HAB-DW-Risk-Viewer 0.3.0.9003 (2025-08-21)

- refactor: Update zip file to exclude any zip files in results
  directory

# HAB-DW-Risk-Viewer 0.3.0.9002 (2025-08-21)

- refactor: Update legend from colorFactor to colorQuantile

# HAB-DW-Risk-Viewer 0.3.0.9001 (2025-08-21)

- refactor: Change scenario data to prediction data files

# HAB-DW-Risk-Viewer 0.3 (2025-08-20)

- refactor: Bump version number for deliverable

# HAB-DW-Risk-Viewer 0.2.0.9004 (2025-08-20)

- refactor: Make summary figures and table reactive

# HAB-DW-Risk-Viewer 0.2.0.9003 (2025-08-20)

- refactor: Update spinner for leaflet (no longer needed)
- refactor: Update tooltips to bsplus package
- refactor: Add legend to map
- refactor: Add dummy file to results folder
  - Avoids an empty directory for when load to Shiny server

# HAB-DW-Risk-Viewer 0.2.0.9002 (2025-08-15)

- refactor: Add download button for summary and results

# HAB-DW-Risk-Viewer 0.2.0.9001 (2025-08-15)

- refactor: Modify map
  - Remove color outline for points
  - Filter points and polygons to remove those with NA values
- refactor: Update summary tab
  - Ensure table shows up
  - Add text above each element to describe the element

# HAB-DW-Risk-Viewer 0.2 (2025-08-15)

- refactor: Bump version number for deliverable

# HAB-DW-Risk-Viewer 0.1.0.9011 (2025-08-14)

- refactor: Update HUC layer pop up
  - Add waterbody and model

# HAB-DW-Risk-Viewer 0.1.0.9010 (2025-08-14)

- refactor: Updates maps
  - Move HUCS (points and polygons) to top to allow mouseover
  - Add control groups

# HAB-DW-Risk-Viewer 0.1.0.9009 (2025-08-14)

- refactor: Updates to Summary tab

# HAB-DW-Risk-Viewer 0.1.0.9009 (2025-08-13)

- refactor: Move states boundary file to RDS
- refactor: Create data-raw file to create states boundary
- refactor: Move ‘map’ tab to front so activates and always works
- refactor: Crop to state boundaries not bounding box

# HAB-DW-Risk-Viewer 0.1.0.9008 (2025-08-11)

- feature: Add summary plots and tables
  - placeholders only
- refactor: Add tigiris cache
- docs: Add missing packages from imported summary code

# HAB-DW-Risk-Viewer 0.1.0.9007 (2025-08-11)

- refactor: Add USEPA template for web
- refactor: Remove ‘crop’ button, leave in code
- docs: Add packages to DESCRIPTION

# HAB-DW-Risk-Viewer 0.1.0.9006 (2025-08-08)

- refactor: Use points and not polygons
- refactor: Update text on some tabs

# HAB-DW-Risk-Viewer 0.1.0.9005 (2025-06-13)

- nearly finished version, still have work to do

# HAB-DW-Risk-Viewer 0.1.0.9004 (2024-11-05)

- refactor: Finish setting up files as a proper repo, Issue \#1
- refactor: Set up dummy Shiny app (Old Faithful)
- refactor: Modify Shiny to use USEPA template, Issue \#2
  - Separate files for header, footer, css, tags
  - Use files from ContDataQC Shiny app

# HAB-DW-Risk-Viewer 0.1.0.9003 (2024-11-04)

- refactor: Add issue templates via web

# HAB-DW-Risk-Viewer 0.1.0.9002 (2024-11-04)

- docs: Update license in DESCRIPTION

# HAB-DW-Risk-Viewer 0.1.0.9001 (2024-11-04)

- refactor: Initial commit setting up package and shiny app
