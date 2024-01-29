# scgUtils (development version 0.0.1)

Initial release of `scgUtils`, featuring a suite of functions for effective data visualisation and survey data analysis.

## New Features

* `colour_display`: A new function for visualizing colour options from specified palettes, enhancing text readability on coloured bars.
* `colour_pal`: Generates colour palettes for graphs, supporting sequential, diverging, categorical, and political palettes.
* `colour_prep`: Prepares a colour palette for visualizing data, assigning colours to unique values across specified columns in a data frame.


* `compile`: A new function that generates a comprehensive CSV or statistics data frame from survey data, combining crosstabs and/or statistics for each pair of row and column variables. It allows saving the results as a CSV file or returning them as a data frame.
* `crosstab`: A new function for generating crosstabs (contingency tables) from survey data. It provides options for formatting, including statistical measures, creating visual plots, and handling weighted data.


* `get_data`: a helper function for loading internal datasets.
* `get_file`: Enhanced file retrieval and preprocessing function, capable of handling files from local storage, OneDrive, web sources, and more.
* `survey dataset`: Integration of Wave 25 of the 2014-2023 British Election Study Internet Panel as a practice dataset within the package. This inclusion offers users a substantial dataset for practicing the package's functions and charts.


* `grid_vars`: A function for transforming binary grid survey questions into a longer format, enabling comparisons across different variables or groups.
* `grp_freq`: A function to calculate the frequency and optionally the percentage of a variable by groups in survey data, supporting both weighted and unweighted data.
* `grp_mean`: Provides the capability to calculate grouped mean (either unweighted or weighted) of a specified variable in survey data.


* `plot_bigfive`: A new function for visualizing Big Five personality traits using radar plots. Supports both individual and group comparisons and accommodates weighted data.
* `plot_binary`: A new function for visualizing binary survey responses using bar plots. Facilitates comparative analysis and supports optional grouping and weighting.
* `plot_popn`: A new function for creating visual representations of population profiles from survey data. Illustrates distributions of age groups across genders or other categories.
* `plot_sankey`: Introduces the ability to create interactive Sankey diagrams for illustrating data flow or transfer between different entities.


* `process_factors`: Function to remove unused levels from factor variables in a data frame, while preserving non-factor columns.
* `theme_scg`: Introduces a custom ggplot2 theme specifically designed for scg plots, offering a visually consistent and appealing style for data visualizations.

## Known Issues

* `plot_popn` occasionally displays incorrect labels when used with certain datasets. A fix is planned for the next release.
* Some users may experience compatibility issues with older versions of R.

## Future Developments

* Addition of `plot_parliament` and plots to visualise likert scale data.
* Addition of weighting functions, automating PowerPoint creation, and developing dashboards.
