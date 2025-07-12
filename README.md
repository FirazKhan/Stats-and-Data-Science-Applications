# MTHM503 Data Science Project

## Reproducible Data Science Analysis

This project demonstrates professional data science practices using `targets` for reproducible workflow management and `renv` for dependency management.

## Project Overview

This project contains three main analytical tasks:

1. **Task 1: Supervised Classification** - Pedestrian casualty severity prediction
2. **Task 2: Regression** - Fire rescue extrication analysis  
3. **Task 3: Unsupervised Learning** - Olive oil composition analysis

## Quick Start

### Prerequisites

- R (version 4.0 or higher)
- Required R packages (managed by `renv`)

### Setup

1. **Clone the repository:**
   ```bash
   git clone <repository-url>
   cd mthm503
   ```

2. **Install dependencies:**
   ```r
   renv::restore()
   ```

3. **Run the complete analysis:**
   ```r
   source("run_analysis.R")
   ```

## Project Structure

```
mthm503/
├── _targets.R                 # Main targets pipeline configuration
├── run_analysis.R             # Main script to run complete analysis
├── visualize_workflow.R       # Workflow visualization script
├── R/                         # R functions for each task
│   ├── functions.R           # Original utility functions
│   ├── load_data.R           # Data loading functions
│   ├── utils.R               # Utility functions
│   ├── task1_classification.R # Task 1: Classification functions
│   ├── task2_regression.R     # Task 2: Regression functions
│   └── task3_unsupervised.R   # Task 3: Unsupervised learning functions
├── tests/                     # Unit tests
│   ├── test_load_data.R      # Original tests
│   └── test_analysis.R       # Comprehensive analysis tests
├── vignettes/                 # Reports and documentation
├── renv/                      # R environment management
├── renv.lock                  # Locked package versions
└── README.md                  # This file
```

## Running Individual Tasks

### Task 1: Classification
```r
# Run only classification task
targets::tar_make(classification_results)
```

### Task 2: Regression
```r
# Run only regression task
targets::tar_make(regression_results)
```

### Task 3: Unsupervised Learning
```r
# Run only unsupervised learning task
targets::tar_make(unsupervised_results)
```

## Workflow Management

### Visualizing the Workflow
```r
# Interactive network diagram
tar_visnetwork()

# Or use the visualization script
source("visualize_workflow.R")
```

### Checking Target Status
```r
# See which targets are up to date
tar_glimpse()

# See target progress
tar_progress()

# See outdated targets
tar_outdated()
```

### Cleaning and Rebuilding
```r
# Clean all targets
tar_destroy()

# Clean specific targets
tar_delete(target_name)

# Rebuild everything
tar_make()
```

## Testing

### Run All Tests
```r
testthat::test_dir("tests/")
```

### Run Specific Test Files
```r
testthat::test_file("tests/test_analysis.R")
testthat::test_file("tests/test_load_data.R")
```

## Data Sources

The project uses the following data sources from the Supabase database:

- **stats19_casualties** - Road accident casualty data
- **stats19_accidents** - Road accident details
- **stats19_vehicles** - Vehicle information
- **fire_rescue_extrication_casualties** - Fire rescue data
- **stats19_by_financial_year** - Annual collision statistics
- **olive_oil** - Olive oil composition data

## Analysis Details

### Task 1: Supervised Classification
- **Objective**: Predict pedestrian injury severity (Fatal/Serious/Slight)
- **Models**: Random Forest vs Decision Tree
- **Evaluation**: Accuracy, Kappa, AUC, Confusion Matrix
- **Features**: 28 variables including casualty, accident, and vehicle characteristics

### Task 2: Regression
- **Objective**: Analyze age and sex effects on fire rescue extrication rates
- **Models**: GLM (Poisson) vs GAM
- **Evaluation**: AIC, Incidence Rate Ratios, Overdispersion
- **Features**: Age bands, sex, with realistic mock age data

### Task 3: Unsupervised Learning
- **Objective**: Understand natural variation in olive oil composition
- **Methods**: PCA + K-means + Hierarchical clustering
- **Evaluation**: Silhouette scores, cluster profiles
- **Features**: 8 fatty acid composition variables

## Dependencies

All dependencies are managed through `renv`. The `renv.lock` file contains the exact versions of all packages used in this analysis.

### Key Packages
- `targets` - Workflow management
- `renv` - Environment management
- `testthat` - Unit testing
- `dplyr`, `ggplot2` - Data manipulation and visualization
- `caret`, `randomForest`, `rpart` - Machine learning
- `mgcv` - Generalized additive models
- `cluster` - Clustering algorithms

## Reproducibility

This project ensures full reproducibility through:

1. **Targets Pipeline** - Automatic dependency tracking and caching
2. **renv** - Exact package version management
3. **Unit Tests** - Automated quality checks
4. **Modular Functions** - Reusable and testable code
5. **Documentation** - Clear workflow and function documentation

## Troubleshooting

### Common Issues

1. **Package Installation Errors**
   ```r
   # Reinstall renv
   install.packages("renv")
   renv::restore()
   ```

2. **Database Connection Issues**
   - Check internet connection
   - Verify database credentials in functions

3. **Target Build Errors**
   ```r
   # Clean and rebuild
   tar_destroy()
   tar_make()
   ```

4. **Test Failures**
   ```r
   # Run tests with verbose output
   testthat::test_dir("tests/", reporter = "verbose")
   ```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Run the complete test suite
6. Submit a pull request

## License

This project is licensed under the GNU General Public License v3.0 - see the LICENSE file for details.

## Contact

For questions or issues, please contact the project maintainer or create an issue in the repository.

---

**Note**: This project demonstrates professional data science practices suitable for academic coursework and real-world applications.

