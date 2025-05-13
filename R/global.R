# Global.R

# =============================================================================
# Global configuration of the EDA application.
# Loads configurations, common functions, and constants.
# =============================================================================
library(lubridate)

# Load configurations
source("R/Config/Config.R", local = TRUE)
source("R/Config/Constants.R", local = TRUE)
source("R/Config/Global_functions.R", local = TRUE)

# Load common modules
source("R/Modules/Common/data_filtering.R", local = TRUE) 
source("R/Modules/Common/s_curve_helpers.R", local = TRUE)
source("R/Modules/Common/s_curve_charts.R", local = TRUE)

log_message("✅ global.R file loaded successfully.")
