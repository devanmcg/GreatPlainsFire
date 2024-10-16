devtools::install_github("r-spatial/rgee")

rgee::ee_install() # Installs a Python environment

pacman::p_load(reticulate, rgee)

Sys.setenv("RETICULATE_PYTHON" = "/usr/bin/python3")

# Set Google Cloud SDK. Only need it the first time you log in. 
Sys.setenv("EARTHENGINE_GCLOUD" = "home/ee-devanmcg/google-cloud-sdk/bin/")
ee_Authenticate() 

# Init your Earth Session  
ee_Initialize()

ee_Initialize(drive = TRUE) # Initialize the Python Environment  (ROOT: users/Public)

# Install geemap in the same Python ENV that use rgee
py_install("geemap") 
gm <- import("geemap")

ee_install_upgrade() # Upgrade the GEE API


# Google Cloud Storage

SaK_file <- "C:/Users/devan.mcgranahan/OneDrive - USDA/ee-devanmcg-rgee.json"

# Assign the SaK to a EE user.
ee_Initialize('ee-devanmcg', drive = TRUE, gcs = TRUE)
install.packages('googleCloudStorageR')
ee_utils_sak_copy(
  sakfile =  SaK_file
)

# Validate your SaK
ee_utils_sak_validate()


ee_clean_user_credentials('ee-devanmcg')
