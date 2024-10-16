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
