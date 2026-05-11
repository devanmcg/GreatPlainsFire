pacman::p_load(tidyverse, magrittr, readxl, sf, terra, tidyterra)

load('../albersEAC.Rdata')

# Function to calculate NBR 
    calc_nbr <- function(nir, swir) {
      return((nir - swir) / (nir + swir))
    }

# Load Rx fire data
  # burn & imagery dates 
    rx_dates <-read_xlsx("S:/DevanMcG/FireScience/Sentinel/SeverityComparison/ImageDates.xlsx", 
                         'PreBurnPostDatesRxND') %>%
                mutate(across(c(BurnDate:PostBurn), ~ as.character(.x)))
  # spatial & fire data for the thermocouples 
    rx_sf <- st_read('./data/ThermocouplePlacements.gpkg', 
                     'AllBurnData') %>%
              mutate(BurnDate = as.character(date) ) %>%
              select(-date) %>%
              full_join(by = c('location', 'BurnDate'), 
                        rx_dates) %>%
              st_transform(4326)

# Imagery
  rx_dir = 'S:/DevanMcG/FireScience/Sentinel/SeverityComparison/Rx'
  
  rx_images <- 
    tibble(file = list.files(rx_dir, pattern = "\\.tiff$", ignore.case = TRUE) ) %>% 
    mutate(info = str_remove(file, '.tiff')) %>%
    separate(info, into = c('location', 'ImageDate', 'type'), sep = '_') %>%
    mutate(location = replace_values(location, 'DAY' ~ 'Dayton')) 

  RxSeverity <- tibble() 

##
## Location-specific corrected dNBR rasters
##
  
# Change manually and send loop on its way
  loc = 'CGREC'
  loc = 'HREC'
  loc = 'Dayton'
{
  begin = Sys.time()
  rx_d <- filter(rx_dates, location == loc)
  lc = tolower(loc)
  loc_sf <- st_read('./data/OriginalRxBurnPerims.gpkg', lc, quiet = TRUE) %>%
            st_transform(4326) 
  tc_zones <- filter(rx_sf, location == loc) %>%
              mutate(area = st_area(.) ) %>% 
              group_by(burn) %>%
              summarise(area = sum(area))  %>%
              st_centroid() %>%
              st_buffer(100) 

  dNBR <- list() 
    for(i in 1:length(rx_d$BurnDate)) {
      date = slice(rx_d, i)

    #
    # Step 1: Create cloud & shadow mask from Scene Classification Layers
    #
    # Load, wrangle SCL as output by Copernicus GUI
      # Pre-fire SCL 
        pre_scl = filter(rx_images, location == loc, 
                                    type == 'SCL', 
                                    ImageDate == date$PreBurn)$file
        PreSCL <- paste0(rx_dir, '/', pre_scl) %>% 
                      rast() 
        names(PreSCL) <- c('r', 'g', 'b', 'dm')
        PreSCL %<>%
          mutate(sc = case_when(
            r == 0 & g == 0 & b  == 0 ~ 0, 
            r == 255 & g == 0 & b== 0 ~ 1, 
            r == 47 & g == 47 & b == 47 ~ 2,
            r == 100 & g == 50 & b == 0 ~ 3,
            r == 0 & g == 161 & b == 0 ~ 4,        # 160 -> 161
            r == 255 & g == 231 & b == 90 ~ 5,     # 230 -> 231
            r == 0 & g == 0 & b == 255 ~ 6, 
            r == 129 & g == 129 & b == 129 ~ 7,   #  128 -> 129
            r == 193 & g == 193 & b == 193 ~ 8,   #  192 -> 193
            r == 255 & g == 255 & b == 255 ~ 9, 
            r == 100 & g == 201 & b == 255 ~ 10,  # g 200 -> 201
            r == 255 & g == 150 & b == 255 ~ 11, 
            TRUE ~ 99 ), 
            sc = as.ordered(sc)) %>%
          select(sc)
      # Post fire SCL
        post_scl = filter(rx_images, location == loc, 
                          type == 'SCL', 
                          ImageDate == date$PostBurn)$file
        PostSCL <- paste0(rx_dir, '/', post_scl) %>% 
                    rast() 
        names(PostSCL) <- c('r', 'g', 'b', 'dm')
        PostSCL %<>%
          mutate(sc = case_when(
            r == 0 & g == 0 & b  == 0 ~ 0, 
            r == 255 & g == 0 & b== 0 ~ 1, 
            r == 47 & g == 47 & b == 47 ~ 2,
            r == 100 & g == 50 & b == 0 ~ 3,
            r == 0 & g == 161 & b == 0 ~ 4,        # g 160 -> 161
            r == 255 & g == 231 & b == 90 ~ 5,     # g 230 -> 231
            r == 0 & g == 0 & b == 255 ~ 6, 
            r == 129 & g == 129 & b == 129 ~ 7,   #  128 -> 129
            r == 193 & g == 193 & b == 193 ~ 8,   #  192 -> 193
            r == 255 & g == 255 & b == 255 ~ 9, 
            r == 100 & g == 201 & b == 255 ~ 10,  # g 200 -> 201
            r == 255 & g == 150 & b == 255 ~ 11, 
            TRUE ~ 99 ), 
            sc = as.ordered(sc)) %>%
          select(sc)
        # Define classifications to exclude
          ex_sc <- c(0, 2, 3, 7:11) %>% as.ordered() 
        # Get the excluded classifications for each image
          pre_mask <- 
            PreSCL %>%
            mutate(sc = ifelse(sc %in% ex_sc, 1, 0)) 
          post_mask <- 
            PostSCL %>%
            mutate(sc = ifelse(sc %in% ex_sc, 1, 0)) 
          
        # Combine pre and post masks
          CombinedMask <- pre_mask + post_mask 
        # Add back in TC sample areas in case they got excluded
          CombinedMask %<>%  mutate(sc = ifelse(sc >= 1, 1, 0))
          
          tc_pixels <- tc_zones %>% 
                    rasterize(CombinedMask, background = 0) %>%
                    mutate(layer = ifelse(layer == 1, -1, 0)) 
          CombinedMask2 <- CombinedMask + tc_pixels
          CombinedMask2 %<>% mutate(sc = ifelse(sc <= 0, 0, 1),
                                    sc = ifelse(sc >= 1, NA, 1))
    #
    # Step 2: PIF correction on post-fire image
    #
      ## Load raw bands
        # pre image
          pre_raw = filter(rx_images, location == loc, 
                           type == 'RAW', 
                           ImageDate == date$PreBurn)$file
          pre_path = paste0(rx_dir, '/', pre_raw)
          pre_ras <- rast(pre_path) %>% 
                        mask(., CombinedMask2)
          pre_ras <-  (pre_ras-10000)/10000
          names(pre_ras) <- c('green', 'red', 'nir', 'swir')
        # post-fire
          post_raw = filter(rx_images, location == loc, 
                            type == 'RAW', 
                            ImageDate == date$PostBurn)$file
          post_path = paste0(rx_dir, '/', post_raw)
          post_ras <- rast(post_path) %>% 
                        mask(., CombinedMask2)
          post_ras = (post_ras-10000)/10000
          names(post_ras) <- c('green', 'red', 'nir', 'swir')
      # PIF correction 
        # Calculate the absolute difference for PIF-sensitive bands
          diff_img <- abs(pre_ras[[c('green', 'red', 'swir')]] - post_ras[[c('green', 'red', 'swir')]])
        # Sum the differences across bands to get a "change magnitude" map
          mag <- sum(diff_img)
        # Define a threshold for "No Change" (e.g., the bottom 1% of pixels) 
          thresh <- quantile(values(mag, na.rm=TRUE), probs = 0.01)
        # Create PIF mask
          pif_mask = ifel(mag >= thresh, NA, mag)
        # Ensure there are enough relatively-invariant pixels to do regression
          pif_pix <- values(pif_mask$sum, na.rm=TRUE) %>% length()  
          if(pif_pix < 100) { 
            thresh <- quantile(values(mag, na.rm=TRUE), probs = 0.1)
            pif_mask = ifel(mag >= thresh, NA, mag)
            pif_pix <- values(pif_mask$sum, na.rm=TRUE) %>% length() 
          }
          if(pif_pix < 10) { 
            thresh <- quantile(values(mag, na.rm=TRUE), probs = 0.1)
            pif_mask = ifel(mag >= thresh, NA, mag)
            pif_pix <- values(pif_mask$sum, na.rm=TRUE) %>% length() 
          }
          
        # Apply mask to images
          ref_pifs <- mask(pre_ras, pif_mask)   # Pre-burn reference image
          subj_pifs <- mask(post_ras, pif_mask) # subject image to correct
    ## A PIF correction loop combining Jay's script and`landsat::PIF` example
      # stash list
        corr_bands <- list()
      # loop through layers to get PIF pixels
        for(l in 1:nlyr(pre_ras)) {
          # Get PIF values for the current band
            vals <- tibble(
                      ref_val = values(ref_pifs[[l]], na.rm=TRUE),
                      subj_val = values(subj_pifs[[l]], na.rm=TRUE) 
                      )
          # Fit the linear model
            # post.corr <- suppressMessages(
            #               lmodel2:::lmodel2(vals[[1]] ~ vals[[2]])$regression.results[2, 2:3]
            #               )
            post.corr <- lm(vals[[1]] ~ vals[[2]])$coefficients
            m = post.corr[[2]]
            b = post.corr[[1]]
    
          # Apply correction to the entire subject band
            corr_bands[[l]] <- (m * post_ras[[l]]) + b
          #cat(paste("Band", i, "Normalized: Gain =", round(b, 3), "Offset =", round(m, 3), "\n"))
        }
      
      # Merge corrected bands back into a single SpatRaster & crop to pastures
        corr_rast <- rast(corr_bands ) %>%
                        crop(loc_sf )
      # crop the pre-burn image to pastures as well
        pre_burn <- pre_ras %>% crop(loc_sf)
        # post_burn <- post_ras %>% crop(loc_sf)   # raw
    #
    # Step 3: Calculate dNBR from pre- and corrected post imagery
    #
      # create NBR rasters for pre and corrected post
        nbr_pre <- calc_nbr(pre_burn$nir, pre_burn$swir )  
        nbr_post <- calc_nbr(corr_rast$nir, corr_rast$swir) # post-fire, corrected
        # nbr_post_raw <- calc_nbr(post_burn$nir, post_burn$swir) # post-fire, raw
        
      # Calculate dNBR
        dnbr <- (nbr_pre - nbr_post) %>% rename(dNBR = nir)
        names(dnbr) <- date$BurnDate
        dNBR[[i]] <- dnbr
    }
        rast(dNBR ) %>%
          writeRaster(paste0('./data/RxRasters/TC/', loc, '.tiff'), overwrite=TRUE)
        Sys.time() - begin 
        beepr::beep() 
  }

# SCL categories 
  # 0: No Data (Missing data) - black  
  # 1: Saturated or defective pixel - red 
  # 2:  Topographic casted shadows ("Dark features/Shadows" for data before 2022-01-25) - very dark grey
  # 3:  Cloud shadows - dark brown
  # 4: Vegetation - green
  # 5:  Not-vegetated - dark yellow
  # 6: Water (dark and bright) - blue
  # 7:  Unclassified - dark grey
  # 8: Cloud medium probability - grey
  # 9: Cloud high probability - white
  # 10: Thin cirrus - very bright blue
  # 11:  Snow or ice - very bright pink