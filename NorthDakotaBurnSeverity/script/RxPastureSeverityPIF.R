pacman::p_load(tidyverse, magrittr, readxl, sf, terra, tidyterra)

# Function to calculate NBR 
    calc_nbr <- function(nir, swir) {
      return((nir - swir) / (nir + swir))
    }

# Load Rx fire data
  rx_perims <-
    bind_rows(
      read_sf('./data/OriginalRxBurnPerims.gpkg', 'hrec') %>%
        mutate(across(c(PreBurn:PostBurn), ~as.character(.x)), 
               burn = paste0(location, '_', row_number())) %>%
        select(location, burn, PreBurn, PostBurn) %>%
        st_transform(4326), 
      read_sf('./data/OriginalRxBurnPerims.gpkg', 'dayton')  %>%
        mutate(across(c(PreBurn:PostBurn), ~as.character(.x)), 
               burn = paste0(location, '_', row_number())) %>%
        select(location, burn, PreBurn, PostBurn)%>%
        st_transform(4326), 
      read_sf('./data/OriginalRxBurnPerims.gpkg', 'cgrec') %>%
        mutate(burn = paste0(location, '_', row_number())) %>%
        select(location, burn, PreBurn, PostBurn) %>%
        st_transform(4326) )

 # Output unique imagery dates   
    rx_perims %>%
      as_tibble() %>%
      select(-geom) %>%
      mutate(location = case_when(
        location %in% c('Fitch', 'Clement') ~ 'HREC', 
        TRUE ~ location
      )) %>%
      pivot_longer(names_to = 'Period', 
                   values_to= 'date', 
                   cols = c(PreBurn, PostBurn)) %>%
      select(-burn, -Period) %>%
      group_by(location) %>%
      distinct() %>%
      write.table("clipboard", sep="\t", row.names=FALSE)
    
# Identify unique post-burn dates to calculate severity for 
  rx_perims %<>%
    select(location, burn, PostBurn, PreBurn) %>%
    mutate(location = case_when(
                        location %in% c('Fitch', 'Clement') ~ 'HREC', 
                        TRUE ~ location
                      )) %>%
    arrange(location, PostBurn) %>%
    mutate(DeltaPair = paste0(PostBurn, '_', PreBurn)) 
  
  # st_write(rx_perims, './data/SeverityComparison.gpkg', 'rx_perims')
  
  DeltaPairs <- 
    rx_perims %>%
        as_tibble() %>%
      group_by(location) %>%
      distinct(DeltaPair) %>%
        ungroup() %>%
      separate(DeltaPair, into = c("PostBurn", "PreBurn"), sep = '_', remove = F )

# Imagery
  rx_dir = 'S:/DevanMcG/FireScience/Sentinel/SeverityComparison/Rx'
  
  rx_images <- 
    tibble(file = list.files(rx_dir, pattern = "\\.tiff$", ignore.case = TRUE) ) %>% 
    mutate(info = str_remove(file, '.tiff')) %>%
    separate(info, into = c('location', 'ImageDate', 'type'), sep = '_') %>%
    mutate(location = replace_values(location, 'DAY' ~ 'Dayton')) 

##
## Location-specific corrected dNBR rasters
##
  
{
  begin = Sys.time()

  for(i in 1:length(unique(DeltaPairs$location))) {
    loc = unique(DeltaPairs$location)[i]
    dp_d <- filter(DeltaPairs, location == loc)
    loc_sf <- rx_perims %>% filter(location == loc) # for cropping dNBR raster
    dNBR <- list() 
    cat(paste('Starting', loc, '...'))
    for(j in 1:length(dp_d$DeltaPair)) {
      dp = slice(dp_d, j)

    #
    # Step 1: Create cloud & shadow mask from Scene Classification Layers
    #
    # Load, wrangle SCL as output by Copernicus GUI
      # Pre-fire SCL 
        pre_scl = filter(rx_images, location == dp$location, 
                                    type == 'SCL', 
                                    ImageDate == dp$PreBurn)$file
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
        post_scl = filter(rx_images, location == dp$location, 
                          type == 'SCL', 
                          ImageDate == dp$PostBurn)$file
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
          CombinedMask %<>% mutate(sc = ifelse(sc >= 1, NA, 1))
    #
    # Step 2: PIF correction on post-fire image
    #
      ## Load raw bands
        # pre image
          pre_raw = filter(rx_images, location == dp$location, 
                           type == 'RAW', 
                           ImageDate == dp$PreBurn)$file
          pre_path = paste0(rx_dir, '/', pre_raw)
          pre_ras <- rast(pre_path) %>% 
                        mask(., CombinedMask)
          pre_ras <-  (pre_ras-10000)/10000
          names(pre_ras) <- c('green', 'red', 'nir', 'swir')
        # post-fire
          post_raw = filter(rx_images, location == dp$location, 
                            type == 'RAW', 
                            ImageDate == dp$PostBurn)$file
          post_path = paste0(rx_dir, '/', post_raw)
          post_ras <- rast(post_path) %>% 
                        mask(., CombinedMask)
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

        # Apply mask to images
          ref_pifs <- mask(pre_ras, pif_mask)   # Pre-burn reference image
          subj_pifs <- mask(post_ras, pif_mask) # subject image to correct
    ## PIF correction loop 
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
        } # close band correction loop
      
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
        names(dnbr) <- dp$DeltaPair
        dNBR[[j]] <- dnbr
    } # close DeltaPair loop
    rast(dNBR ) %>%
      writeRaster(paste0('./data/RxRasters/Pasture/', loc, '.tiff'), overwrite=TRUE)
    cat(paste('   ...done, raster written.\n'))
  } # close location loop
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