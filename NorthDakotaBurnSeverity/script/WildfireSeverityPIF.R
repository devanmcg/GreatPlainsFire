pacman::p_load(tidyverse, magrittr, sf, terra, tidyterra)

load('../albersEAC.Rdata')

# Function to calculate NBR 
    calc_nbr <- function(nir, swir) {
      return((nir - swir) / (nir + swir))
    }

# Regional rangeland raster
  # rr <- terra::rast("S:/DevanMcG/GIS/SpatialData/US/USFS/ReevesRangelands/USrangelands.tif")
  # region <- read_sf('./data/SeverityComparison.gpkg', 'StudyRegion') %>%
  #             st_transform(st_crs(rr))
  # 
  # r_rr <- rr %>% terra::crop(region)
  # terra::writeRaster(r_rr, "S:/DevanMcG/GIS/SpatialData/US/USFS/ReevesRangelands/NoPlainsRange.tif")
  
  r_rr <- terra::rast("S:/DevanMcG/GIS/SpatialData/US/USFS/ReevesRangelands/NoPlainsRange.tif")

  #st_layers('./data/SeverityComparison.gpkg')

  wf_sf <- read_sf('./data/SeverityComparison.gpkg', 'GreatPlainsModifiedWF')
  
  wf_dir = 'S:/DevanMcG/FireScience/Sentinel/SeverityComparison/CompWildfires/BufferedForPIF'
  
  wf_images <- 
    tibble(file = list.files(wf_dir, pattern = "\\.tiff$", ignore.case = TRUE) ) %>% 
    mutate(info = str_remove(file, '.tiff')) %>%
    separate(info, into = c('FireCode', 'period', 'ImageDate', 'type'), sep = '_') %>%
    mutate(FireCode = str_replace(FireCode, '-', '_'))
  
  dud_rasters = c('HayCreek_2018' )
  
  wf_perims <- wf_sf %>% 
              filter(FireCode %in% unique(wf_images$FireCode), 
                      ! FireCode %in% dud_rasters) 
  
  # unique(wf_images$FireCode)
  # unique(wf_images$file)
  # unique(wildfires$FireCode)
  
  load('./data/WfSeverity.Rdata') 
  
  ToDo = setdiff(unique(wf_perims$FireCode), unique(WfSeverity$FireCode)) 
    
  WfSeverityLM <- tibble()   # for correction via coefficients from lm
  WfSeverityLmd2 <- tibble() # for correction via coefficients from lmodel2
{
  begin = Sys.time()
    for(i in 1:length(unique(wf_perims$FireCode))) {
    # Get fire
      fc = unique(wf_perims$FireCode)[i]
      fire <-  wf_perims %>% 
                filter(FireCode == fc)
 
    #
    # Step 1: Create cloud & shadow mask from Scene Classification Layers
    #
    # Load, wrangle SCL as output by Copernicus GUI
      # Pre-fire SCL 
        pre_scl = filter(wf_images, FireCode == fc, type == 'SCL', 
                         period == 'B')$file
        PreSCL <- paste0(wf_dir, '/', pre_scl) %>% 
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
        post_scl = filter(wf_images, FireCode == fc, type == 'SCL', 
                         period == 'A')$file
        PostSCL <- paste0(wf_dir, '/', post_scl) %>% 
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
        # Create the combined mask
          CombinedMask <- pre_mask + post_mask 
          CombinedMask %<>% mutate(sc = ifelse(sc >= 1, NA, 1))
    #
    # Step 2: PIF correction on post-fire image
    #
      ## Load raw bands
        # pre image
          pre_raw = filter(wf_images, FireCode == fc, type == 'RAW', 
                             period == 'B')$file
          pre_path = paste0(wf_dir, '/', pre_raw)
          pre_ras <- rast(pre_path) %>% 
                        mask(., CombinedMask)
          pre_ras <-  (pre_ras-10000)/10000
          names(pre_ras) <- c('green', 'red', 'nir', 'swir')
        # post-fire
          post_raw = filter(wf_images, FireCode == fc, type == 'RAW', 
                              period == 'A')$file
          post_path = paste0(wf_dir, '/', post_raw)
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
        for(i in 1:nlyr(pre_ras)) {
          # Get PIF values for the current band
            vals <- tibble(
                      ref_val = values(ref_pifs[[i]], na.rm=TRUE),
                      subj_val = values(subj_pifs[[i]], na.rm=TRUE) 
                      )
          # Fit the linear model
            # post.corr <- suppressMessages(
            #               lmodel2:::lmodel2(vals[[1]] ~ vals[[2]])$regression.results[2, 2:3]
            #               )
            post.corr <- lm(vals[[1]] ~ vals[[2]])$coefficients
            m = post.corr[[2]]
            b = post.corr[[1]]
    
          # Apply correction to the entire subject band
            corr_bands[[i]] <- (m * post_ras[[i]]) + b
          #cat(paste("Band", i, "Normalized: Gain =", round(b, 3), "Offset =", round(m, 3), "\n"))
        }
      
      # Merge corrected bands back into a single SpatRaster & crop to fire perimeter
        corr_rast <- rast(corr_bands ) %>%
                        crop(fire %>% 
                               st_transform(4326) 
                             )
      # crop the pre-burn image to the fire perimeter as well
        pre_burn <- pre_ras %>% crop(fire %>% st_transform(4326))
        # post_burn <- post_ras %>% crop(fire %>% st_transform(4326))   # raw
    #
    # Step 3: Calculate dNBR from pre- and corrected post imagery
    #
      # create NBR rasters for pre and corrected post
        nbr_pre <- calc_nbr(pre_burn$nir, pre_burn$swir )  
        nbr_post <- calc_nbr(corr_rast$nir, corr_rast$swir) # post-fire, corrected
        #nbr_post <- calc_nbr(post_burn$nir, post_burn$swir) # post-fire, raw
        
      # Calculate dNBR
        dnbr <- (nbr_pre - nbr_post) %>% rename(dNBR = nir)

    #
    # Step 4: Sample dNBR from rangeland pixels 
    #
      # internal buffer against edge effects
        buff <- fire %>%
                  st_transform(albersEAC) %>%
                  mutate(AreaHa = st_area(.), 
                         AreaHa = as.numeric(AreaHa) * 0.0001) %>% 
                  st_buffer(-20)
      # create gridded sample points
        # cell size factor scaled to total area
          cs = case_when(
                buff$AreaHa < 10 ~ 25, 
                between(buff$AreaHa, 10, 50) ~ 36,
                between(buff$AreaHa, 50, 100) ~ 50, 
                between(buff$AreaHa, 100, 200) ~ 100, 
                between(buff$AreaHa, 200, 300) ~ 125, 
                between(buff$AreaHa, 300, 500) ~ 150, 
                between(buff$AreaHa, 500, 700) ~ 200, 
                between(buff$AreaHa, 700, 3000) ~ 300, 
                between(buff$AreaHa, 3000, 5000) ~ 500, 
                between(buff$AreaHa, 5000, 7000) ~ 700,
                buff$AreaHa > 7000 ~ 1000 )
       # Find points as centroids of grid w/in perimeter   
          pts <- buff %>%
                  st_make_grid(cellsize = cs, 
                               square = FALSE) %>% 
                  st_centroid() %>%
                  st_intersection(buff) %>%
                  st_as_sf() %>%
                  rowid_to_column('ID') %>%
                  st_transform(5070)
        # Filter sample points to rangeland cells
          r_pts <-  
            terra::extract(r_rr, 
                           terra::vect(pts), 
                           df = TRUE) %>%
            filter(LABEL %in% c('Rangeland', 
                                'Transitional Rangeland', 
                                'Afforested CO') ) 
          pts %<>% filter(ID %in% r_pts$ID) 
      # Extract values at points
        terra::extract(dnbr,
                       pts %>%
                         st_transform(4326) %>%
                         terra::vect(), 
                       fun = mean, 
                       method = 'bilinear',
                       bind = TRUE)  %>% 
        as_tibble() %>% 
        add_column(FireCode=fc, .before = 1 ) %>% 
        filter(! is.na(dNBR)) %>% 
        bind_rows(., WfSeverityLM) -> WfSeverityLM
    }
  Sys.time() - begin 
    }
    
  #save(WfSeverityLM, file = './data/WfSeverityLM.Rdata') 
  #save(WfSeverity, file = './data/WfSeverity.Rdata')
  
# Compare the two correction types

  full_join(by = 'FireCode', 
            WfSeverityLM %>% 
              filter(FireCode %in% unique(WfSeverity$FireCode) ) %>%
              group_by(FireCode) %>%
              summarise(RawMean = mean(dNBR), 
                        RawSE = sd(dNBR)/sqrt(n())),  
            WfSeverity %>%
              group_by(FireCode) %>%
              summarise(CorMean = mean(dNBR), 
                        CorSE = sd(dNBR)/sqrt(n())) 
  ) %>%
    separate(FireCode, into=c('name', 'year'), sep = '_') %>%
    mutate(year = str_remove(year, '20'), 
           year = as.numeric(year)) %>%
    ggplot(aes(x = RawMean, y = CorMean, color = year)) + theme_bw(14) + 
    geom_abline(slope = 1, intercept = 0) + 
    geom_point() +
    geom_errorbar(aes(ymin = CorMean - CorSE, 
                      ymax = CorMean + CorSE)) +
    geom_errorbar(aes(xmin = RawMean - RawSE, 
                      xmax = RawMean + RawSE), 
                  orientation = 'y') +
    coord_cartesian(xlim = c(0.03,0.67), 
                    ylim = c(0.03,0.67)) +
    labs(x = 'LM correction coefficients', 
         y = 'Lmd2 correction coefficients', 
         title = 'Mean dNBR by wildfire')

# Compare corrected dNBR to first round, uncorrected values by fire
  load('./data/WfBurnIndices.Rdata')
    
  full_join(by = 'FireCode', 
    WfBurnIndices %>% 
      filter(FireCode %in% unique(WfSeverity$FireCode) ) %>%
      group_by(FireCode) %>%
      summarise(RawMean = mean(dNBR), 
                RawSE = sd(dNBR)/sqrt(n())),  
        WfSeverityLM %>%
      group_by(FireCode) %>%
      summarise(CorMean = mean(dNBR), 
                CorSE = sd(dNBR)/sqrt(n())) 
    ) %>%
  separate(FireCode, into=c('name', 'year'), sep = '_') %>%
  mutate(year = str_remove(year, '20'), 
         year = as.numeric(year)) %>%
  ggplot(aes(x = RawMean, y = CorMean, color = year)) + theme_bw(14) + 
  geom_abline(slope = 1, intercept = 0) + 
    geom_point() +
    geom_errorbar(aes(ymin = CorMean - CorSE, 
                      ymax = CorMean + CorSE)) +
    geom_errorbar(aes(xmin = RawMean - RawSE, 
                      xmax = RawMean + RawSE), 
                  orientation = 'y') +
    coord_cartesian(xlim = c(0.03,0.67), 
                    ylim = c(0.03,0.67)) +
    labs(x = 'Raw post-fire NBR', 
         y = 'Corrected post-fire NBR', 
         title = 'Mean dNBR by wildfire')

