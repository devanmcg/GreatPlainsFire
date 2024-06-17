pacman::p_load(foreach, doSNOW)

imagery_dir = 'S:/DevanMcG/GIS/SpatialData/DunnRanch'

tars <- list.files(imagery_dir, '.tar.gz')

{
  begin = Sys.time()
  cores = parallel::detectCores()
  cl <- makeCluster(cores) 
  registerDoSNOW(cl)

  foreach(i=1:length(tars) ) %dopar% {
    untar(paste0(imagery_dir, '/', tars[i]), 
          exdir = imagery_dir)
  }
  stopCluster(cl)
  Sys.time() - begin
}





