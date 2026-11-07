library(carobiner)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them

carob_script <- function(path) {
"Impact Assessment of Three Reductions - Three Gains in South Vietnam.
Measure awareness and adoption of 3R3G in the Mekong Delta Region and assess differences in input use, productivity, farm costs and incomes between adopters and non-adopters."
  
  
  ## Identifiers
  uri <- "doi.org/10.7910/DVN/26196"
  group <- "survey"
  
  ## Download data 
  ff  <- carobiner::get_data(uri, path, group)
  
  ## metadata 
  # change the major and minor versions if you see a warning
  meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
                                  # include the data provider and/or all institutes listed as authors (if any)
                                  data_organization = "IRRI",
                                  publication = NA,
                                  project = NA,
                                  data_type = "survey",
                                  treatment_vars = NA,
                                  response_vars = NA, 
                                  carob_completion = 50,
                                  carob_effort = 1,
                                  carob_contributor = "Huynh Thi Truc Linh",
                                  carob_date = "2026-11-07",
                                  notes = NA, # notes for the end-user
                                  design = NA
  )
  
  ## read data 
  
  f <- ff[basename(ff) == "Impact Assessment of Three R and Three G.xls"]
  
  proc <- function(s) {
    r <- carobiner::read.excel(f, sheet=s, skip=3)
    r <- r[!is.na(r$ResNo), ]
    names(r) <- gsub("-", "", names(r))
  
  ## select the variables of interest and assign them to the correct name
  d <- data.frame(
    country = r$Country,
    
    ## make sure that names are normalized (proper capitalization, spelling, no additional white space).
    ## you can use carobiner::fix_name()
    ## first use "location", then use "site"

    location = r$Site,
    elevation = r$Alt,
    
    adm1 = carobiner::fix_name(r$Province, "title"),
    adm2 = carobiner::fix_name(r$District, "title"),
    
    ## the treatment code/description (if any)	
    d$treatment <- r$Treatment
  )
  
  ## separate individual trials. For example trials in different locations or years. 
  ## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
  d$trial_id <- as.character(as.integer(as.factor( ____ )))
  
  ## about the data (TRUE/FALSE)
  d$on_farm <- NA
    d$is_survey <- NA
    d$irrigated <-
    
    ## crop rotation. If available, add all crops, including "d$crop". Use an underscore for intercrops 
    d$crop_rotation <- "crop1;crop2;crop3_crop4"
  
  ## each site must have corresponding longitude and latitude
  ## if the raw data do not provide them you can estimate them from the location/adm data 
  ## see carobiner::geocode
  d$longitude <- 
    d$latitude <- 
    # are the coordinates from the source (data/publication) or estimated by you?	
    d$geo_from_source <- TRUE/FALSE
  
  
  ## time can be year ("2023", four characters), year-month ("2023-07", 7 characters) or date ("2023-07-21", 10 characters).
  ## if dates come as character values, you can use as.character(as.Date()) for dates to assure the correct format.
  d$planting_date <- as.character(as.Date(   ))
  d$harvest_date  <- as.character(as.Date(    ))
  
  ### Fertilizers 
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  d$P_fertilizer <- 
    d$K_fertilizer <-
    d$N_fertilizer <- 
    d$S_fertilizer <- 
    d$lime <- 
    ## normalize names 
    d$fertlizer_type <- 
    
    ## for legumes   
    d$inoculated <- TRUE or FALSE
  d$inoculant <- "name of inoculant"
  
  ### in general, add comments to your script if computations are
  ### based on information gleaned from metadata, a publication, 
  ### or when they are not immediately obvious for other reasons
  
  ### Yield
  
  yield <- r$yield_tonha * 1000
  #what plant part does yield refer to?
  d$yield_part <- "tubers"
  d$yield_moisture <- r$moisture * 100
  
  #NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
  # Also record fresh and/or dry weight production of other organs (or "residue" or "total")
  # if the data allow for that 
  
  d$fwy_storage <- r$yield_tonha * 1000
  d$dmy_storage <- (1-r$moisture) * r$yield_tonha * 1000
  d$dmy_totat <- r$dry_biomass
  
  # all scripts must end like this
  carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)