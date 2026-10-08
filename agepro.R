library("ageproR")
# Load path of ageproR's included test Example1 Input File
inpfile <- file.path("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/AGEPRO/agepro_test/NJ-NYB 3YEAR PROJECTION 2021.inp")
# Load path of ageproR's included test Example1 Bootstrap File
bsnfile <- file.path("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/AGEPRO/agepro_test/NJ-NYB_Bootstrap_file_2021.BSN")

# Create a agepro_inp_model with default values (This will 
test <- ageproR::agepro_inp_model$new()
test$general$yr_start <- 2025
test$general$yr_end <- 2125
test$read_inp(inpfile)
test$general <- general_params$new(yr_start = 2025,
                                   yr_end = 2125,
                                   age_begin = test$general$age_begin,
                                   age_end = test$general$age_end,
                                   num_pop_sims = test$general$num_pop_sims,
                                   num_fleets = test$general$num_fleets,
                                   num_rec_models = test$general$num_rec_models,
                                   discards_present = test$general$discards_present,
                                   seed = test$general$seed)
# NOTE: This will give a WARNING that a NULL Recruitment Model is Found. This behavior is expected. 
# ageproR will check to see if agepro models will have NULL or invalid Recruitment models before being saved to file
test$set_bootstrap_filename(bsnfile)

recr <- read.csv("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/output/tog/asap/vtsage/plots/ASAP_summary_vtsage.csv")
test$recruit$recruit_data[[1]]$observations <- as.matrix(recr$Recr)
colnames(test$recruit$recruit_data[[1]]$observations) <- "recruit"
test$recruit$recruit_probability #needs to be updated to named vector with all years

agepro_params <- read.csv("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/output/tog/asap/vtsage/plots/AGEPRO_ave_params_vtsage.csv")
test$fishery #sel.age
#?fishery_selectivity
# Set Recruit Models. 
test$set_recruit_model(c(14))
# NOTE: This will OVERWRITE the previous [RECRUIT] values.
# NOTE 2: Setting recruitment values depend on the number of recruits an agepro_model is initialized with. For example to set a `agepro_inp_model` with two Beverton-Holt and a single Ricker recruit:
test <- ageproR::agepro_inp_model$new(num_rec_models=3)
test$set_recruit_model(c(5,5,6)) 
# Save Path of Bootstrap File
# Save Input Files 
# Using tempfile() as example filepath
outfile <- tempfile("example1_", fileext = ".inp")
# Load path of ageproR's included test Example1 Input File
inpfile <- file.path("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/AGEPRO/agepro_test/NJNYB_100_F30test.inp")
# Load path of ageproR's included test Example1 Bootstrap File
bsnfile <- file.path("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/AGEPRO/agepro_test/ORIG.BSN")
#test$write_inp(outfile)