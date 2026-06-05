library("ageproR")

# Load path of ageproR's included test Example1 Input File
inpfile <- file.path("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/AGEPRO/agepro_test/NJNYB_100_F30test.inp")

# Load path of ageproR's included test Example1 Bootstrap File
bsnfile <- file.path("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/AGEPRO/agepro_test/ORIG.BSN")

# Create a agepro_inp_model with default values (This will 
test <- ageproR::agepro_inp_model$new()
test$read_inp(inpfile)
# NOTE: This will give a WARNING that a NULL Recruitment Model is Found. This behavior is expected. 
# ageproR will check to see if agepro models will have NULL or invalid Recruitment models before being saved to file

# Set Recruit Models. 
test$set_recruit_model(c(14))
# NOTE: This will OVERWRITE the previous [RECRUIT] values.
# NOTE 2: Setting recruitment values depend on the number of recruits an agepro_model is initialized with. For example to set a `agepro_inp_model` with two Beverton-Holt and a single Ricker recruit:
test <- ageproR::agepro_inp_model$new(num_rec_models=3)
test$set_recruit_model(c(5,5,6)) 

# Save Path of Bootstrap File
test$set_bootstrap_filename(bsnfile)

# Save Input Files 
# Using tempfile() as example filepath
outfile <- tempfile("example1_", fileext = ".inp")
test$write_inp(outfile)