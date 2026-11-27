library("ageproR")
# Load path of ageproR's included test Example1 Input File
inpfile <- file.path("C:/Users/galax/OneDrive - New Jersey Office of Information Technology/Documents/AGEPRO/agepro_test/NJ-NYB 3YEAR PROJECTION 2021.inp")
# Load path of ageproR's included test Example1 Bootstrap File
bsnfile <- file.path("C:/Users/galax/OneDrive - New Jersey Office of Information Technology/Documents/AGEPRO/agepro_test/NJ-NYB_Bootstrap_file_2021.BSN")

test2 <- ageproR::import_agepro_model(inpfile)
fields <- as.list(test2)
fields <- Filter(Negate(is.function), fields)
df <- as.list(fields)
