root <- "C:/Users"
usr <- "jgorzo"
loc <- "OneDrive - New Jersey Office of Information Technology/Documents"
root <- file.path(root, usr, loc)

test <- read.table(file.path(root,
                     "output/tog/asap/Sept3_noadjust/base/AUG29_KD_RAW.STD"),
                   header=F, skip=1)
names(test) <- c("index", "name", "value", "sd")
write.csv(test[test$name=="recruits",],
          file.path(root, "output/tog/asap/recruits.csv"))

x=dget('c:/users/jgorzo/onedrive - new jersey office of information technology/documents/output/tog/asap/sept3_noadjust/base/asap3.rdat')
