library(readxl)
library(dplyr)
library(glmmTMB)
library(gamm4)
library(lme4)
source("./indices/bootstrap_functions.r")

base_path <- "C:/Users"
usr <- "jgorzo"
loc <- "OneDrive - New Jersey Office of Information Technology/Documents"
root <- file.path(base_path, usr, loc)

vts16 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                                "NJDEP Trap_survey_yr_1_2016.xlsx"), sheet = "trap_survey_yr_1")
names(vts16) <- tolower(names(vts16))
#vts16$video <- NULL
names(vts16)[names(vts16)=="length_mm"] <- "total_length_mm"
names(vts16)[names(vts16)=="soak_time days"] <- "soak_time" 

vts17 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_2_2017.xlsx"), sheet = "trap_survey_yr_2")
names(vts17) <- tolower(names(vts17))
#vts17$video <- NULL
names(vts17)[names(vts17)=="length_mm"] <- "total_length_mm"
names(vts17)[names(vts17)=="soak_time_days"] <- "soak_time" 

vts18 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_3_2018.xlsx"), sheet = "NJDEP TrapSurvey_yr_3")
names(vts18) <- tolower(names(vts18))
names(vts18)[names(vts18)=="length_mm"] <- "total_length_mm"
vts18 <- vts18[,which(names(vts18) %in% names(vts17))]

vts19 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_4_2019.xlsx"), sheet = "AllCombined")
names(vts19) <- tolower(names(vts19))
vts19 <- vts19[,which(names(vts19) %in% names(vts17))]

vts20 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_5_2020.xlsx"), sheet = "AllCombined")
names(vts20) <- tolower(names(vts20))
vts20 <- vts20[,which(names(vts20) %in% names(vts17))]

vts21 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_6_2021.xlsx"), sheet = "AllCombined")
names(vts21) <- tolower(names(vts21))
vts21 <- vts21[,which(names(vts21) %in% names(vts17))]

vts22 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_7_2022.xlsx"), sheet = "AllCombined")
names(vts22) <- tolower(names(vts22))
vts22 <- vts22[,which(names(vts22) %in% names(vts17))]

vts23 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_8_2023.xlsx"), sheet = "AllCombined")
names(vts23) <- tolower(names(vts23))
vts23 <- vts23[,which(names(vts23) %in% names(vts17))]

vts24 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_9_2024.xlsx"), sheet = "AllCombined")
names(vts24) <- tolower(names(vts24))
vts24 <- vts24[,which(names(vts24) %in% names(vts17))]
vts <- rbind(vts16, vts17, vts18, vts19, vts20, vts21, vts22, vts23) %>%
  arrange(haul_date) %>% rename(onoff=`on/off reef`)
vts <- vts[,which(names(vts) %in% names(vts24))]
vts <- rbind(vts, vts24)
vts[,c("season", "species")] <- apply(vts[,c("season", "species")], 2, tolower)
vts[vts$vessel=="reef bound",]$vessel <- "reefbound"
vts[vts$vessel=="resiliance",]$vessel <- "resilience"
vts$reef <- toupper(vts$reef)
vts[grep("u", vts$total_length_mm),]$total_length_mm <- "245"
vts[which(vts$total_length_mm=="NA"),]$total_length_mm <- "1"
vts$total_length_mm <- as.integer(vts$total_length_mm)
vts <- vts %>% filter(total_length_mm > 0 & total_length_mm < 2000)
vts$season <- factor(vts$season, levels=c("spring", "summer", "fall", "winter"))
tog <- vts[vts$species=="tautog",]
ggplot(tog, aes(x=season, y=total_length_mm)) + geom_point()

counts <- vts %>%
  group_by(year, set_date, haul_date, season, reef, soak_time, trap_id, vessel, species) %>%
  summarise(count = n()) %>% ungroup()
filled <- counts %>%
  complete(species, nesting(year, set_date, haul_date, season, reef, soak_time, trap_id, vessel),
           fill=list(count=0)) %>% filter(species == "tautog" & !is.na(year))
ggplot(data = filled, aes(x = reef, y = count)) +
  geom_bar(position = "dodge", stat = "sum") #+
  #facet_wrap(~Year, scales = "free")
 #+
#facet_wrap(~Year, scales = "free")
filled <- filled %>% filter(reef != "S7")
filled$Year <- as.factor(filled$year)
ggplot(data = filled, aes(x = Year)) +
  geom_histogram(stat="count")
ggplot(data = filled, aes(x = Year, y = count)) +
  geom_bar(position = "dodge", stat = "sum")
#filled$material <- as.factor(filled$material)
filled$trap_id <- as.factor(filled$trap_id)
filled$vessel <- as.factor(filled$vessel)

allvars <- "count ~ Year + season + set_date + haul_date + (1|reef) + soak_time + (1|trap_id) + (1|vessel)"
mod <- as.formula(allvars)
bmc <- buildmerControl(include= ~ Year) #+ (1|site)
nb <- buildglmmTMB(mod, filled, family = nbinom2, 
                   buildmerControl = bmc)
NB <- glmmTMB(formula(nb), #when just year and surface temp, super high STD ERROR
              data = filled,
              family = nbinom2)
#fm1R <- refit(NB, simulate(NB)[[1]])
get_fixef <- function(x) fixef(x)$cond
b1 <- lme4::bootMer(NB, FUN=get_fixef, nsim=1000, .progress="txt")
#View standard errors
print(b1)
boot_ci <- confint(b1, type = "perc")
#https://easystats.github.io/parameters/reference/bootstrap_model.html
###from script
SE <- boot.NB(NB, nboots=1000) #come back to check this tomorrow! might have to make categorical vars factors...
p.data <- expand.pred(NB$frame)
index.out <- data.frame(Year=as.numeric(as.character(unique(filled$Year))), #use unique rather than levels bc removed 3 years
                        Index= predict(NB, newdata=p.data, type="response"))
index.out <- cbind.data.frame(index.out, SE)
index.out$CV <- index.out$SE / index.out$Index
index.out$scale <- index.out$Index / mean(index.out$Index)
#####

ggplot(index.out) +
  geom_ribbon(aes(x = Year, ymin = LCI, ymax = UCI), alpha = 0.4) +
  geom_point(aes(x = Year, y = Index), shape = 16) +
  geom_line(aes(x = Year, y = Index)) +
  ylim(c(0, NA)) +
  theme_bw()
#save(index.out, file="NYWLI.RData")

gamselect <- buildgamm4(mod, data = filled, buildmerControl = bmc)
GAM.NB <- gamm4(formula(gamselect), data = filled, family = 'nb') #still wins
#SE2 <- boot.GAM(GAM.NB, nboots = 1000)
