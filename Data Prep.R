
#I thought if we can separate data by subsetting it in two data subsets so we can prepare 
#demographic and sociographic separately from the other analysis as i see we do not include it in the 
#cleaned data. (assume it from the prof's code). following is from her, and adapted
data.wide <- read.csv("QuestionaireData_CityTrips.csv")
dim(data.wide )
str(data.wide )
install.packages("pacman")
pacman::p_load(reshape2, ggplot2, car, dplyr, psych, coefplot, corrplot, MASS, smacof,stringr)
describe(data.wide )
head(data.wide )
data.wide$id_unique <- 1:nrow(data.wide)
head(data.wide)
names(data.wide)
data.long <- data.wide %>% 
  dplyr::select(c("Sample", "ID", "id_unique"), #pretty  tricky thing if we do not put dplyr:: for me it works not so good, as we have MASS preinstalled
         contains("_Att"),
         starts_with("Pref_")) 
data.long <- melt(data.long, id.vars = c("Sample", "ID", "id_unique"))
head(data.long)
dim(data.long)
data.long <- data.long[!is.na(data.long$value), ]
dim(data.long)
data.long$type <- ifelse(str_detect(data.long$variable, "_Att"), "Attribute", "Pref")
head(data.long)


# so we need to convert it to a character class and then apply strsplit()
strsplit(as.character(data.long[1, ]$variable), "_")


data.long$City <- ifelse(data.long$type == "Attribute",
                         sapply(strsplit(as.character(data.long$variable), "_"), `[`, 1),
                         sapply(strsplit(as.character(data.long$variable), "_"), `[`, 2))
head(data.long)
unique(data.long$City)

# Now let's create column Attribute, which is the index for attributes now contained
# in column variable if type = "Attribute" and let's set the value to "Pref" if type = "Pref
data.long$Attribute <- ifelse(data.long$type == "Attribute",
                              sapply(strsplit(as.character(data.long$variable), "_"), `[`, 2),
                              "Pref")
head(data.long)
unique(data.long$Attribute)

# Attribute column is a character. Let's save it as a factor and
# assign meaningful labels from the documentation file
# Define the vector of labels
attLabels <- c("friendly", "historical", "affordable", "trendy",
               "vibrant nightlife", "delicious food", "easy-to-get-around",
               "good shopping", "cultural events", "interesting museums",
               "clean", "green", "international", "too touristic",
               "fun", "noisy", "romantic", "safe", "beautiful", 
               "english-speaker-friendly")
length(attLabels) # should be 20


# Set Attribute variable as a factor, define levels from 1 to 20, and 
# define the labels 
data.long$Attribute <- factor(data.long$Attribute, levels = c(paste0("Att", 1:20), "Pref"),
                              labels = c(attLabels, "Pref"))
head(data.long)

# Let's reshape to wide format 
# so that we have one column for each attribute and one column for Pref
data.long <- dcast(data.long, 
                   Sample + ID + id_unique + City ~ Attribute, 
                   value.var = "value")
head(data.long)
dim(data.long) # 1590   25

# Are these correct dimensions?
# Each respondent was supposed to evaluate 6 cities
length(unique(data.long$id_unique)) # 266
266 * 6 # 1596 

head(data.long)
tail(data.long)
any(is.na(data.long))
colSums(is.na(data.long))
tail(data.long)
#Treating NAs, checking with different attributes and etc, for missing id i just took the attribute with the biggest number of NAs

missings.id <- unique(subset(data.long, is.na(international))$id_unique)
missings.id #here we got the single unique ids that have NAs and i checked them manually
data.long[data.long$id_unique =="103",]  #as a rule i decided to consider units with  3  and more missing observations per cities as shitty but this point is worth discussing
missings.id <- c(57,59,103,125,143,157,167,216,221,244,255) #unique id numbers that i did not like, so i rewrote the value
newdata <- subset(data.long, data.long$id_unique %in% missings.id) #and subset with these missings id
newdata [order(newdata $id), ] #idk why but it is here
 
data.long <- subset(data.long, !id_unique %in% missings.id) #did not use subset, decided to go differently
head(data.long)
dim(data.long)
length(unique(data.long$id_unique))
length(unique(data.long$City))
#NOW I AM FIGURING OUT WHAT I DELETED WHAT I WAS NOT SUPPOSED TO haha, so 255*6 will work
#i will check what observations i probably had to leave untouched, for the other NAs I think I will substitute them with mean value per attribute and city