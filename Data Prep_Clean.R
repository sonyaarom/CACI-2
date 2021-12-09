
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
missings.id <- c(34,57,59,103,125,143,157,167,216,221,244,255,213) #unique id numbers that i did not like, so i rewrote the value
newdata <- subset(data.long, data.long$id_unique %in% missings.id) #and subset with these missings id
newdata [order(newdata $id), ] #idk why but it is here
 
data.long <- subset(data.long, !id_unique %in% missings.id) #did not use subset, decided to go differently
head(data.long)
dim(data.long)
length(unique(data.long$id_unique))
length(unique(data.long$City))
#NOW I AM FIGURING OUT WHAT I DELETED WHAT I WAS NOT SUPPOSED TO haha, so 255*6 will work
data.long[data.long$id_unique =="34",]#something weird here, 1 observation? will delete it, same for 213. not full observations
#i will check what observations i probably had to leave untouched, for the other NAs I think I will substitute them with mean value per attribute and city

sum(data.long$id_unique)
frequency(data.long$id_unique)
table(data.long$id_unique)

#now dimensions: 253 id  * 6  observations


install.packages("tidyr","dplyr")
library("dplyr","tidyr")
data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(friendly= ifelse(is.na(friendly),round(mean(friendly,na.rm = T)),friendly))
colSums(is.na(data.long))
data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(historical= ifelse(is.na(historical),round(mean(historical,na.rm = T)),historical))
colSums(is.na(data.long))
data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(affordable= ifelse(is.na(affordable),round(mean(affordable,na.rm = T)),affordable))
data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(trendy= ifelse(is.na(trendy),round(mean(trendy,na.rm = T)),trendy))
colSums(is.na(data.long))
data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`vibrant nightlife`= ifelse(is.na(`vibrant nightlife`),round(mean(`vibrant nightlife`,na.rm = T)),`vibrant nightlife`))
data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`delicious food`= ifelse(is.na(`delicious food`),round(mean(`delicious food`,na.rm = T)), `delicious food`))
colSums(is.na(data.long)) 

data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`easy-to-get-around`= ifelse(is.na(`easy-to-get-around`),round(mean(`easy-to-get-around`,na.rm = T)), `easy-to-get-around`))
data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`good shopping`= ifelse(is.na(`good shopping`),round(mean(`good shopping`,na.rm = T)), `good shopping`))
data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`cultural events`= ifelse(is.na(`cultural events`),round(mean(`cultural events`,na.rm = T)), `cultural events`))

data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`interesting museums`= ifelse(is.na(`interesting museums`),round(mean(`interesting museums`,na.rm = T)), `interesting museums`))

data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`clean`= ifelse(is.na(`clean`),round(mean(`clean`,na.rm = T)), `clean`))

data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`green`= ifelse(is.na(`green`),round(mean(`green`,na.rm = T)), `green`))

data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`international`= ifelse(is.na(`international`),round(mean(`international`,na.rm = T)), `international`))

data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`too touristic`= ifelse(is.na(`too touristic`),round(mean(`too touristic`,na.rm = T)), `too touristic`))

data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`fun`= ifelse(is.na(`fun`),round(mean(`fun`,na.rm = T)), `fun`))

data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`noisy`= ifelse(is.na(`noisy`),round(mean(`noisy`,na.rm = T)), `noisy`))

data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`romantic`= ifelse(is.na(`romantic`),round(mean(`romantic`,na.rm = T)), `romantic`))

data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`safe`= ifelse(is.na(`safe`),round(mean(`safe`,na.rm = T)), `safe`))

data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`beautiful`= ifelse(is.na(`beautiful`),round(mean(`beautiful`,na.rm = T)), `beautiful`))

data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`english-speaker-friendly`= ifelse(is.na(`english-speaker-friendly`),round(mean(`english-speaker-friendly`,na.rm = T)), `english-speaker-friendly`))

data.long <- data.long %>% 
  group_by(City) %>% 
  mutate(`Pref`= ifelse(is.na(`Pref`),round(mean(`Pref`,na.rm = T)), `Pref`))

colSums(is.na(data.long)) 
library("psych")                                
describe(data.long)
#do not see outliers here, min and max are in normal range, probably it was these deleted two id (34,213) that had just not enough observation (not NA, but had just one line of assessed cities for example)