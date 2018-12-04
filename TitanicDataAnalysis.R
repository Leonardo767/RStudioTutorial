# Load raw data
train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)

test.Survived <- data.frame(Survived = rep(NA, nrow(test)), test[,])
test.Survived <- test.Survived[,c(2,1,3,4:12)]
data.combined <- rbind(train, test.Survived)
str(data.combined)

# convert data types
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
str(data.combined)

# Take a look at survival rates (table)
table(data.combined$Survived)
table(data.combined$Pclass)

# Load ggplot2 to use visualizations
library(ggplot2)
library(stringr)

# Hypothesis: upper class had higher survival rate
ggplot(train, aes(x=Pclass, fill=factor(Survived))) +
  geom_bar(width=0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Look at the first few names
head(as.character(train$Name))

# how many unique names are there accrross combined dataset?
length(unique(as.character(data.combined$Name)))
length(as.character(data.combined$Name))
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
data.combined[which(data.combined$Name %in% dup.names),]

# any correlation between name titles (Mr., Miss, Mrs.) and survivorship?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,1:5]

# any correlation between gender and survivorship?
males <- data.combined[which(data.combined$Sex == "male"),]
males[1:5,1:5]

# Expand on Survivorship vs Pclass by adding their name title as a third axis
# this will now be Survivorship vs (Pclass and Title)

# create a utility function to help with title extraction
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

titles <- NULL

for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$Title <-as.factor(titles)

# Visualize
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fil = "Survived")








