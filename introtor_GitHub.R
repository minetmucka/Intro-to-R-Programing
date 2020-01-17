#########################################################################################################################################
## Objective: Intro to R                                                                                                                #
## Data source: iris data                                                                                                               #
## Please install "plyr" package: install.packages("plyr") for splitting, applying and combining data                                   #
## Please install "lattice" package: install.packages("lattice") for trellis graphics                                                   #
## Please install "ggplot2" package: install.packages("ggplot2") for Data Visualization                                                 #
#########################################################################################################################################


Iris_Data <- read.csv("C:/Users/muckam/Desktop/DataScienceBootcamp/Datasets/Iris_Data.csv")

head(Iris_Data)

Iris_Data %>% 
  rename(
    Type = Species
  )

Iris_Data [1:5,]

Iris_Data [Iris_Data$Sepal.Length<=5, ]

tail(Iris_Data,3)
summary(Iris_Data)
class(Iris_Data)
class(Iris_Data$Sepal.Length)
class(Iris_Data$Sepal.Width)
class(Iris_Data$Petal.Length)
class(Iris_Data$Petal.Width)
class(Iris_Data$Type)



# Basic box plot
p <- ggplot(Iris_Data, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_boxplot()
p

data(iris) #does a dataset exist? 

ncol(Iris_Data)
dim(Iris_Data)
str(Iris_Data) #structure of the dataset
summary(Iris_Data)

boxplot(Iris_Data$Petal.Length)
boxplot(Petal.Length~Species, data=Iris_Data)

plot(Iris_Data$Sepal.Length, Iris_Data$Sepal.Width)

Iris_Data$NewCol <- Iris_Data$Sepal.Length + Iris_Data$Sepal.Width

Iris_Data$NewCol

summary(Iris_Data)

cor(iris$Petal.Width,iris$Petal.Length)

plot(Sepal.Width ~ Sepal.Length, data=Iris_Data, main="Scatter Plot")

library(lattice)
xyplot(Sepal.Width ~ Sepal.Length, data=Iris_Data, groups=Species, auto.key=TRUE)

boxplot(Petal.Length~Species, data=Iris_Data)

table(Iris_Data$Species)

densityplot(Iris_Data$Petal.Length, main = "Density Kernel", xlab = "Petal Length")
densityplot(~Petal.Length, data=Iris_Data, groups = Species, auto.key=TRUE)

#Core Graphics
pairs(Iris_Data[,1:4], main="Simple Scatter Matrix")

data(mtcars)
head(mtcars)
summary(mtcars)
class(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)

class(mtcars$cyl)

boxplot(mpg~cyl, data=mtcars) #boxplot is good with categories and numbers 

xyplot(mpg ~ hp, data=mtcars) #scatter only numbers with numbers, not numbers with categories

library(ggplot2) #ggplot is a layering package, you can add more to it.
data(diamonds)
head(diamonds)

ggplot(diamonds, aes(x=carat)) + geom_histogram() 

ggplot(diamonds) + geom_density(aes(x=carat), fill="blue")

ggplot(diamonds, aes(x=carat, y=price)) +geom_point()

g<- ggplot(diamonds, aes(x=carat, y=price)) 

g + geom_point(aes(color=color))
g + geom_point(aes(color=clarity))

g + geom_point(aes(color=color)) + facet_wrap(~color) #wrap by color

g + geom_point(aes(color=clarity)) + facet_wrap(~color) #wrap by clarity

g + geom_point(aes(color=clarity)) + facet_wrap(cut ~ clarity) #wrap by clarity

g + geom_point(aes(color=color)) + facet_grid(cut ~ clarity) #grid by clarity



titanic <- read.csv("C:/Users/muckam/Desktop/DataScienceBootcamp/Datasets/titanic.csv")

View(titanic)


## let's look at the features
str(titanic)

# Casting & Readability
titanic$Survived <- as.factor(titanic$Survived)
levels(titanic$Survived) <- c("Dead", "Survived")
levels(titanic$Embarked) <- c("Unknown", "Cherbourg", "Queenstown", "Southampton")
str(titanic[,c("Embarked","Survived")])

# Is Sex a good predictor?


## Let's load the ggplot2 library first
library(ggplot2)
## plotting the Dead versus Survived   
## Store the plot for future modification
t1 <- ggplot(titanic, aes(x=Survived, fill=Sex)) 

## Use the object and add a bar plot
## The bars for Dead and Surived are further divided by Sex
t1 + geom_bar()   


## Faceting By gender
## We use facet_wrap to stitch two plots 
## facet_Wrap by Sex stitches the individual plots for female and male 
t1 + geom_bar(aes(color= Sex)) + facet_wrap(~ Sex)


## Faceting By embarked
## facet_wrap by Embarked stitches the plots for Cherbourg, Southampton, Queenstown, and Unknown (missing value)  
t1 + geom_bar(aes(color= Sex)) + facet_wrap(~ Embarked)



## Faceting by more than 1 variable
## facet_wrap can take only a single variable
## facet_grid is used to facet by more than one variable simultaneously 
## simultaneous facet by Sex and Embarked 
plt  <- ggplot(titanic, aes(x = Survived, fill = Sex)) + geom_bar()
plt  + facet_grid(Sex ~ Embarked)


# Is Age a good predictor?
## Let's look at the Age distribution by gender 
p <- ggplot(titanic, aes(Age, fill = Sex)) + geom_histogram()
p + labs(x ="Distribution of Age", y="Frequency of Bucket", title = "Distribution of Passenger Ages on Titanic")


## Boxplot of Age
## Plotting Age versus Survived
p <- ggplot(titanic, aes(Survived, Age)) 
p + geom_boxplot() 


## We can get the mean and median age amongst Dead and Survived  
summary(titanic$Age)
summary(titanic[titanic$Survived=="Dead",]$Age)
summary(titanic[titanic$Survived=="Survived",]$Age)


## Let's explore Age further 
## Density plot of Age by Sex 
q1 <- ggplot(titanic, aes(Age, color = Sex)) 
q1 + geom_density() 


## Density plot of Age by Survived
q2 <- ggplot(titanic, aes(Age, color = Survived)) 
q2 + geom_density()


## Exercise 3:
## 1. Create densityplot of "Age" on facet_grid egmented by Survived, Sex, and Embarked
## 2. Create densityplot of "Fare" on facet_grid egmented by Survived, Sex, and Embarked
## 3. Create boxplot of "Age" on facet_grid egmented by Survived, Sex, and Embarked
## 4. Create boxplot of "Fare" on facet_grid egmented by Survived, Sex, and Embarked



## Density plot of Age on facet_grid segmented by Sex and Embarked 
q3 <- ggplot(titanic, aes(x = Age, color = Survived)) + geom_density()
q3 + facet_grid(Sex ~ Embarked)

## Density plot of Fare on facet_grid segmented by Sex and Embarked 
q4 <- ggplot(titanic, aes(x = Fare, color = Survived)) + geom_density()
q4 + facet_grid(Sex ~ Embarked)


## Boxplot of Age on facet_grid segmented by Sex and Embarked 
q5 <- ggplot(titanic, aes(x = Survived, y = Age)) + geom_boxplot()
q5 + facet_grid(Embarked ~ Sex)



## Boxplot of Fare on facet_grid segmented by Sex and Embarked 
q6 <- ggplot(titanic, aes(x = Survived, y = Fare)) + geom_boxplot()
q6 + facet_grid(Embarked ~ Sex)



# Exercise 4:
# Create a new column "Child", and assign each row either "Adult" or "Child"
# based on a consistent metric. Then use ggplot to create a series of box plots
# relating Fare, Child, Sex, and Survived

child <- titanic$Age
child[child < 13] <- 0
child[child >= 13] <- 1
titanic$Child <- as.factor(child)
levels(titanic$Child)
levels(titanic$Child) <- c("Child", "Adult")
g <- ggplot(data=titanic[!is.na(titanic$Child),],
            aes(x=Child, y=Fare))
g.b <- g + geom_boxplot()






