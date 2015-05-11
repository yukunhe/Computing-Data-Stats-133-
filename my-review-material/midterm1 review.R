# youtube UCB STAT133 FALL 2013 Hawk Ibser
# bcourses lecture spector notes 2011
# ggplot, dataframes, or any all, subsetting, apply

summary(housing$date)
# gives min, q1, median and mean

tmp <- housing$date
class(tmp)

names(housing)
summary(housing$name)

housing.less3 <- housing[housing$br<3, ]
dim(housing)
table(housing.less3$br)

housing.less3.Berkeley <- housing[(housing$br <3) & (housing$city == "Berkeley"), ]
head(housing.less3.Berkeley)

table(i%%j)
# just look at the first value; true and false; single% look at every element

housing.less3.Berkeley <- housing[(housing$br <3) & (housing$city == "Berkeley"), c("city", "br","date")]
# first condition for rows and second condition for columns


any(k)
all(k)
# one true false value. at least one true is true; all of them are true is true


food <- list(veg=c("tomato", "pepper", "cucumber"), fruit = c(...), dairy = c(...))
# list

food[[4]]>6

x <- rnorm(1000)
y<- sample(1:100)
y<-sample(1:100, size =1000, replace=T)
fit<- lm(y-x)
fit$coef


# apply, sapply, lapply
#do the same calculaton over and over in one project, for loops and while loops, if statments
# sapply is a simple version of lapply

replicate(1000, rnorm(n))

help(lapply)
lapply(food, "[",1)
lapply(food, length)
# return as list
sapply(food, length)
# do what lapply but return 
# easier than for loop

x1 <- matrix(rnorm(9), col= 3)


#vectors
a=c(1,2,3)
a[1]
# [1] 1
a[c(1,2)]
#[1]1 2
# extract element except the first and the last
a[-c(1,3)]
#[1] 2

# rep function
rep(5,3)
#[1] 5 5 5
rep(1:4, c(2,3,3,4))
#[1] 1 1 2 2 2 3 3 3 4 4 4 4

# recycle
c(1,2,3,4)+c(1,2)
#[1]2 4 4 6









# practise midterm

x <- seq(2,20,by=2)

y<-seq(-1,-19,by=-2)

z<-rep(1:5,c(4,4,4,4,4))

m<-matrix((1:12)*10,3,4,byrow=T)

p<-list(x,y,z,m)

nmat<-matrix(rnorm(10000,mean=6,sd=2),100,100)

type.rain<-class(rain)

name.stations<-names(rain)

n.obs<-c(sum(rain$st050183),sum(rain$st050263),sum(rain$st050712),sum(rain$st050843),sum(rain$st050263))

max.rain<-sapply(rain, max)

boxplot(rain)

