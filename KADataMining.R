library(readxl)
library(ggplot2)
library(dplyr)


################################ CCU01C MISSION: Third Grade


################  this is the data taken from CCU01C class

Summary <- read_excel("Desktop/Projects/Untitled Folder/CCU01C/Khan Academy Report -- 02-17-1970 to 02-28-2018.xlsx", sheet = 'Summary')
Mission.specific <- read_excel("Desktop/Projects/Untitled Folder/CCU01C/Khan Academy Report -- 02-17-1970 to 02-28-2018.xlsx", sheet = "Mission-specific")
Exercises <- read_excel("Desktop/Projects/Untitled Folder/CCU01C/Khan Academy Report -- 02-17-1970 to 02-28-2018.xlsx", sheet = "Exercises")
Videos <- read_excel("Desktop/Projects/Untitled Folder/CCU01C/Khan Academy Report -- 02-17-1970 to 02-28-2018.xlsx", sheet = "Videos")
Points <- read_excel("Desktop/Projects/Untitled Folder/CCU01C/Khan Academy Report -- 02-17-1970 to 02-28-2018.xlsx", sheet = "Points")
Badges <- read_excel("Desktop/Projects/Untitled Folder/CCU01C/Khan Academy Report -- 02-17-1970 to 02-28-2018.xlsx", sheet = "Badges")
Skills <- read_excel("Desktop/Projects/Untitled Folder/CCU01C/Khan Academy Report Skill Progress.xlsx")



####Filter for  only the exercises that are part of the 3rd grade mission 

grade3.ex <- Exercises[Exercises$Exercise %in% Skills$Exercise,]


#what is the one exercise in the 3rd grade mission that no student has yet attempted?

nrow(Skills)

length(unique(grade3.ex$Exercise))

n <-   unique(grade3.ex$Exercise)  
p <- Skills$Exercise


setdiff(p,n)



######## Create a total minutes spent on Mission Collumn

ccu01c <- Mission.specific

m <- grade3.ex %>% select(Student,`Time Spent (min)`) %>% group_by(Student) %>% summarise(total.minutes = sum(`Time Spent (min)`))

m <- as.data.frame(m)

ccu01c <- full_join(ccu01c,m, by="Student")



#####Creating a totalHours Collumn


myHours <- function (x) {
  return (x/60)
}



totalHours <- lapply(ccu01c$total.minutes, myHours)
totalHours <- unlist(totalHours)
ccu01c <- cbind(ccu01c,totalHours)

#### Summarize of totalHours

summarise(ccu01c, maxHours=max(ccu01c$totalHours,na.rm=TRUE), medianhours=median(ccu01c$totalHours,na.rm=TRUE), meanHours=mean(ccu01c$totalHours,na.rm=TRUE))

###### Creating a Weeks Performing Collumn

myWeeks <- function (x) {
  return (x/3)
}

totalWeeks <- lapply(ccu01c$totalHours, myWeeks)
totalWeeks <- unlist(totalWeeks)
ccu01c<- cbind(ccu01c,totalWeeks)



summarise(ccu01c, max=max(ccu01c$totalWeeks,na.rm=TRUE), median=median(ccu01c$totalWeeks,na.rm=TRUE), mean=mean(ccu01c$totalWeeks,na.rm=TRUE))


########### Time Spent on Exercises

##### Overall Numbers for Time Spent on mission exercises

summarise(grade3.ex, max = max(`Time Spent (min)`),
          min = min(`Time Spent (min)`),
          avg = mean(`Time Spent (min)`),)

### Time spent by
i <- grade3.ex %>% select(Student,`Time Spent (min)` ) %>% group_by(Student) %>% summarise(Ex.time.max = max(`Time Spent (min)`),
                                                                                      Ex.time.min = min(`Time Spent (min)`),
                                                                                      ex.time.avg = mean(`Time Spent (min)`),
                                                                                      ex.time.median = median(`Time Spent (min)`))                                                                                   



##### Summarize time spent per  Excercise

 d <- grade3.ex %>% select(Exercise,`Time Spent (min)` ) %>% group_by(Exercise) %>% summarise(Ex.time.max = max(`Time Spent (min)`),
                                                                                      Ex.time.min = min(`Time Spent (min)`),
                                                                                      ex.time.avg = mean(`Time Spent (min)`),
                                                                                      ex.time.median = median(`Time Spent (min)`)) 
i <- as.data.frame(i)

ccu01c <- full_join(ccu01c,i, by="Student")


##### Create a progress collumn

nSkills <- nrow(Skills)

prog <- function (a,b,c,d,e) {
  mastered <- a *4
  level2 <- b *3
  level1 <- c * 1
  practiced <- d*1
  per <- (mastered+level2+level1+practiced)/(4*e)
  return(per)
  
}


Progress <- mapply(prog, Mission.specific$Mastered,
                   Mission.specific$`Level 2`, Mission.specific$`Level 1`, Mission.specific$Practiced, nSkills)

ccu01c <- cbind(ccu01c,Progress)

head(ccu01c)


ccu01c <- ccu01c[order(ccu01c$Progress, decreasing = TRUE),]


###################### Find the exercise students struggle with the most

#z <- Exercises %>% select(Exercise,`Current Status` ) %>% group_by(`Exercise`) %>% summarise(nstudents= n_distinct(Student))


z <- filter(grade3.ex, `Current Status`=='Struggling')

z <- table(z$Exercise)

z<- as.data.frame(z)

x <- filter(z, Freq>5)



######################################################  Graphs  ###################################################### 

hist(ccu01c$Progress, main= " Distribution of Progress on 3rd grade mission #CCU01C")



plot(ccu01c$ex.time.avg,ccu01c$Progress, main= "Does  avg. time per Exercise on Mission. affect Progress #CCU01C", xlab="Ex. Time avg", ylab= " Progress" )

plot(ccu01c$ex.time.median,ccu01c$Progress, main= "Does median time per Ex. affect Progress #CCU01C", xlab="Ex. Time median", ylab= " Progress" )


hist(ccu01c$Progress, main= " Distribution of Progress #CCU01C")

######### breakdown of  current status
(bean <- table(grade3.ex$`Current Status`))


ggplot(grade3.ex,aes(grade3.ex$`Current Status`))+geom_bar()


########################  Time Series 


##### Time series of Student participation per day

grade3.ex$`Last Done` <- as.Date(grade3.ex$`Last Done`) 


d <- grade3.ex %>% select(`Last Done`,Student ) %>% group_by(`Last Done`) %>% summarise(nstudents= n_distinct(Student))


ggplot(d, aes(`Last Done`, nstudents)) + geom_line() +
  scale_x_date() + xlab("  ") + ylab(" # Students on KA ") + coord_cartesian(ylim = c(0, 11)) 



###### Time series of median time spent on KA per day


c <- grade3.ex %>% select(`Last Done`,`Time Spent (min)` ) %>% group_by(`Last Done`) %>% summarise(avg.time= median(`Time Spent (min)`))                                                                          




ggplot(c, aes(`Last Done`, avg.time)) + geom_line() +
  scale_x_date() + xlab("  ") + ylab(" Median Time Spent ") 



