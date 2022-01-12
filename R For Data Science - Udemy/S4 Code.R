games
Games
MinutesPlayed

my.data <- c(1:20)
my.data

A <- matrix(my.data, 4, 5)
A
A[2,3]

B <- matrix(my.data, 4, 5, byrow=T)
B
B[2,5]
?matrix()


?rbind()
r1 <- c("I", "am", "sad")
r2 <- c("what", "a", "Time")
r3 <- c(1,2,3)
C <- rbind(r1,r2,r3)
C


c1 <- 1:5
c2 <- -1:-5
D <- cbind(c1,c2)
D


charlie <- 1:5
charlie

names(charlie) <- c("a","b","c","d","e")
charlie
charlie["d"]
names(charlie)

temp.vec <- rep(c("a","B","zZ"), each=3)
temp.vec

Bravo <- matrix(temp.vec, 3, 3)
Bravo

rownames(Bravo) <- c("How", "are", "you?")
Bravo

colnames(Bravo) <- c("X", "Y", "Z")
Bravo

Bravo[2,2] <- 0
Bravo

rownames(Bravo)

round(FieldGoals / Games, 1)
round(MinutesPlayed / Games)

round(FieldGoals / FieldGoalAttempts, 2)

t(FieldGoals)

matplot(t(FieldGoals/FieldGoalAttempts), type = "b", pch = 15:18, col=c(1:4,6))
legend("bottomleft", inset=0.01, legend=Players, col=c(1:4,6), pch = 15:18, horiz=F)

Games[1,10,]
Games[c(1,10),]
Games[,c("2012", "2013")]


#Creeating a function
myplot <- function(data, rows=1:10){
#set default value to 1:10
Data <- data[rows,,drop=F]
matplot(t(Data), type = "b", pch = 15:18, col=c(1:4,6))
legend("bottomleft", inset=0.01, legend=Players[rows], col=c(1:4,6), pch = 15:18, horiz=F)
}

myplot(MinutesPlayed/Games, 1:3)





