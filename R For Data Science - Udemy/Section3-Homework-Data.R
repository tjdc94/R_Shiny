#Data
r <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
e <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)

#Solution

#Profit per month
pm <- round(r, 2) - round(e, 2)

#Profit after tax
pat <- pm * 0.7

#Profit margin per month
pmm <- round(pat / r * 100, 0)

#Good Month
for (i in pat){
 gm[i] <- i > mean(pat)
}


#Bad Month
for (i in pat){
  bm <- i < mean(pat)
}

#Best Month

best <- max(round(pat,digits = 2)) / 1000
round(bm, 0)

#Worst Month

worst <- min(round(pat,2)) / 1000
round(bm, 0)


print(gm)

