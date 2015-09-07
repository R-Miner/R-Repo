# The e1071 package for running SVM on the data set
require("e1071")

png("Graphs.png")
#----------------------------------------------------------------------------------------------------------------------
#					Variable declarations
#----------------------------------------------------------------------------------------------------------------------
max_population<-20
mutation_rate<- 0.25; mutate<-0
n<-2; svm_list<<-vector("list",n); error<<- numeric(n);
avg_error<-numeric(n); min_error<-numeric(n)


# Generates matrix of 0's and 1's
population<-matrix(sample(c(0,1), max_population*43, replace = TRUE), ncol=43) 
print(population)

#Reads the original file
kdd_train<-read.csv("kdd_training_numeric.csv", header = TRUE) #c(1,5,6:11,13:20,23:42)

#----------------------------------------------------------------------------------------------------------------------
# 					Converts binary to decimal
#----------------------------------------------------------------------------------------------------------------------
bin2dec <- function(vec)
{
b <- as.numeric(unlist(strsplit(as.character(as.numeric(vec)), "")))
sum(2 ^ ((length(b) - 1):0)[b == 1])
}

#----------------------------------------------------------------------------------------------------------------------
#					 Evaluation function :svm and predict
#----------------------------------------------------------------------------------------------------------------------

evaluate <- function(start,stop=start)
{
#print("in evaluate")
for (r in start:stop){

# Creating new folder for each data samples
newdir <- paste("data_set_", r,sep ="")
dir.create(newdir)

# All the rows with cloumn 36 minus 
minus_data <- kdd_train[(1:length(which(kdd_train[ ,36] == -1))), c(which(population[r,9:42] == 1),36)]

# Training set of size 1000
training_data_1000 <- rbind(head(minus_data, 500), tail(kdd_train[ ,c(which(population[r,9:42] == 1),36)], 500))
write.csv(training_data_1000, file = file.path(newdir, paste("training_data_1000_", r, ".csv", sep ="")), append = FALSE, quote = FALSE, row.names = FALSE)

#Testing set
testdata <- rbind(tail(minus_data, (length(which(kdd_train[ ,36] == -1))-500)), kdd_train[((length(which(kdd_train[ ,36] == -1))+1):(nrow(kdd_train)-500)),c(which(population[r,9:42] == 1),36)])
write.csv(testdata, file = file.path(newdir, paste("test_data_", r, ".csv", sep ="")), append = FALSE, quote = FALSE, row.names = FALSE)

svm_list[[r]] <<- svm(read.csv( file =file.path(newdir, paste("training_data_1000_", r, ".csv", sep ="")), header = TRUE)[ ,c(1:length(which(population[r,9:42]==1)))],as.factor(read.csv( file = file.path(newdir, paste("training_data_1000_", r, ".csv", sep ="")), header = TRUE)[ ,length(which(population[r,9:42]==1))+1]),kernel = "polynomial",cost = (bin2dec(population[r,1:4])+1)* 1/16, gamma = (bin2dec(population[r,5:8])+1)* 1/16, scale = FALSE)

#print("success")

# Misclassification Error on test data set
error[r] <<- 1-sum(as.factor(read.csv( file = file.path(newdir, paste("test_data_", r, ".csv", sep ="")), header = TRUE)[ ,length(which(population[r,9:42]==1))+1]) == predict(svm_list[[r]],read.csv( file = file.path(newdir, paste("test_data_", r, ".csv", sep ="")), header = TRUE)[ ,c(1:length(which(population[r,9:42]==1)))]))/length(as.factor(read.csv( file = file.path(newdir, paste("test_data_", r, ".csv", sep ="")), header = TRUE)[ ,length(which(population[r,9:42]==1))+1]))

} # End of cost loop
}

#----------------------------------------------------------------------------------------------------------------------
#			Sorting: for finding the best chromosome, minimum error and average error
#----------------------------------------------------------------------------------------------------------------------

pick_chroms <- function(population,i)
{
#print("in sorting")
#Sorting 3 of them in increasing order of error
ord_error <- order(error); 
error <- error[ord_error]; svm_list <- svm_list[ord_error]; population <- population[ord_error, ]
min_error[i] <<- error[1]
best_svm <<- svm_list[1]
fittest <<- error[1]
avg_error[i] <<- mean(error,max_population)
}
#-----------------------------------------------------------------------------------------------------------------------
#			Selection:Selecting the chromosomes for crossover and mutation(Roulette Wheel)
#-----------------------------------------------------------------------------------------------------------------------
selection <- function(population)
{
print("in selection")
start <- 1; end <- max_population; popul <- matrix(nrow = max_population, ncol = 43) 
error_so_far <- 0

for(i in 1:max_population)
{
random <- runif(1, 0,sum(error))
error_so_far= error_so_far + error[i]

#if the error_so_far > random number return the chromo at this point
if (error_so_far >= random)
{
popul[start,c(1:43)] <- population[i,c(1:43)]
start <- start+1	
print("selected")
print(i)
}
else
{	
	popul[end,c(1:43)] <-population[i,c(1:43)] 
	end <- end-1
}

}
population <<- popul
rm(popul)
}

#-----------------------------------------------------------------------------------------------------------------------
#			Crossover:Single-point crossover at a random chromosome point.
#-----------------------------------------------------------------------------------------------------------------------

crossover<- function(population)
{
#print("in crossover")
crosspoint<- as.integer(runif(1, 9, 43))#sample(9:43, 1)
popul<- population
for (i in seq(1,max_population/2,2)) 
{
popul[c(i,i+1), c(crosspoint:43)] <- popul[c(i+1,i),c(crosspoint:43)]
}
population[c(max_population/2 +1:max_population), ] <<- popul[c(1:max_population/2), ]
rm(popul)
}
#-----------------------------------------------------------------------------------------------------------------------
#			Mutation: Mutates depending on mutation rate
#-----------------------------------------------------------------------------------------------------------------------

mutation<- function(population)
{
print("in mutation")

for (i in 1:max_population)
{
random<-runif(1, 0,1)
print(random)

if(random < mutation_rate)
{
mutation_bit<- as.integer(runif(1, 9, 43))
if (population[i,mutation_bit] == 0) 
	population[i,mutation_bit] <<- 1
else if (population[i,mutation_bit] == 1) 
	population[i,mutation_bit] <<- 0
mutate<<-i
break
}
}
}
#-----------------------------------------------------------------------------------------------------------------------
#						Main Sequence
#-----------------------------------------------------------------------------------------------------------------------

#1st generation
evaluate(1,max_population)

# next 10 generations 
for(i in 1:10)
{
selection(population)
print(error)
pick_chroms(population,i)
print(i)
#print("after sorting=============================")
#print(error)
generation<<-i
if(all(duplicated(error)[-1]))
{
break;
}
crossover(population)

mutation(population)
if(mutate > 0 && mutate < max_population/2 +1)
{	
	evaluate(mutate)
	mutate <<- 0
}

evaluate(max_population/2 +1,max_population)
}
#Sorting last generation
#print("out of the loop")
pick_chroms(population,i+1)
#print(error)
plot(c(1:(generation+1)),min_error, pch=18,xlim=c(0,(generation+1)),ylim=c(0,1), xlab= "generation",ylab = "Minimum Error")
plot(c(1:(generation+1)),avg_error, pch=18,xlim=c(0,(generation+1)),ylim=c(0,1), xlab= "generation",ylab = "Average Error")

#plot(c(1:(generation+1)),min_error, pch=18,xlim=c(0,(generation+1)),ylim=c(0,1))
#points(c(1:(generation+1)),avg_error, type='p', col="red", xlab='x', ylab='y')

dev.off()
