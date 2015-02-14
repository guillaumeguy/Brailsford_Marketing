# Author: Guillaume Guy
# Date: 2015/02/14
setwd("C:/Users/gguy/Desktop/Blog/20150214 Incremental_Improvement_Braisford")

# ----------- YEARLY PARAMETERS --------------- #

new_customers_acquisition <- 50

spend_per_state <- 100


## Transition Matrix:

transition <- matrix(c(80,10,10,0  # New customers to ...
                       ,0,80,20,0 # Loyal customers to ...
                       ,0,5,70,25  # At Risk customers to ...
                       ,0,1,2,97)
                       ,byrow = T,ncol = 4)


# in thousands:
starting_state <- c(500,1000,3000,10000)

# Improvement
improvement <- 1 # 1 %

# Support function
lappend <- function(lst, obj) {
  if (typeof(obj) != "list") {
    lst[[length(lst) + 1]] <- obj
    return(lst)
  }
  if (typeof(obj) == "list" & typeof(lst) != "list") {
    lstout <- list(lst)
    for (i in 1:length(obj)) {
      lstout[[length(lstout) + 1]] <- obj[[i]]
    }
    return(lstout)
  }
  if (typeof(obj) == "list" & typeof(lst) == "list") {
    for (i in 1:length(obj)) {
      lst[[length(lst) + 1]] <- obj[[i]]
    }
    return(lst)
  } else return(NA)
} 

# generate new state after 1 cycle
iterate <- function(matrix_transition,current_state,recruiting){
  
  
  results <- matrix(current_state,ncol=4) %*% matrix_transition / 100 
    
  #results <- results[1:3] 
  
  # Add acquired customers 
  results[1] <- results[1] + recruiting
  
  return(results)
}


# generate future all states given initial state + transition
generate_iterations <- function(model ='normal',starting_state_var,transition_var,new_customers_acquisition_var, it = 10){

mylist <- list()

new_state <- starting_state_var

for(i in 1:it){
  
  new_state <- iterate(transition_var,new_state,new_customers_acquisition)
  
  mylist[[i]] <- c(iteration=i,state=new_state) 

  
  }
  
  
mylist <- lappend(mylist,c(iteration=0,state=starting_state_var) )

results <- do.call('rbind',mylist)

results <- as.data.frame(results)

results[,'model'] <- model

return(results)
}


############### Normal Pass #############
normal_model <- generate_iterations(model = 'normal',starting_state,transition,new_customers_acquisition)


############### 1% improvement Pass: #############

# improvement 
transition_improved <- transition


# New Customer
new_customers_acquisition_improved <- new_customers_acquisition * (1+ improvement)
transition_improved[1,2] <-  transition_improved[1,2] + improvement
transition_improved[1,3] <-  transition_improved[1,3] - improvement

# Active
transition_improved[2,2] <-  transition_improved[2,2] + improvement
transition_improved[2,3] <-  transition_improved[2,3] - improvement

# At Risk
transition_improved[3,2] <-  transition_improved[3,2] + improvement
transition_improved[3,3] <-  transition_improved[3,3] - improvement /2
transition_improved[3,4] <-  transition_improved[3,4] - improvement /2 

# Lost
transition_improved[4,2] <-  transition_improved[4,2] + improvement /2 
transition_improved[4,3] <-  transition_improved[4,3] + improvement /2
transition_improved[4,4] <-  transition_improved[4,4] - improvement 


incremental_improvement_model <- generate_iterations(model = 'incremental',starting_state,transition_improved,new_customers_acquisition_improved)

## Merge Results and export
final_results <- rbind(normal_model,incremental_improvement_model)


write.csv(final_results,file='2015_02_15_simulation.csv')