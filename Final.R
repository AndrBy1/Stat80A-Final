#Stat80A Final
#Darren Ung, Phoebe Mason, Andrew Byi

# ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE)
# ```
# 
# # Making figures the right size
# 
# If you try to plot something directly, you might get something that looks 
# not quite right (longer or shorter in one dimension than it should be):
# 
# ```{r}
# plot(1:9, 1:9)
# ```
# 
# To change the size and shape manually, follow these steps:
#   
#   First, click the gear icon (next to "Knit") and then click "Chunk Output in 
# Console". (Do this now.) Now, plots will appear in the "Plots" window in 
# RStudio.
# 
# ```{r}
# plot(1:9, 1:9)
# ```
# 
# Now, you can click and drag the border between this window (the file you are 
#                                                             editing) and the "Plots" window until the shape and size of the plot looks 
# good.
# 
# Once the plot looks good, go to the Console and type `dev.size()`, and hit 
# Enter. You'll get two numbers output; let's call them x and y. Insert a chunk 
# and where it says {r}, instead write 
# 
# {r, fig.dim = 2 * c(x, y), fig.align = 'center'}
# 
# (but instead of x and y, put the actual numbers output by `dev.size()`). It's 
# fine to round the numbers to the nearest tenth (one decimal place). The chunk 
# should now look like this:
# 
# ```{r, fig.dim = 2 * c(3.2, 3.4), fig.align='center'}
# plot(1:9, 1:9)
# ```
# 
# The two numbers 3.2 and 3.4 might be different for you; it depends on your 
# computer. Everything else should be the same.

# Problem 1 Hint

# 38 pockets
# 
# each has probability 1/38
# 
# I want to bias one pocket (00) by adding 0.07 probability to it.
# 
# ```{r}
pockets <- c("00", 0:36)

remaining_prob <- 1 - 1/38 - 0.07
prob_each <- remaining_prob / 37 # evenly distribute the remaining probability over the other pockets
probs <- c(1/38 + 0.07, rep(prob_each, 37)) # put all those probabilities into the same vector
sum(probs) # check that the probabilities still sum to 1

# you'll have to modify this when more than one pocket is biased
# ```
# 
# ```{r}
# sample like in previous homeworks
spins <- factor(spins, levels = c("00", 0:36)) # sorts the plot labels from 00 to 36 

# table gets the frequencies
# barplot makes the plot
barplot(table(spins))
# ```

# Problem 2
# 
# For each lottery draw, randomly select 5 numbers from the range 1 to 47 and a Mega number from the range 1 to 27.
# 
# ```{r}
# for one day
set.seed(123)
winning_5_numbers <- sample(1:47, 5)
winning_mega_number <- sample(1:27, 1)

prizes <- character(500000)
for(i in 1:500000) {
  first_five_nums <- sample(1:47, 5)
  mega_number <- sample(1:27, 1)
  
  if(winning_mega_number == mega_number & all(first_five_nums %in% winning_5_numbers)) {
    # it's a first prize
    prizes[i] <- "first prize"
  } else if(TRUE) { # but remove TRUE with the logical condition of getting second prize (you can figure this out) 
    prizes[i] <- "second prize"
  } else if(TRUE) {# but remove TRUE with the logical condition of getting third prize
    prizes[i] <- "third prize"
  } else {
    prizes[i] <- "no prize"
  }
}

# hint for third prize
sum(x %in% y) # counts # of matches between x and y.
# If x and y share 4 numbers in common, then sum(x %in% y) == 4 
# ```
