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
pockets
biased_pockets <- c(2, 4, 21)
biased_prob <- 0.028

#this is to find probability of unbiased pockets after applying the biased ones
remaining_prob <- 1 - 3/38 - (biased_prob * 3)
remaining_prob
prob_each <- remaining_prob / 35 # evenly distribute the remaining probability over the other pockets
prob_each
probs <- ifelse(pockets %in% biased_pockets, 1/38 + biased_prob, prob_each)
#probs <- c(1/38 + 0.028, 1/38 + 0.028, 1/38 + 0.028, rep(prob_each, 35)) # put all those probabilities into the same vector
probs
sum(probs) # check that the probabilities still sum to 1

# you'll have to modify this when more than one pocket is biased
# ```
# 
# ```{r}
# sample like in previous homeworks
num_sims = 5000
spins <- sample(pockets, size = num_sims, replace = TRUE, prob = probs)
spins <- factor(spins, levels = c("00", 0:36)) # sorts the plot labels from 00 to 36 

# table gets the frequencies
# barplot makes the plot
barplot(table(spins))
hist(table(spins))
# ```






# Problem 2
# 
# For each lottery draw, randomly select 5 numbers from the range 1 to 47 and a Mega number from the range 1 to 27.
# 
# ```{r}
# for one day
set.seed(123)
# generate winning numbers of a single draw
winning_5_numbers <- sample(1:47, 5)
winning_mega_number <- sample(1:27, 1)

# simulate 500,000 lottery draws
prizes <- character(500000)
for(i in 1:500000) {
  player_5_numbers <- sample(1:47, 5)
  player_mega_number <- sample(1:27, 1)
  
  # match 5 numbers between win and player lottery
  matches_5 <- sum(player_5_numbers %in% winning_5_numbers)
  match_mega <- player_mega_number == winning_mega_number
  
  # issue out prizes
  if(matches_5 == 5 && match_mega) {
    prizes[i] <- "first prize"
  } else if(matches_5 == 5 && !match_mega) {
    prizes[i] <- "second prize"
  } else if(matches_5 == 4 && match_mega) {
    prizes[i] <- "third prize"
  } else {
    prizes[i] <- "no prize"
  }
}

# calculate prize occurrence
prize_counts <- table(prizes)
print(prize_counts)

# calculate simulated probabilities
simulated_probabilities <- prize_counts / 500000
print(simulated_probabilities)

# calculate theoretical/analytic probabilities

# total combos: C(47, 5) * 27 -- essentially the first prize already
first_prize_theoretical <- 1 / (choose(47, 5) * 27)
# second prize: C(47,5) ways to choose 5 numbers correctly, 26/27 ways to choose Mega incorrectly
second_prize_theoretical <- 1 / (choose(47, 5)) * (26 / 27)
# third prize: ways to match 4 out of 5: C(5,4) * C(42,1) = 5 * 42 = 210
# Probability = [C(5,4) * C(42,1)] / [C(47,5)] * (1/27)
third_prize_theoretical <- (choose(5, 4) * choose(42, 1)) / choose(47, 5) * (1 / 27)

# print theoretical probabilities
cat("Theoretical probabilities:\n")
cat("First prize:", first_prize_theoretical, "\n")
cat("Second prize:", second_prize_theoretical, "\n")
cat("Third prize:", third_prize_theoretical, "\n")

# compare simulated and theoretical probabilities
comparison <- data.frame(
  Prize = c("First Prize", "Second Prize", "Third Prize"),
  Simulated = c(
    simulated_probabilities["first prize"], 
    simulated_probabilities["second prize"], 
    simulated_probabilities["third prize"]
  ),
  Theoretical = c(
    first_prize_theoretical,
    second_prize_theoretical,
    third_prize_theoretical
  )
)

# column for the ratio of simulated to theoretical
comparison$Ratio <- comparison$Simulated / comparison$Theoretical

# print comparison
print(comparison)

# plot the results for visualization
barplot(
  rbind(comparison$Simulated, comparison$Theoretical),
  beside = TRUE,
  names.arg = comparison$Prize,
  col = c("blue", "red"),
  main = "Simulated vs. theoretical lottery probabilities",
  ylab = "Probability",
  legend.text = c("Simulated", "Theoretical")
)