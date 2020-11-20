library(dplyr)
library(tidyr)
library(ggplot2)

n <- 500
m <- 5000

# personal
p_t_given_rep <- c(0.9, 0.1)
p_t_given_dem <- c(0.01, 0.99)
p_party <- c(0.5, 0.5)

# precinct
all_p_rep <- rbeta(n, 1, 5)

all_votes <- NULL

# simulate n precincts
for (i in 1:n){
  
  #m = round(400 - 300 * all_p_rep[i])
  
  p_party_aff <- c(all_p_rep[i], 1-all_p_rep[i])
  
  # simulate m people
  votes <- tibble(
    precinct = i,
    party = sample(c('r', 'd'), size=m, replace=T, prob=p_party_aff)
    ) %>%
    mutate(
      tr = if_else(party == 'r',
        sample(c(T,F), size=m, replace=T, prob=p_t_given_rep),
        sample(c(T,F), size=m, replace=T, prob=p_t_given_dem)),
      vote = if_else(party == 'r',
        if_else(tr,
          sample(c('r', 'tr'), size=m, replace=T, prob=p_party),
          'bi'),
        if_else(tr,
          'tr',
          sample(c('d', 'bi'), size=m, replace=T, prob=p_party))))
  
  all_votes = bind_rows(all_votes, votes)
}


# Counting to make the plot
precinct_counts <- all_votes %>%
  group_by(precinct, vote) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = vote, values_from = count) %>%
  mutate(true_rep_prop = all_p_rep)

plot_df <- precinct_counts %>%
  mutate(
    rep_prop = r / (r+d),
    tr_prop = tr / (tr+bi),
    tr_prop_delta = tr_prop - rep_prop,
    bi_prop_delta = (1-tr_prop) - (1-rep_prop),
    party_prop = (r+d)/(r+d+tr+bi))

g <- ggplot(plot_df, aes(x = rep_prop, y = tr_prop_delta)) +
  geom_point()
  #geom_point(aes(y = party_prop, color='red'))

g2 <- ggplot(plot_df, aes(x = rep_prop, y = tr_prop)) + geom_point()

g3 <- ggplot(plot_df, aes(x = 1-rep_prop, y = bi_prop_delta)) + geom_point()

g4 <- ggplot(plot_df, aes(x = 1-rep_prop, y = 1-tr_prop)) + geom_point()

g5 <- ggplot(plot_df, aes(x = tr_prop, y = true_rep_prop)) + geom_point()

g6 <- ggplot(plot_df, aes(x = rep_prop, y = true_rep_prop)) + geom_point()

g7 <- ggplot(plot_df, aes(x = true_rep_prop, y = tr_prop_delta)) + geom_point()
