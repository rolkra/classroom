#' Create fake data
#'
#' Fake data that can be used for unit-testing
#'
#' @param obs Number of observations
#' @param target_name Variable name of target
#' @param factorise_target Should target variable be factorised
#' (from 0/1 to facotr no/yes)?
#' @param target1_prob Probability that buy = 1
#' @param add_extreme Add an obervation with extreme values?
#' @param seed Seed for randomization
#'
#' @return A dataframe
#' @export

fakedata = function(obs = 1000, 
                    target_name = "target_ind",
                    factorise_target = FALSE,
                    target1_prob = 0.5, 
                    add_extreme = TRUE,
                    seed = 123) {
  
  # set seed (randomization)
  set.seed(seed)
  
  # create basic dataset
  data <- data.frame(
    id = seq(from = 100100100, to = 100100100 + obs - 1),
    period = rep(202012, obs),
    target_ind = sample(c(0, 1),
                        obs,
                        prob = c(1 - target1_prob, target1_prob),
                        replace = TRUE)
  )
  
  # add features
  data <- data %>%
    dplyr::mutate(
      age = round(ifelse(target_ind == 1,
                         rnorm(obs, mean = 45, sd = 10),
                         rnorm(obs, mean = 60, sd = 10)
      ), 0),
      city_ind = ifelse(target_ind == 1,
                        sample(c(0, 1), obs, replace = TRUE, prob = c(0.4, 0.6)),
                        sample(c(0, 1), obs, replace = TRUE, prob = c(0.6, 0.4))
      ),
      male_ind = ifelse(target_ind == 1,
                        sample(c(0, 1), obs, replace = TRUE, prob = c(0.3, 0.7)),
                        sample(c(0, 1), obs, replace = TRUE, prob = c(0.7, 0.3))
      ),
      fixedvoice_ind = ifelse(age > 70,
                              sample(c(0, 1), obs, replace = TRUE, prob = c(0.3, 0.7)),
                              sample(c(0, 1), obs, replace = TRUE, prob = c(0.95, 0.05))
      ),
      fixeddata_ind = 1,
      fixedtv_ind = ifelse(target_ind == 1,
                             sample(c(0, 1), obs, replace = TRUE, prob = c(0.4, 0.6)),
                             sample(c(0, 1), obs, replace = TRUE, prob = c(0.8, 0.2))
      ),
      mobilevoice_ind = sample(c(0, 1), obs, replace = TRUE, prob = c(0.6, 0.4)),
      mobiledata_ind = sample(c(0, 1), obs, replace = TRUE, prob = c(0.2, 0.8)),
      bbi_speed_ind = ifelse(age > 60,
                             sample(c(0, 1), obs, replace = TRUE, prob = c(0.9, 0.1)),
                             sample(c(0, 1), obs, replace = TRUE, prob = c(0.2, 0.8))
      ),
      bbi_usg_gb = ifelse(age > 75,
                          round(rnorm(obs, mean = 10, sd = 1)),
                          round(rnorm(obs, mean = 50, sd = 10))
      ) + city_ind * 20 + target_ind * 10,
      hh_single = ifelse(age < 35 & city_ind == 1,
                         sample(c(0, 1), obs, replace = TRUE, prob = c(0.2, 0.8)),
                         sample(c(0, 1), obs, replace = TRUE, prob = c(0.7, 0.3))
      ),
      event_web = 1
      
  ) # mutate
  
  # factorise target?
  if (factorise_target) {
    data$buy <- factor(data$buy,
                              levels = c(0, 1),
                              labels = c("no", "yes"))
  }
  
  # add extreme values?
  if (add_extreme) {
    extreme <- data[nrow(data), ]
    extreme$bbi_usg_gb <- 100000
    data <- data %>% bind_rows(extreme)
  }
  
  # rename target?
  if (target_name != "target_ind") {
    
    data[[target_name]] <- data$target_ind
    data$target_ind <- NULL
    
  }
  
  # return data
  data
  
} # fakedata
