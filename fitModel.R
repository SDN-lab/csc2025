fitModel <- function(data, params, model_id = "one_k_one_beta_linear") {
  # Fits parameters to model and calculates softmax and negative log-likelihood

  # Get reward, effort, and choices from data
  reward = data$reward
  effort = data$effort
  choices = data$choices
  recipient = data$recipient

  # Assign k parameter values
  if (grepl("one_k", model_id, fixed = TRUE)) {
    n_ks = 1
    k_param = rep(params[1], length(recipient))
  } else if (grepl("two_k", model_id, fixed = TRUE)) {
    n_ks = 2
    k_param = (recipient == "food") * params[1] + (recipient == "climate") * params[2]
  } else {
    stop("k parameters in '", model_id, "' misspecified.")
  }

  # Assign beta parameter values
  if (grepl("one_beta", model_id, fixed = TRUE)) {
    beta_param = rep(params[1 + n_ks], length(recipient))
  } else if (grepl("two_beta", model_id, fixed = TRUE)) {
    beta_param = (recipient == "food") * params[1 + n_ks] + (recipient == "climate") * params[2 + n_ks]
  } else {
    stop("beta parameters in '", model_id, "' misspecified")
  }

  # Compute subjective value of "work" option
  # (Based on type of model specified by 'model_id')
  if (grepl("linear", model_id, fixed = TRUE)) {
    SV = reward - (effort * k_param)
  } else if (grepl("para", model_id, fixed = TRUE)) {
    SV = reward - ((effort^2) * k_param)
  } else if (grepl("hyper", model_id, fixed = TRUE)) {
    SV = reward / (1 + (effort * k_param))
  } else {
    stop("Model ID '", model_id, "' is incorrectly specified.")
  }

  # Calculate probabilities of choosing work option
  prob = exp(SV * beta_param) / (exp(SV * beta_param) + exp(1 * beta_param))

  # Probability of choosing "rest" option
  prob[!choices] = 1 - prob[!choices]

  # Convert probabilities to log-likelihoods
  # Note: the LogSumExp function can be used to calculate log-likelihoods in a more numerically stable way (<https://en.wikipedia.org/wiki/LogSumExp>)
  LL = log(prob)
  negLL = -sum(LL) # sum LL together and flip sign to make values positive (for fmin minimizing function)

  return(negLL)
}
