simulateData <- function(true_params, model_id = "one_k_one_beta_linear") {
  # Generates simulated data for a participant based on the "true" parameters supplied
  reward = rep(2:4, times = 8)
  effort = rep(2:5, each = 3, times = 2) # repeat same 12 trials for each recipient
  recipient = rep(c("food", "climate"), each = 12)

  # Define k parameters
  if (grepl("one_k", model_id, fixed = TRUE)) {
    n_ks = 1
    k_param_true = rep(true_params[1], length(recipient))
  } else if (grepl("two_k", model_id, fixed = TRUE)) {
    n_ks = 2
    k_param_true = (recipient == "food") * true_params[1] + (recipient == "climate") * true_params[2]
  } else {
    stop("k parameters in '", model_id, "' misspecified")
  }

  # Define beta parameters
  if (grepl("one_beta", model_id, fixed = TRUE)) {
    beta_param_true = rep(true_params[1 + n_ks], length(recipient))
  } else if (grepl("two_beta", model_id, fixed = TRUE)) {
    beta_param_true = (recipient == "food") * true_params[1 + n_ks] + (recipient == "climate") * true_params[2 + n_ks]
  } else {
    stop("beta parameters in '", model_id, "' misspecified.")
  }

  # Compute subjective value of "work" option
  # (Based on type of model specified by 'model_id')
  if (grepl("linear", model_id, fixed = TRUE)) {
    SV = reward - (effort * k_param_true)
  } else if (grepl("para", model_id, fixed = TRUE)) {
    SV = reward - ((effort^2) * k_param_true)
  } else if (grepl("hyper", model_id, fixed = TRUE)) {
    SV = reward / (1 + (effort * k_param_true))
  } else {
    stop("Model ID: '", model_id, "' is incorrectly specified.")
  }

  # Calculate probabilities of choosing work option
  # (calculates probability for each row of matrix (i.e., trial) based on SV and baseline values)
  prob = exp(SV * beta_param_true) / (exp(SV * beta_param_true) + exp(1 * beta_param_true))

  choices = prob > runif(length(prob))

  data = data.frame(choices, reward, effort, recipient)
  data$k_param_true = k_param_true
  data$beta_param_true = beta_param_true

  return(data)
}
