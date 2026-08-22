library(tidyverse)

set.seed(42)

n_samples <- 100  # total lesions
n_bands_raw <- 1347  # number of raw spectral bands
wavelengths_raw <- seq(300, 900, length.out = n_bands_raw)

generate_raw_spectrum <- function(label) {
  center <- ifelse(label == "benign", 600, 580)
  base <- exp(-((wavelengths_raw - center)^2) / (2 * 1000))
  noise <- rnorm(n_bands_raw, 0, 0.05)
  spectrum <- base + noise
  return(spectrum)
}

labels <- rep(c("malignant", "benign"), each = n_samples / 2)
spectra_raw <- t(sapply(labels, generate_raw_spectrum))

df_raw <- as_tibble(spectra_raw)
df_raw$label <- labels

library(signal)

# Gaussian smoothing kernel
smooth_spectrum <- function(spectrum) {
  filt <- rep(1/5, 5)  # simple moving average filter
  stats::filter(x = spectrum, filter = filt, sides = 2, circular = FALSE)
}


# Normalize (area under curve = 1)
normalize_spectrum <- function(spectrum) {
  spectrum / sum(spectrum)
}

# Downsample to 47 bands
downsample_spectrum <- function(spectrum, wavelengths_raw, target_bands = 47) {
  wavelengths_down <- seq(min(wavelengths_raw), max(wavelengths_raw), length.out = target_bands)
  approx(wavelengths_raw, spectrum, xout = wavelengths_down)$y
}

spectra_processed <- spectra_raw %>%
  apply(1, function(spectrum) {
    s <- smooth_spectrum(spectrum)
    s[is.na(s)] <- mean(s, na.rm = TRUE)  # handle edge NAs
    s <- normalize_spectrum(s)
    downsample_spectrum(s, wavelengths_raw)
  }) %>%
  t()


colnames(spectra_processed) <- paste0("wl_", round(seq(360, 820, length.out = 47)), "nm")
df_processed <- as_tibble(spectra_processed) %>%
  mutate(label = labels)


library(torch)

# Prepare data for torch
x <- as.matrix(df_processed %>% select(-label))
y <- as.numeric(df_processed$label == "malignant") + 1  # gives 1 and 2


x_tensor <- torch_tensor(x, dtype = torch_float())
y_tensor <- torch_tensor(y, dtype = torch_long())

# Create dataset
dataset <- tensor_dataset(x_tensor$unsqueeze(2), y_tensor)
dataloader <- dataloader(dataset, batch_size = 16, shuffle = TRUE)

# Define CNN model
model <- nn_module(
  "CNNClassifier",
  initialize = function() {
    self$conv1 <- nn_conv1d(1, 16, kernel_size = 3, padding = 1)
    self$pool <- nn_max_pool1d(kernel_size = 2)
    self$conv2 <- nn_conv1d(16, 32, kernel_size = 3, padding = 1)
    
    # Dummy input to calculate output shape
    dummy_input <- torch_randn(c(1, 1, 47))
    output_shape <- dummy_input %>%
      self$conv1() %>%
      nnf_relu() %>%
      self$pool() %>%
      self$conv2() %>%
      nnf_relu() %>%
      self$pool() %>%
      torch_flatten(start_dim = 2) %>%
      dim()
    
    self$flattened_size <- output_shape[2]
    
    self$fc1 <- nn_linear(self$flattened_size, 64)
    self$fc2 <- nn_linear(64, 2)
  },
  
  forward = function(x) {
    x %>%
      self$conv1() %>%
      nnf_relu() %>%
      self$pool() %>%
      self$conv2() %>%
      nnf_relu() %>%
      self$pool() %>%
      torch_flatten(start_dim = 2) %>%
      self$fc1() %>%
      nnf_relu() %>%
      self$fc2()
  }
)


net <- model()
optimizer <- optim_adam(net$parameters, lr = 0.001)
loss_fn <- nn_cross_entropy_loss()

# Training loop
library(coro)

for (epoch in 1:20) {
  net$train()
  total_loss <- 0
  coro::loop(for (batch in dataloader) {
    optimizer$zero_grad()
    output <- net(batch[[1]])
    loss <- loss_fn(output, batch[[2]])
    loss$backward()
    optimizer$step()
    total_loss <- total_loss + loss$item()
  })
  cat(sprintf("Epoch %d - Loss: %.4f\n", epoch, total_loss))
}

