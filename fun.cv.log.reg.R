fun.cv.log.reg <- function(formula, training_data, folds = 10, seed = 8173, workers = 10) {
  set.seed(seed)
  
  # Cross-Validation-Splits erstellen
  cv_folds <- vfold_cv(training_data, v = folds)
  
  # Rezept erstellen
  log_rec <-
    recipe(formula, data = training_data)
  
  # Modellspezifikationen
  log_spec <-
    logistic_reg() |>
    set_engine(
      engine = "glm",
      family = binomial
    ) |>
    set_mode("classification")
  
  # Workflow erstellen
  log_wf <- workflow() |>
    add_recipe(log_rec) |>
    add_model(log_spec)
  
  # Hilfsfunktion zum Extrahieren von Modellen
  get_model <- function(x) {
    extract_fit_parsnip(x) |> tidy()
  }
  
  # Parallele Verarbeitung einrichten
  plan(multisession, workers = workers)
  
  # Cross-Validation ausführen
  set.seed(seed)
  log_res <- log_wf |>
    fit_resamples(
      resamples = cv_folds,
      parallel_over = "resamples",
      metrics = metric_set(roc_auc, accuracy, kap, sens, spec, ppv, npv, f_meas),
      control = control_resamples(
        save_pred = TRUE,
        extract = get_model
      )
    )
  
  return(log_res)
}
