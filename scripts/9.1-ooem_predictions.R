


testSet_load_values = c(
  rbind(
    testSet$Loads.0,
    testSet$Loads.1,
    testSet$Loads.2,
    testSet$Loads.3,
    testSet$Loads.4,
    testSet$Loads.5,
    testSet$Loads.6,
    testSet$Loads.7,
    testSet$Loads.8,
    testSet$Loads.9,
    testSet$Loads.10,
    testSet$Loads.11,
    testSet$Loads.12,
    testSet$Loads.13,
    testSet$Loads.14,
    testSet$Loads.15,
    testSet$Loads.16,
    testSet$Loads.17,
    testSet$Loads.18,
    testSet$Loads.19,
    testSet$Loads.20,
    testSet$Loads.21,
    testSet$Loads.22,
    testSet$Loads.23
  )
)


mape.ooem = 100 * mean(abs((testSet_load_values - ooem_predictions[["ooem_predictions"]])/testSet_load_values))