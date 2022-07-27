signature BACK_END =
  sig

    val output : (LLKSpec.grammar * Predict.predict_maps * string) -> unit

  end
