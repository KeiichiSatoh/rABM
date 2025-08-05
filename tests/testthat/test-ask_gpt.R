# test_that("ask_gpt executes without error with dummy config", {
#  skip_on_cran()
#  skip_if_not(Sys.getenv("OPENAI_API_KEY") != "", "API key not set")
#
#  result <- ask_gpt(
#    question = "What does init_agents do?",
#    gpt = "openai",
#    overwrite_rag = FALSE,
#    gpt_args = list(api_key = Sys.getenv("OPENAI_API_KEY")),
#    embed_args = list(model = "text-embedding-3-small", api_key = Sys.getenv("OPENAI_API_KEY"))
#  )
#
#  expect_s3_class(result, "data.frame")
#  expect_true(all(c("question", "answer") %in% colnames(result)))
# })
