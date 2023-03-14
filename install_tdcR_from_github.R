library(devtools)
options(download.file.method = "wininet")
# requires personal access token
# https://github.com/settings/tokens/new
install_github("mogd001/tdcR", auth_token = "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", force = TRUE)
