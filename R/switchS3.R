#' switchS3
#' @export
switchS3 <- function(cloudEnv = c("dev", "stage", "prod")[1]){
  prior <- ifelse(Sys.getenv("AWS_ACCESS_KEY_ID") == 
                    Sys.getenv("AWS_ACCESS_KEY_ID_DEV"), "dev", 
                  "prod")
  Sys.setenv(R_CONFIG_ACTIVE = "local")
  Sys.setenv(AWS_DEFAULT_REGION = Sys.getenv("AWS_DEFAULT_REGION_DEV"))
  if (cloudEnv == "dev") {
    creds <- jsonlite::fromJSON(credJsonNP.rw)$dev
  }
  if (cloudEnv == "stage") {
    creds <- jsonlite::fromJSON(credJsonNP.rw)$stage
  }
  if (cloudEnv == "prod") {
    creds <- jsonlite::fromJSON(credJsonNP.rw)$prod
  }
  Sys.setenv(AWS_ACCESS_KEY_ID = creds$`aws-access-key-id`)
  Sys.setenv(AWS_SECRET_ACCESS_KEY = creds$`aws-secret-access-key`)
  return(prior)
}

# configList <- getConfig(includeSecrets = TRUE)
# configList$secret$s3_prod
# configList$secret$`aws-local`
# configList$secret$`s3-rw`
# configList$secret$`s3-ro`
# names(configList$secret)

# switchS3 <- function(cloudEnv = c("dev", "stage", "prod")[1]){
#   prior <- ifelse(Sys.getenv("AWS_ACCESS_KEY_ID") ==
#                     Sys.getenv("AWS_ACCESS_KEY_ID_DEV"), "dev",
#                   "prod")
#   Sys.setenv(R_CONFIG_ACTIVE = "local")
#   Sys.setenv(AWS_DEFAULT_REGION = Sys.getenv("AWS_DEFAULT_REGION_DEV"))
#   if (cloudEnv == "dev") {
#     creds <- jsonlite::fromJSON(credJsonNP.rw)$dev
#   }
#   if (cloudEnv == "stage") {
#     creds <- jsonlite::fromJSON(credJsonNP.rw)$stage
#   }
#   if (cloudEnv == "prod") {
#     creds <- jsonlite::fromJSON(credJsonNP.rw)$prod
#   }
#   Sys.setenv(AWS_ACCESS_KEY_ID = creds$`aws-access-key-id`)
#   Sys.setenv(AWS_SECRET_ACCESS_KEY = creds$`aws-secret-access-key`)
#   return(prior)
# }