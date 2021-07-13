variable "region" {
}

variable "environment" {
  description = "Deployment environment."
}

variable "application" {
  description = "Name of the web application. Used as webapp S3 bucket name, suffixed by the deployment environment name."
  default     = "pde"
}

variable "prefix" {
   default = "gnss-analysis"
}

variable "tf_state_bucket" {
  description = "Name of terraform state S3 bucket."
}

variable "tf_state_table" {
  description = "Name of terraform state lock DDB table."
}
