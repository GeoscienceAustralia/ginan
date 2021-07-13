data "terraform_remote_state" "remote_state" {
  backend = "s3"

  config {
    bucket         = "${var.tf_state_bucket}"
    dynamodb_table = "${var.tf_state_table}"
    key            = "${var.application}/${var.environment}/terraform.tfstate"
  }
}

terraform {
  backend "s3" {}
}

#resource "aws_s3_bucket" "webapp_bucket" {
#  bucket = "${var.prefix}-${var.application}-mybucket-${var.environment}"
#
#}

data "aws_ami" "example" {
 most_recent = true
 owners = ["604917042985"]
 name_regex = "^acs_pea_rhe7_15May2019"

 
}

resource "aws_launch_template" "foobar" {
  name_prefix   = "${var.prefix}-${var.application}-${var.environment}"
  image_id      = "${data.aws_ami.example.image_id}"
  instance_type = "t2.micro"
  name_id       = "${var.prefix}-${var.application}-${var.environment}"
}

resource "aws_autoscaling_group" "bar" {
  availability_zones = ["ap-southeast-2a"]
  desired_capacity   = 1
  max_size           = 1
  min_size           = 1

  launch_template {
    id      = "${aws_launch_template.foobar.id}"
    version = "$Latest"
  }
}
