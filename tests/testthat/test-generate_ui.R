testthat::test_that("generate_vpc_ui returns valid ui class for continuous vpc", {

  ui <- generate_VPC_ui(settings = initialize_settings,
                        observed = tidyvpc::obs_data,
                        simulated = tidyvpc::sim_data,
                        vpc.type = "continuous")

  testthat::expect_s3_class(ui, "shiny.tag.list")

})


testthat::test_that("generate_vpc_ui returns valid ui class for categorical vpc", {

  ui <- generate_VPC_ui(settings = initialize_settings,
                        observed = tidyvpc::obs_cat_data,
                        simulated = tidyvpc::sim_cat_data,
                        vpc.type = "categorical")

  testthat::expect_s3_class(ui, "shiny.tag.list")


})
