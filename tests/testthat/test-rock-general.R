testthat::context("general ROCK tests")

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("reading a source with no ROCK stuff works properly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_source(file.path(examplePath,
                                    "lorum-ipsum.rock"));

  testthat::expect_s3_class(testres, "rockParsedSource");

});

###-----------------------------------------------------------------------------

testthat::test_that("an inductive code tree is read correctly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_source(file.path(examplePath,
                                    "longer-test.rock"),
                          silent=TRUE);

  testthat::expect_equal(testres$inductiveCodeTrees$codes$inductFather$inducChild3$label,
                         "inducChild3");

});

###-----------------------------------------------------------------------------

testthat::test_that("a code tree is printed correctly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_source(file.path(examplePath,
                                    "longer-test.rock"),
                          silent=TRUE);

  testthat::expect_output(print(testres),
                          "This source contained inductive coding trees.");

  testthat::expect_output(print(testres),
                          "This source contained deductive coding trees.");

});
###-----------------------------------------------------------------------------

testthat::test_that("a single deductive code tree is read correctly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_source(file.path(examplePath,
                                "second-test-file.rock"));

  testthat::expect_equal(testres$deductiveCodeTrees$parentCode1$someParent$childCode1$label,
                         "childCode1");

});

###-----------------------------------------------------------------------------

testthat::test_that("Multiple sources are read correctly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_sources(examplePath,
                           extension="rock",
                           silent=TRUE);

  testthat::expect_equal(testres$deductiveCodeTrees$children$parentCode1$someParent$childCode2$label,
                         "childCode2");

});

###-----------------------------------------------------------------------------

testthat::test_that("multiple sources without deductive code trees are read correctly (i.e. as NA)", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_sources(examplePath,
                           regex = "ipsum");

  testthat::expect_true(identical(testres$deductiveCodeTrees, NA));

});

###-----------------------------------------------------------------------------

testthat::test_that("A deductive code tree is read correctly from multiple DCT files", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_sources(examplePath,
                           extension="dct",
                           silent=TRUE);

  testthat::expect_equal(testres$deductiveCodeTrees$behavior_xl67k7w8j$intention_71vr5q3q$attitude_71vqm37n$label,
                         "Attitude");

});

###-----------------------------------------------------------------------------

testthat::test_that("Sources are exported to html properly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_sources(examplePath,
                           extension="rock",
                           silent=TRUE);

  testres <- export_to_html(testres);

  testthat::expect_true(grepl('<span class="code codes">[[grandchildCode2]]</span>',
                              testres[["example-1.rock"]],
                              fixed=TRUE));

});


###-----------------------------------------------------------------------------

testthat::test_that("Coded fragments are collected properly", {

  examplePath <- file.path(system.file(package="rock"), 'extdata');

  testres <- parse_sources(examplePath,
                           extension="rock",
                           silent=TRUE);

  testres <- collect_coded_fragments(testres);

  testthat::expect_true(grepl('\n### Topic2 *(path: codes>Topic2)*\n\n-----\n\n\n\n**Source: `longer-test.rock`**\n\n<div class=\"utterance\">It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. <span class=\"code codes\">[[Topic2]]</span>',
                              testres,
                              fixed=TRUE));

});

