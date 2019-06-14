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



# clean_transcript(input=file.path(workingPath, "test", "P.I.int.txt"),
#                  outputFile=file.path(workingPath, "test", "P.I.int.rock"),
#                  extraReplacements=list(c("\\n-\\s", "\n---turn-of-talk---\n")));


