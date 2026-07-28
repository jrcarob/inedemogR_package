test_that("with_ine_cache calls fetch_fun and saves to cache on a first, cold call", {
  cache_dir <- withr::local_tempdir()
  call_count <- 0
  fetch <- function() {
    call_count <<- call_count + 1
    list(value = "fresh")
  }

  result <- with_ine_cache("test_key", use_cache = TRUE, force = FALSE, cache_dir, fetch)

  expect_equal(call_count, 1)
  expect_equal(result$value, "fresh")
  expect_true(file.exists(file.path(cache_dir, "test_key.rds")))
})

test_that("with_ine_cache returns the cached value without calling fetch_fun again", {
  cache_dir <- withr::local_tempdir()
  call_count <- 0
  fetch <- function() {
    call_count <<- call_count + 1
    list(value = paste0("fetch_", call_count))
  }

  first <- with_ine_cache("test_key", use_cache = TRUE, force = FALSE, cache_dir, fetch)
  second <- with_ine_cache("test_key", use_cache = TRUE, force = FALSE, cache_dir, fetch)

  expect_equal(call_count, 1)
  expect_equal(first, second)
  expect_equal(second$value, "fetch_1")
})

test_that("with_ine_cache re-fetches and refreshes the cache when force = TRUE", {
  cache_dir <- withr::local_tempdir()
  call_count <- 0
  fetch <- function() {
    call_count <<- call_count + 1
    list(value = paste0("fetch_", call_count))
  }

  with_ine_cache("test_key", use_cache = TRUE, force = FALSE, cache_dir, fetch)
  refreshed <- with_ine_cache("test_key", use_cache = TRUE, force = TRUE, cache_dir, fetch)

  expect_equal(call_count, 2)
  expect_equal(refreshed$value, "fetch_2")
})

test_that("with_ine_cache always calls fetch_fun and never touches disk when use_cache = FALSE", {
  cache_dir <- withr::local_tempdir()
  call_count <- 0
  fetch <- function() {
    call_count <<- call_count + 1
    list(value = paste0("fetch_", call_count))
  }

  with_ine_cache("test_key", use_cache = FALSE, force = FALSE, cache_dir, fetch)
  with_ine_cache("test_key", use_cache = FALSE, force = FALSE, cache_dir, fetch)

  expect_equal(call_count, 2)
  expect_false(dir.exists(cache_dir) && file.exists(file.path(cache_dir, "test_key.rds")))
})
