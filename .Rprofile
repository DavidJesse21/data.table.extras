# A `use_test` function for the tinytest framework
use_test = function(name, open = TRUE) {
  test_file = fs::path("inst", "tinytest", paste0("test_", name), ext = "R")
  fs::file_create(test_file)
  if (open) fs::file_show(test_file)
}
