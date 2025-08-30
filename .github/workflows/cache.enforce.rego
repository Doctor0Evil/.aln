package bithub.cache

default ok = false

ok {
  # require at least one cache path and one artifact mapping per matrix entry
  some i
  m := input[".bit/build.art.create.yml"].build.matrix[i]
  count(m.cache_paths) > 0
  count(m.artifacts) > 0
}

deny[msg] {
  not ok
  msg := "Bit.Hub: build.art.create must define cache_paths and artifacts for every entry"
}
