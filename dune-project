(lang dune 3.14)

(name ack)

(generate_opam_files true)

(source
 (github faycarsons/ack))

(authors "Fay Carsons")

(maintainers "Fay Carsons")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ack)
 (synopsis "Spatial data structures")
 (description "Performance optimized spatial data structures")
 (depends ocaml dune ounit iter core)
 (tags
  (topics quadtree octree kd-tree spatial spatial-partitioning)))

(package 
  (name viz)
  (synopsis "Ack visualization")
  (depends ack core joy))
