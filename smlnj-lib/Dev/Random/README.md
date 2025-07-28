Library for general random-number generation.  Consists of engines that
generate random words in a uniform distribution and distributions that control 
the random distribution (this design is similar to the C++ standard library
design).

TODO:
  * "functional" API to random number generation
  * "splittable" PRNGs (aka "splitmix").  See
    - Deterministic Parallel Random-Number Generation for Dynamic-Multithreading
      Platform
      by Leiserson, Schardl, and Sukha, PPoPP 2012
    - Fast Splittable Pseudorandom Number Generators
      by Steele, Lea, and Flood, OOPSLA 2014
    - LXM: Better Splittable Pseudorandom Number Generators
      by Steele and Vigna, OOPSLA 2021

