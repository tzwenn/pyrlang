# README #

### What is this repository for? ###

* Prlang is an Erlang BEAM bytecode interpreter written in RPython. It implements approximately 25% of BEAM instructions. It can support integer calculations (but not bigint), closures, exception handling, some operators to atom, list and tuple, user modules, and multi-process in single core. Pyrlang is still in development.

### How do I get set up? ###

* rpython -Ojit targettest.py
* the test demos (*.beam) can be found in test_beam directory.
