SatSolverProject
================

How to run the tests?
---------------------

Some of the tests use the z3 solver. If it is not installed, the tests will
just fail. On Ubuntu for instance this is as simple as:
```
sudo apt-get install z3
```

The test can be run by invoking sbt in the SatSolverProject folder:
```
cd SatSolverProject
sbt test
```

