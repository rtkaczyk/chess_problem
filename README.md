Chess Problem
=============

## Compiling

```
$ sbt assembly
```

## Running

#### Recursive version

```
$ ./run recursive < main.in
```

`main.in` is a file specifying a concrete problem. For the main problem stated in the description its contents should be:

```
7 7 2 2 2 0 1
``` 

#### Akka version

If using remote deployment, run the remote (slave) application with:

```
$ ./run remote -Dconfig.file=./conf/remote.conf


```

Then run the master application with:

```
$ ./run parallel -Dconfig.file=./conf/master.conf < main.in
```

`conf` directory contains example configurations. Change the IP addresses and number of actors (crunchers) as necessary. When external configuration file is not provided the default is to deploy 4 actors on local machine only:

```
$ ./run parallel < main.in
```

#### Threads version

This version uses plain Java threads to accomplish the task. Run with:

```
$ ./run threads -Dapp.solvers=3 < main.in
```

`app.solvers` property specifies the number of threads solving the problem. On my machine, best results were achieved using 3 threads.
