
# algezebra-benchmark

This module describe benchmark for the algezebra project.

## Run benchmark

* Run sbt command to the top of the algezebra porject

           sbt 

* then localise the sbt project to the noetherBenchmark module

        
        # Be localised on the algezebra-benchmar project.
        sbt:algezebra> project algezebraBenchmark
        
        # Launch all the benchmark..
        sbt:algezebraBenchmark> jmh:run .*
        
        # launch specify benchmark (CalibrationHistogram benchmark)
        sbt:algezebraBenchmark> jmh:run .*CalibrationHistogram.*

### example

        jmh:run -t1 -f1 -wi 2 -i 3 .*Cuckoo.*


give something like : 

```
[info] Running (fork) org.openjdk.jmh.Main -t1 -f1 -wi 2 -i 3 .*Cuckoo.*
[info] # JMH version: 1.19
[info] # VM version: JDK 1.8.0_171, VM 25.171-b11
[info] # VM invoker: /usr/lib/jvm/jdk1.8.0_171/jre/bin/java
[info] # VM options: <none>
[info] # Warmup: 2 iterations, 1 s each
[info] # Measurement: 3 iterations, 1 s each
[info] # Timeout: 10 min per iteration
[info] # Threads: 1 thread, will synchronize iterations
[info] # Benchmark mode: Throughput, ops/time
[info] # Benchmark: com.github.cesarcolle.algezebra.benchmark.CuckooFilterBenchmarkQuery.queryCuckooFilter
[info] # Parameters: (bucketNumber = 100, fingerprintBucket = 10, nbrOfElements = 100)
[info] # Run progress: 0,00% complete, ETA 00:02:00
[info] # Fork: 1 of 1
[info] # Warmup Iteration   1: 2842265,646 ops/s
[info] # Warmup Iteration   2: 96433,529 ops/s
[info] Iteration   1: 703906,116 ops/s
[info] Iteration   2: 4266016,071 ops/s
[info] Iteration   3: 3448687,607 ops/s
...

```


   