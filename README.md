# ldn-reader

## TODO

- [ ] Exception Handling
- [ ] Fix space leak
- [ ] Fix some exception using debug mode

## Benchmark

### rough

- OSX Yosemite
- 2.7GHz Intel Core i7
- 16GB Memory

```
server: ldn-reader +RTS -N
    (after finished the first crawling)
benchmark: ab -n 10000 -c 100 "http://127.0.0.1:3000/top"

server configuration:
    CrawlerDelaySec: 300
    Environment: Production
    CrawlerEnabled: True
    ArticleMaxNum: 20
```

#### 06/16 01:04 5385441a683e85f4286461dd9cc2e0a3937157d4
```
Server Software:        Warp/3.0.13.1
Server Hostname:        127.0.0.1
Server Port:            3000

Document Path:          /top
Document Length:        51205 bytes

Concurrency Level:      100
Time taken for tests:   34.513 seconds
Complete requests:      10000
Failed requests:        0
Total transferred:      513240000 bytes
HTML transferred:       512050000 bytes
Requests per second:    289.75 [#/sec] (mean)
Time per request:       345.127 [ms] (mean)
Time per request:       3.451 [ms] (mean, across all concurrent requests)
Transfer rate:          14522.51 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0   36  49.8      7     222
Processing:    18  307 218.0    216    1534
Waiting:       18  256 216.7    171    1533
Total:         19  343 219.6    261    1534

Percentage of the requests served within a certain time (ms)
  50%    261
  66%    336
  75%    461
  80%    516
  90%    635
  95%    709
  98%    903
  99%   1431
 100%   1534 (longest request)
```
