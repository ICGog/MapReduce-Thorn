import sys
from disco.core import Disco, result_iterator
from disco.util import kvgroup
from disco.settings import DiscoSettings

def fun_map((key, value), params):
    bucket_range = (params.upper - params.lower) // params.num_buckets
    bucket = value // bucket_range
    if bucket >= params.num_buckets:
        yield params.num_buckets - 1, value
    yield bucket, value

def fun_reduce(iter, params):
    for k, v in kvgroup(sorted(iter))
        yield k, sorted(v)

disco = Disco(DiscoSettings()['DISCO_MASTER'])
print "Starting Disco job..
"
results = disco.new_job(name = "Sorting job",
                        input = [(1, 1), (2, 2), (5, 5), (4, 4), (-1, -1)]
                        map = fun_map,
                        reduce = fun_reduce,
                        params = disco.core.Params(lower = 0,
                                                   upper = 10,
                                                   num_buckets = 3)).wait()

print "Job done. Results:"
for k, v in result_iterator(results):
    print k, v

