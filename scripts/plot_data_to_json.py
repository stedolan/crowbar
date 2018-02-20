#!/usr/bin/env python

import json, sys

if len(sys.argv) != 2:
    print("usage: %s afl-output/plot_data")
    sys.exit(1)

fd = open(sys.argv[1])
header = fd.readline()

assert header.startswith("# ")
fieldnames = [x.strip() for x in header[2:].strip().split(",")]

assert "execs_per_sec" in fieldnames
assert "map_size" in fieldnames

def parse(n):
    if n.endswith("%"): n = n[:-1]
    return float(n) if "." in n else int(n)

MAP_SIZE = 64 * 1024
lines = []
for line in fd:
    fields = [x.strip() for x in line.strip().split(",")]
    assert len(fields) == len(fieldnames)
    d = {k: parse(v) for k, v in zip(fieldnames, fields)}
    d['bits'] = int(round(d['map_size'] * 0.01 * MAP_SIZE))
    d['tests'] = lines[-1]['tests'] + int(round((d['unix_time'] - lines[-1]['unix_time']) * d['execs_per_sec'])) if lines else 1
    lines.append(d)

print(json.dumps({"engine": "afl-fuzz", "log": lines}, indent=2))
