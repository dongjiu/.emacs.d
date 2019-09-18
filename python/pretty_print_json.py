import json
import sys

file_name = sys.argv[1]
with open(file_name) as fh:
    text = fh.read()
    parsed = json.loads(text) 
    print(json.dumps(parsed, indent=4, sort_keys=True))
