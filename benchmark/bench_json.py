import json
import time

import numpy as np

s = """
{
    "name": "greenwood",
  "age" : 18,
  "male":true,
  "degrees": [
        "Bachelor",
    "Master",
      "No PhD"
  ]
}
"""

times = 10000
data = []
for _ in range(times):
    start = time.time_ns()
    json.loads(s)
    end = time.time_ns()
    data.append(end - start)

print(np.mean(data))