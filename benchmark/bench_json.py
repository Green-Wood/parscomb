import json
import time

import numpy as np

s = """
{
    "productCatalog": [
        {
            "productId": "A101",
            "productName": "Smartphone",
            "price": 699.99,
            "category": "Electronics",
            "availability": "In Stock",
            "rating": 4.5,
            "reviews": [
                {
                    "author": "Alice",
                    "rating": 5,
                    "comment": "Great device!"
                },
                {
                    "author": "Bob",
                    "rating": 4,
                    "comment": "Solid performance."
                }
            ]
        },
        {
            "productId": "B202",
            "productName": "Laptop",
            "price": 1199.99,
            "category": "Electronics",
            "availability": "In Stock",
            "rating": 4.7,
            "reviews": [
                {
                    "author": "Charlie",
                    "rating": 5,
                    "comment": "Amazing laptop!"
                },
                {
                    "author": "David",
                    "rating": 4.5,
                    "comment": "Great value for the price."
                }
            ]
        }
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