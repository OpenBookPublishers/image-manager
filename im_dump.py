#!/usr/bin/env python3

import sys
import csv
import requests
import json

keys = [
    'acceptability',
    'res_category',
    'placeholder',
    'resolution',
    'format',
    'licence_status',
    'image_name',
    'chapter_name'
]

optional_keys = [
    "provenance",
    "url",
    "orig_artist",
    "orig_year",
    "orig_medium",
    "orig_title",
    "orig_size"
]

computed_keys = [
    "figure",
    "image_location"
]

def run():
    _, base_url = sys.argv
    assert base_url.endswith("/")
    endpoint = base_url + "api/images"
    resp = requests.get(endpoint).json()

    column_ids = keys + computed_keys + optional_keys

    w = csv.writer(sys.stdout)
    w.writerow(column_ids)

    for img in resp:
        optionals = img["optional"]
        for optional_key in optional_keys:
            img[optional_key] = optionals.get(optional_key, "")

        img["image_location"] = base_url + "images/" + img["hash"]
        img["figure"] = "%d.%d" % (int(img["figure_id"]), int(img["rank"]+1))
        row = map(lambda key: img[key], column_ids)
        w.writerow(row)

if __name__ == '__main__':
    run()
