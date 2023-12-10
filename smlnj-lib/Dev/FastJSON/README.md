# A faster parser for JSON files

This directory is for the development of a "fast" JSON parser that
consists of a merged lexer and parser with minimal error reporting.


## Evaluation

To evaluate, download the following file

> https://unpkg.com/@mdn/browser-compat-data/data.json

and then use **jq** to make it five times bigger (over 3M lines
and 100Mb of data)

```shell
jq '{ a: ., b: ., c: ., d: . }' data.json > huge.json
```
