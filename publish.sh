#!/bin/bash
clj -A:fig:min
scp -r target/public/cljs-out/dev-main.js lex@axw.se:cloud/static/pump/cljs-out/
scp -r resources/public/ lex@axw.se:cloud/static/pump/
