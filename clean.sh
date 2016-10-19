#!/bin/bash

find . \( -name "*.cmo" -o -name "*.cmi" -o -name "*.cmx" -o -name "*.o" \) -not -path "./_build*" -exec rm {} \;
