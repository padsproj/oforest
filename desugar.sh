#!/bin/bash

ppxdir=$(ocamlfind query forest)/../../bin
ocamlfind ppx_tools/rewriter $ppxdir/ppx_pads $1|
ocamlfind ppx_tools/rewriter $ppxdir/ppx_forest 
