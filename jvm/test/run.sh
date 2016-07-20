#!/bin/sh

../../min-caml $1; jasmin $1.s; java Caml
