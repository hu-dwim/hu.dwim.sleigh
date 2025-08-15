# hu.dwim.sleigh

## What

An experiment to parse
[Sleigh](https://ghidra.re/ghidra_docs/languages/html/sleigh.html) (a
CPU instruction DSL), with the ultimate goal of generating an
assembler for x86_64.

## Status

**It's a pre-alpha experiment.**

Preprocessing works, and I was making good progress with the second,
parsing pass when then I realized that LLVM's TableGen can emit a json
file that is easy to parse and probably also contains a more detailed
description of the CPU ISA's, so I'm switching my attention back to
[hu.dwim.tablegen-to-assembler](https://github.com/hu-dwim/hu.dwim.tablegen-to-assembler).

## Why

My ultimate goal is an x86_64 assembler for
[Maru](https://github.com/attila-lendvai/maru).

But it's easier to work in a mature language with Slime, so I'm hoping
to finish this CL lib and then either:

  - port this to Maru (ideal outcome),

  - or simply generate the assembler for Maru and check it in into its
    repo (a less pleasing but more pragmatic shortcut).

## Who

Written by [attila@lendvai.name](mailto:attila@lendvai.name).

## Where

The primary communication channel is the facilities on
[the project's GitHub page](https://github.com/hu-dwim/hu.dwim.sleigh).
